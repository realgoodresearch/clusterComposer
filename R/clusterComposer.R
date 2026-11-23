utils::globalVariables(c(
  ".", ".N", ":=", "ID", "cid",
  "group_id", "overlap_group", "pop", "pop_building"
))

#' Generate Bespoke Spatial Sampling Frames from Buildings
#'
#' @param aoi Character string. Area of Interest split into sections with one single polygon per section.
#' @param bf_file_path Character string. File pathway to the building footprints geopackage file (.gpgk). This file should contain geometries of each building along with their population count in a column named "pop_building".
#' @param run_id Character string. Text to be added to output file names for easy identification.
#' @param output_file_path Character string. File pathway to the folder outputs should be saved.
#' @param pop_per_EZ Numeric, single number. Default is 250 people. Ideal population size of clusters (Enumeration Zones).
#' @param max_pop_per_EZ Numeric, single number. Default is 350 people. Maximum population size of clusters (Enumeration Zones).
#' @param min_pop_per_EZ Numeric, single number. Default is 150 people. Minimum population size of clusters (Enumeration Zones).
#' @param initial_buffer_dist_max Numeric, single number. Default is 2.5 metres. Maximum buffering around buildings to identify initial collections of buildings in close proximity. If a large distance is specified in densely populated areas, there will be pointless rejecting of collections that are too large and therefore pointless computing.
#' @param initial_buffer_dist_min Numeric, single number. Default is 2.5/16 metres (i.e. 0.15625 metres). Minimum buffering around buildings to identify initial collections of buildings in close proximity. Must equal `initial_buffer_dist_max` divided by a power of two, for example 2, 4, 8, or 16.
#' @param d Numeric, single number. Default is 1000 metres. Maximum distance allowed in clustering collections of buildings.
#' @param epsg Numeric, single number. Default is 3857. EPSG code for the projected coordinate reference system used for spatial calculations.
#' @param n_cores Numeric, single number. Default is 1. The number of computer cores you would like for processing. Recommend using your total number of cores minus 1. Use parallel::detectCores() to find the total number of cores available on your computer.
#' @param delete_temp_files Logical. Default is TRUE. If `TRUE`, intermediate files are deleted when the function exits. If `FALSE`, they are retained.
#' @import dplyr
#' @importFrom data.table as.data.table
#' @import rgeoda
#' @import stars
#' @import sf
#' @importFrom stats aggregate
#' @importFrom utils stack
#' @export
clusterComposer <- function(
                aoi,
                bf_file_path,
                run_id,
                output_file_path,
                pop_per_EZ=250,
                max_pop_per_EZ=350,
                min_pop_per_EZ=150,
                initial_buffer_dist_max=2.5,
                initial_buffer_dist_min=2.5/16,
                d=1000,
                epsg=3857,
                n_cores=1,
                delete_temp_files=TRUE
){

  # check number of cores specified by user does not exceed those available
  available_cores <- parallel::detectCores()
  if(n_cores > available_cores){
    message("Requested ", n_cores, " cores, but only ", available_cores, " are available. Using ", available_cores, " cores.")
    n_cores <- available_cores
  }

  # check pop values are suitable
  if(min_pop_per_EZ>=pop_per_EZ){
    stop("`min_pop_per_EZ` must be less than `pop_per_EZ`.", call. = FALSE)
  }
  if(max_pop_per_EZ<=pop_per_EZ){
    stop("`max_pop_per_EZ` must be greater than `pop_per_EZ`.", call. = FALSE)
  }

  # create column in aoi of IDs
  aoi$section_id <- 1:nrow(aoi)
  # ensure name of geometry col is "geometry"
  names(aoi)[names(aoi) == attr(aoi, "sf_column")] <- "geometry"
  attr(aoi, "sf_column") <- "geometry"

  # set up a temporary folder to store intermediary section files
  temp_folder <- file.path(output_file_path, paste0(run_id, "_temp"))
  dir.create(temp_folder, showWarnings = FALSE)
  if (isTRUE(delete_temp_files)){
    on.exit(unlink(temp_folder, recursive = TRUE, force = TRUE), add = TRUE)
  }

  # set up parallel processing
  cl <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  parallel::clusterEvalQ(cl, {
    library(sf)
    library(stars)
    library(dendextend)
    library(dplyr)
    library(data.table)
    library(rgeoda)
    library(igraph)
  })
  parallel::clusterExport(
    cl,
    varlist = c(
      "aoi",
      "bf_file_path",
      "run_id",
      "output_file_path",
      "pop_per_EZ",
      "max_pop_per_EZ",
      "min_pop_per_EZ",
      "initial_buffer_dist_max",
      "initial_buffer_dist_min",
      "d",
      "epsg",
      "temp_folder"
    ),envir = environment()
  )
  invisible(parallel::parLapplyLB(cl,aoi$section_id,function(section) {

    # read in subset bfs within aoi extent
    aoi_section <-  aoi[aoi$section_id==section,]
    bf <- sf::st_read(paste0(bf_file_path),wkt_filter = st_as_text(st_geometry(aoi_section)))

    # if no bfs in section or sum of pop across all bfs is less than max pop per EZ, save aoi as EZ
    if(nrow(bf) == 0 | sum(bf$pop_building) <= max_pop_per_EZ) {
      final_polys <- st_sf(ID = integer(), area = numeric(), bldCount = integer(), pop = numeric(), geometry = st_sfc(crs = st_crs(aoi)))
      geom <- st_geometry(aoi[aoi$section_id == section, ])
      if(nrow(bf) == 0){
        final_polys <- rbind(final_polys, st_sf(ID = 1L, bldCount = 0L, pop = 0, area = as.numeric(st_area(geom)), section_ID = section, geometry = geom))
      }else{ # i.e. if sum(bf$pop_building) <= max_pop_per_EZ
        final_polys <- rbind(final_polys, st_sf(ID = 1L, bldCount = nrow(bf), pop = sum(bf$pop_building), area = as.numeric(st_area(geom)), section_ID = section, geometry = geom))
      }
      final_polys_path <- file.path(temp_folder,paste0(run_id, "_", section, ".gpkg"))
      st_write(final_polys, final_polys_path, append = FALSE)
      return()
    }

    # save crs & transform data
    original_crs <- st_crs(aoi_section)
    aoi_section <- st_transform(aoi_section,epsg)
    bf <- st_transform(bf,epsg)
    # building centroids
    bf_cent <- sf::st_centroid(bf)

    ### STEP 1: buffer buildings and merge areas of overlap to create collections of buildings.
    ### buffer size is such that the total population size of a collection does not exceed
    ### max_pop_per_EZ. Buffer size differs across the collections of buildings.
    ### Note that individual buildings may exceed this population limit and so there will
    ### be instances here where a “collection” is greater than the limit

    # buffer buildings function
    buffer_to_max_pop <- function(bf,pop_limit,buffer_start,min_buffer){
      results <- list()
      id_counter <- 0
      recurse_cluster <- function(buildings, buff){
        # buffer and dissolve
        grp <- buildings |>
          st_buffer(buff) |>
          st_union() |>
          st_make_valid() |>
          st_cast("POLYGON") |>
          st_as_sf()
        # assign unique IDs across all recursion levels
        n <- nrow(grp)
        grp$group_id <- seq(id_counter + 1, length.out = n)
        id_counter <<- id_counter + n   # update global group counter so that we don't get same ID used in diff iterations
        # assign buildings to dissolved areas
        building_groups <- st_join(st_centroid(buildings),grp,join = st_within)
        # sum building populations & join to grp object
        pops <- building_groups |>
          st_drop_geometry() |>
          group_by(group_id) |>
          summarise(pop = sum(pop_building, na.rm = TRUE))
        grp <- left_join(grp, pops, by = "group_id")
        grp <- grp[!is.na(grp$pop),]
        # loop through each group
        # if pop below pop limit stick with this buffer size
        # if pop too high repeat function with small buffer size
        for(i in seq_len(nrow(grp))){
          g <- grp[i,]
          members <- buildings[building_groups$group_id == g$group_id,]
          if(g$pop <= pop_limit ||
             buff <= min_buffer ||
             nrow(members) <= 1){
            results[[length(results)+1]] <<- g
          } else {
            recurse_cluster(members, buff / 2)
          }
        }
      }
      recurse_cluster(bf, buffer_start)
      do.call(rbind, results)
    }

    # apply buffer buildings function
    bld_sets <- buffer_to_max_pop(
      bf = bf,
      pop_limit = min_pop_per_EZ,
      buffer_start = initial_buffer_dist_max,
      min_buffer = initial_buffer_dist_min
    )

    # some buildings don't get assigned to group in cases where the pop would be too high if they were added to a group
    # so identify those unassigned and give each their own group ID
    assigned <- st_join(st_centroid(bf),bld_sets,join = st_within)
    unassigned <- bf[is.na(assigned$group_id), ]
    if(nrow(unassigned) > 0){
      # match geometry col name with that of bld_sets
      names(unassigned)[names(unassigned) == attr(unassigned, "sf_column")] <- "x"
      attr(unassigned, "sf_column") <- "x"
      # create group IDs, add pop value for bld as group pop
      unassigned$group_id <- seq(max(bld_sets$group_id) + 1,length.out = nrow(unassigned))
      unassigned$pop <- unassigned$pop_building
      # finally add unassigned blds to bld_sets
      bld_sets <- rbind(bld_sets, unassigned[, names(bld_sets)])
    }
    rm(assigned,unassigned)

    ### STEP 2: create min area rectangles around building collections
    ### and dissolve rectangles with significant overlap

    # min area rectangles
    outer_rectangle <- bld_sets |> group_by(group_id) |> summarise() |> st_minimum_rotated_rectangle()
    # negative buffer rectangles and identify pairs with overlap,i.e. only pairs with sig overlap
    # need to transform here because S2 does not support negative geodetic buffers
    outer_rectangle_buff <- st_buffer(outer_rectangle, -10)
    # neg buffer makes small polygons disappear
    # add the unbuffered polygons of these back in before consider overlaps
    disappeared <- st_is_empty(outer_rectangle_buff)
    st_geometry(outer_rectangle_buff)[disappeared] <- st_geometry(outer_rectangle)[disappeared]
    # find overlapping polygons
    adj <- st_intersects(outer_rectangle_buff)
    g <- igraph::graph_from_adj_list(adj, mode = "all")
    membership <- igraph::components(g, mode = "weak")$membership
    overlap_groups <- split(seq_len(nrow(outer_rectangle_buff)), membership)
    # dissolve the original rectangles based on their overlap when negatively buffered
    # these become the new bld_sets polygons
    bld_sets <- do.call(rbind, lapply(seq_along(overlap_groups), function(i){
      rows <- overlap_groups[[i]]
      st_sf(overlap_group = i, geometry = st_union(st_geometry(outer_rectangle[rows, ])))
    }))
    rm(outer_rectangle,outer_rectangle_buff,disappeared,adj,g,membership)
    # for any (small) overlaps between new dissolved polygons, use smaller polygon of a pair to mask area of bigger one
    poly_area <- as.numeric(st_area(bld_sets))
    area_order <- order(poly_area)
    adjusted_geometry <- st_geometry(bld_sets)
    for(k in seq_along(area_order)){
      i <- area_order[k]
      smaller <- area_order[seq_len(k - 1)]
      if(length(smaller) > 0){
        overlapping_smaller <- smaller[lengths(st_intersects(bld_sets[i, ], bld_sets[smaller, ])) > 0]
        if(length(overlapping_smaller) > 0) adjusted_geometry[i] <- st_difference(adjusted_geometry[i], st_union(adjusted_geometry[overlapping_smaller]))
      }
    }
    # update bld_sets with new geometries
    st_geometry(bld_sets) <- adjusted_geometry
    bld_sets <- bld_sets[!st_is_empty(bld_sets), ]
    bld_sets <- st_make_valid(bld_sets)
    rm(poly_area,area_order,adjusted_geometry,i,k,smaller,overlapping_smaller)
    # calculate pop for each polygon
    matches <- st_within(st_centroid(bf), bld_sets)
    bf$overlap_group <- vapply(matches, function(x) if(length(x)) bld_sets$overlap_group[x[1]] else NA_integer_, integer(1))
    pop_sums <- bf |> st_drop_geometry() |> filter(!is.na(overlap_group)) |> group_by(overlap_group) |> summarise(pop = sum(pop_building, na.rm = TRUE), .groups = "drop")
    bld_sets <- left_join(bld_sets, pop_sums, by = "overlap_group")
    bld_sets$pop[is.na(bld_sets$pop)] <- 0
    rm(matches,pop_sums)
    # update col names
    names(bld_sets)[1] <- "group_id"
    names(bld_sets)[names(bld_sets) == attr(bld_sets, "sf_column")] <- "x"
    attr(bld_sets, "sf_column") <- "x"


    ### STEP 3: apply hierarchical clustering to combine building collections to meet target population

    # distance to neighbours

    dist_mat <- st_distance(bld_sets)
    dst <- stats::as.dist(dist_mat)
    clust <- stats::hclust(dst, method = 'ward.D2')
    rm(dist_mat,dst)
    # complete full clustering based on max distance first
    clust1 <- dendextend::cutree(clust, h = d)
    ptid <- 1:nrow(bld_sets)
    bld_sets <- cbind(bld_sets, clust1, ptid)
    colnames(bld_sets)[which(colnames(bld_sets) == 'clust1')] <- 'cid'
    bld_sets_dt <- data.table::as.data.table(bld_sets)
    clustat <- merge(bld_sets_dt[,.(at_sum = sum(pop)), cid], bld_sets_dt[,.N, cid], by = 'cid')
    # identify clusters that are lower than the pop threshold
    valid_clusts_first <- clustat$cid[clustat$at_sum <= max_pop_per_EZ]
    final_clustpts_list <- lapply(valid_clusts_first, function(y){bld_sets_dt$ptid[bld_sets_dt$cid == y]})
    # get subsets of the tree for clusters that have too high pop
    ktemp <- nrow(clustat)
    dend <- stats::as.dendrogram(clust)
    dend_list <- dendextend::get_subdendrograms(dend, k = ktemp)
    rm(clust1,ptid,clust,valid_clusts_first,dend, ktemp)
    # get_subdendrograms gives order that doesn't correspond to cluster IDs given in clust1
    # so overwrite cid to be the element in dend_list so we can link back to original building collections
    dend_members <- lapply(dend_list, labels)
    names(dend_members) <- seq_along(dend_members)
    df <- stack(dend_members)
    names(df) <- c("rowid", "cid")
    df <- df[order(df$rowid), ]
    df$cid <- as.integer(df$cid)
    bld_sets_dt$cid <- df$cid
    clustat <- merge(bld_sets_dt[,.(at_sum = sum(pop)), cid], bld_sets_dt[,.N, cid], by = 'cid')
    big_clusts <- clustat$cid[clustat$at_sum > max_pop_per_EZ]
    rm(dend_members,df,clustat)
    # loop through each of the large clusters identifying the levels in different branches of the tree that gives lower than the pop threshold
    for(big_clust in big_clusts){
      clustpts_list <- list()
      kval <- 2
      kmax <- attributes(dend_list[[big_clust]])$members
      while(!kval == 0){
        clustnew <- dendextend::cutree(dend_list[[big_clust]], k = kval)
        bld_sets_dt_sub <- bld_sets_dt[bld_sets_dt$ptid %in% as.numeric(names(clustnew))]
        bld_sets_dt_sub <- cbind(bld_sets_dt_sub, clustnew)
        clustnewat <- merge(bld_sets_dt_sub[,.(at_sum = sum(pop)), clustnew], bld_sets_dt_sub[,.N, clustnew], by = 'clustnew')
        rm(clustnew)
        if(length(which(clustnewat$at_sum > max_pop_per_EZ)) == length(clustnewat$at_sum)){
          kval <- kval + 1
        }else{
          valid_clusts <- which(clustnewat$at_sum <= max_pop_per_EZ)
          for(clid in valid_clusts){
            if(!bld_sets_dt_sub$ptid[bld_sets_dt_sub$clustnew == clid][1] %in% unlist(clustpts_list)){
              nil <- length(clustpts_list) + 1
              clustpts_list[[nil]] <- bld_sets_dt_sub$ptid[bld_sets_dt_sub$clustnew == clid]
            }
          }
          rm(valid_clusts)
          if(length(which(clustnewat$at_sum > max_pop_per_EZ)) == 0){
            kval <- 0
          }else{
            kval <- kval + 1
          }
        }
        rm(clustnewat)
        if(kval > kmax) break
      }
      # if there were any original bld collections with a pop > maxattrib we want to add them sep
      if(any(!bld_sets_dt_sub$ptid %in% unlist(clustpts_list))){
        restricted_clusts <- bld_sets_dt_sub$ptid[!bld_sets_dt_sub$ptid %in% unlist(clustpts_list)]
        for(restricted_clust in restricted_clusts){
          nil <- length(clustpts_list) + 1
          clustpts_list[[nil]] <- restricted_clust
        }
        rm(restricted_clusts)
      }
      rm(bld_sets_dt_sub, kval, kmax)
      final_clustpts_list <- c(final_clustpts_list, clustpts_list)
      rm(clustpts_list)
    }
    rm(list = intersect(c("big_clust", "clid", "nil", "restricted_clust"), ls()))
    cid_dt <- data.table::data.table(ptid = unlist(final_clustpts_list),
                                     cid = unlist(lapply(1:length(lengths(final_clustpts_list)), function(clusterid){
                                       rep(clusterid, length(final_clustpts_list[[clusterid]]))
                                     }))
    )
    # add new cluster IDs based on hierarchical clustering to the building collection polygons
    bld_sets_dt[, cid := NULL]
    bld_sets_dt <- merge(bld_sets_dt, cid_dt, by = 'ptid')
    bld_sets_temp <- st_as_sf(bld_sets_dt, sf_column_name = "x")
    cluster_pops <- aggregate(pop ~ cid, data = bld_sets_temp, FUN = sum, na.rm = TRUE)
    bld_sets_temp <- merge(bld_sets_temp, cluster_pops, "cid")
    rm(big_clusts, dend_list,final_clustpts_list,cid_dt,bld_sets_dt,cluster_pops)
    # unionise polygons for each cid
    hclusters <- bld_sets_temp |>
      group_by(cid) |>
      summarise(do_union = TRUE, .groups = "drop")
    rm(bld_sets_temp)
    hclusters <- st_make_valid(hclusters)

    # buffer slightly to fill any divits
    hclusters <- st_buffer(hclusters, 0.5)
    hclusters <- st_make_valid(hclusters)
    hclusters <- st_simplify(hclusters, dTolerance = 5)
    hclusters <- hclusters[!st_is_empty(hclusters), ] # in case a tiny cluster formed and then geometry disappears when simplifying

    ### STEP 4: allocate small gaps between clusters to a neighbouring cluster

    # first for multipolygon clusters, fill space between parts
    # this allows maintenance of group assignment through future steps
    # start with smallest part
    for(i in seq_len(nrow(hclusters))){
      if(st_geometry_type(hclusters)[i] != "MULTIPOLYGON") next
      geom <- suppressWarnings(st_make_valid(st_geometry(hclusters)[i]))
      repeat{
        parts <- suppressWarnings(st_cast(geom, "POLYGON"))
        if(length(parts) <= 1) break
        smallest <- which.min(as.numeric(st_area(parts)))
        source_part <- parts[smallest]
        other_parts <- parts[-smallest]
        target_part <- other_parts[which.min(as.numeric(st_distance(source_part, other_parts)))]
        coords <- st_coordinates(source_part)
        coords <- coords[coords[, "L1"] == 1, c("X", "Y"), drop = FALSE]
        coords <- unique(coords[-nrow(coords), , drop = FALSE])
        vertices <- st_sfc(lapply(seq_len(nrow(coords)), function(j) st_point(coords[j, ])), crs = st_crs(hclusters))
        connections <- st_nearest_points(vertices, rep(st_boundary(target_part), length(vertices)), pairwise = TRUE)
        connections <- connections[as.numeric(st_length(connections)) > 0]
        if(length(connections) == 0) break
        connections <- st_sfc(lapply(connections, function(line){
          xy <- st_coordinates(line)[, c("X", "Y"), drop = FALSE]
          if(nrow(xy) < 2 || anyNA(xy)) return(line)
          direction <- xy[nrow(xy), ] - xy[1, ]
          line_length <- sqrt(sum(direction^2))
          if(!is.finite(line_length) || line_length == 0) return(line)
          xy[nrow(xy), ] <- xy[nrow(xy), ] + direction / line_length
          st_linestring(xy)
        }), crs = st_crs(hclusters))
        new_geom <- suppressWarnings(st_make_valid(st_union(c(parts, st_buffer(connections, 0.02, endCapStyle = "FLAT")))))
        polygon_parts <- suppressWarnings(st_cast(st_collection_extract(new_geom, "POLYGON"), "POLYGON"))
        polygon_parts <- st_sfc(lapply(polygon_parts, function(p) st_polygon(list(p[[1]]))), crs = st_crs(hclusters))
        new_geom <- st_union(polygon_parts)
        if(length(st_cast(new_geom, "POLYGON", warn = FALSE)) >= length(parts)) break
        geom <- new_geom
      }
      st_geometry(hclusters)[i] <- geom
    }
    rm(list = intersect(c("i", "geom", "parts", "smallest", "source_part", "other_parts", "target_part", "coords", "vertices", "connections", "new_geom", "polygon_parts"), ls()))
    # clusters are now single polygons
    # create complete minimum area rectangles around clusters
    hclusters_rectangles_full <- hclusters
    st_geometry(hclusters_rectangles_full) <- st_minimum_rotated_rectangle(st_geometry(hclusters))
    # mask each complete rectangle using all other pre-rectangle cluster polygons
    masked_rectangles <- vector("list", nrow(hclusters))
    for(i in seq_len(nrow(hclusters))){
      other_polygons <- st_union(st_geometry(hclusters[-i, ]))
      masked <- st_difference(st_geometry(hclusters_rectangles_full[i, ]), other_polygons)
      masked <- st_buffer(st_buffer(masked, -10, joinStyle = "MITRE", mitreLimit = 100), 10, joinStyle = "MITRE", mitreLimit = 100)
      if(length(masked)==0){ # occasionally a rectangle is completely covered and therefore masked by cluster polygons. Here, we create an empty entry
        masked_rectangles[[i]] <- NULL
      }else{
        masked_rectangles[[i]] <- st_make_valid(masked)[[1]]
      }
      rm(masked,other_polygons)
    }
    hclusters_adj_rectangles <- hclusters
    st_geometry(hclusters_adj_rectangles) <- st_sfc(masked_rectangles, crs = st_crs(hclusters))
    hclusters_adj_rectangles <- hclusters_adj_rectangles[!st_is_empty(hclusters_adj_rectangles), ]
    rm(masked_rectangles)
    # create polygons in empty spaces outside clusters
    # here we create separate objects containing extension lines and then add these to the original set of lines
    # extend corners of exposed rectangle sides until another rectangle or section boundary is reached
    aoi_boundary <- st_boundary(aoi_section)
    corner_extensions <- list()
    for(i in seq_len(nrow(hclusters_rectangles_full))){
      coords <- st_coordinates(st_boundary(hclusters_rectangles_full[i, ]))[, c("X", "Y"), drop = FALSE]
      centre <- st_coordinates(st_centroid(hclusters_rectangles_full[i, ]))[1, c("X", "Y")]
      other_rectangles <- hclusters_rectangles_full[-i, ]
      other_boundaries <- if(nrow(other_rectangles) > 0) st_boundary(st_union(other_rectangles)) else NULL
      for(j in seq_len(nrow(coords) - 1)){
        a <- coords[j, ]
        b <- coords[j + 1, ]
        midpoint <- (a + b) / 2
        direction <- b - a
        side_length <- sqrt(sum(direction^2))
        if(!is.finite(side_length) || side_length == 0) next
        normal <- c(-direction[2], direction[1]) / side_length
        if(sum(normal * (midpoint - centre)) < 0) normal <- -normal
        for(corner in list(a, b)){
          corner_point <- st_sfc(st_point(corner), crs = st_crs(hclusters_rectangles_full))
          corner_overlaps <- nrow(other_rectangles) > 0 && any(lengths(st_intersects(corner_point, other_rectangles)) > 0)
          targets <- if(corner_overlaps || is.null(other_boundaries)) st_geometry(aoi_boundary) else st_union(c(other_boundaries, st_geometry(aoi_boundary)))
          ray <- st_sfc(st_linestring(rbind(corner, corner + normal * 100)), crs = st_crs(hclusters_rectangles_full))
          hits <- suppressWarnings(st_intersection(ray, targets))
          if(length(hits) == 0) next
          hits <- suppressWarnings(st_cast(hits, "POINT"))
          hit_coords <- st_coordinates(hits)[, c("X", "Y"), drop = FALSE]
          distances <- sqrt(rowSums((hit_coords - matrix(corner, nrow(hit_coords), 2, byrow = TRUE))^2))
          valid <- which(distances > 0.01)
          if(length(valid) == 0) next
          nearest <- valid[which.min(distances[valid])]
          hit <- hit_coords[nearest, ]
          unit_direction <- (hit - corner) / distances[nearest]
          corner_extensions[[length(corner_extensions) + 1]] <- st_linestring(rbind(corner - unit_direction * 2, hit + unit_direction * 2))
        }
      }
    }
    corner_extensions <- st_sfc(corner_extensions, crs = st_crs(hclusters_rectangles_full))
    rm(i, coords, centre, other_rectangles, other_boundaries, j, a, b, midpoint, direction, side_length, normal, corner, corner_point, corner_overlaps, targets, ray, hits, hit_coords, distances, valid, nearest, hit, unit_direction)
    # extend all lines close to section edge to create small empty polygons that can be merged to correct polygon
    # convert all boundaries to LINESTRING
    b <- st_boundary(hclusters_adj_rectangles)$x
    b <- st_union(b)
    bsimp <- st_simplify(b, dTolerance = 5, preserveTopology = TRUE)
    bsimp <- st_union(c(bsimp, st_boundary(aoi_section$geometry)))
    #bsimp <- st_snap(bsimp,aoi_section,1) # st_snap only works in some places - dont need this now as draw lots of lines between gaps instead
    lines <- st_cast(bsimp, "LINESTRING")
    lines <- st_make_valid(lines)
    lines <- st_collection_extract(lines, "LINESTRING")
    # create extensions to corners of hclusters_adj_rectangles if they reach section boundary within a short dist
    original_lines <- lines
    extended_lines <- list()
    for(i in seq_along(original_lines)){
      coords <- st_coordinates(original_lines[i])[, c("X", "Y"), drop = FALSE]
      other_lines <- original_lines[-i]
      create_extension <- function(endpoint, neighbour){
        direction <- endpoint - neighbour
        line_length <- sqrt(sum(direction^2))
        if(!is.finite(line_length) || line_length == 0) return(NULL)
        ray <- st_sfc(st_linestring(rbind(endpoint, endpoint + direction / line_length * 100)), crs = st_crs(lines))
        boundary_hits <- suppressWarnings(st_intersection(ray, aoi_boundary))
        if(length(boundary_hits) == 0) return(NULL)
        boundary_hits <- suppressWarnings(st_cast(boundary_hits, "POINT"))
        hit_coords <- st_coordinates(boundary_hits)[, c("X", "Y"), drop = FALSE]
        hit_distances <- sqrt(rowSums((hit_coords - matrix(endpoint, nrow(hit_coords), 2, byrow = TRUE))^2))
        boundary_point <- hit_coords[which.min(hit_distances), ]
        extension <- st_sfc(st_linestring(rbind(endpoint, boundary_point)), crs = st_crs(lines))
        endpoint_buffer <- st_buffer(st_sfc(st_point(endpoint), crs = st_crs(lines)), 0.01)
        extension_check <- st_difference(extension, endpoint_buffer)
        if(length(other_lines) > 0 && any(lengths(st_intersects(extension_check, other_lines)) > 0)) return(NULL)
        if(any(lengths(st_intersects(extension_check, hclusters_adj_rectangles)) > 0)) return(NULL)
        extension[[1]]
      }
      extension_start <- create_extension(coords[1, ], coords[2, ])
      extension_end <- create_extension(coords[nrow(coords), ], coords[nrow(coords) - 1, ])
      if(!is.null(extension_start)) extended_lines[[length(extended_lines) + 1]] <- extension_start
      if(!is.null(extension_end)) extended_lines[[length(extended_lines) + 1]] <- extension_end
    }
    extended_lines <- st_sfc(extended_lines, crs = st_crs(lines))
    # add the corner_extensions and extended_lines made in last two code chunks to lines object
    lines <- c(lines, extended_lines, corner_extensions)
    # omit lines outside of section boundary. Use buffer because using exact or very small buffer leads to corners being cut off and incorrect polys form
    lines <- st_intersection(lines,st_buffer(st_geometry(aoi_section),5))
    # polygonize all lines created
    polygons_sf <- lines |>
      st_zm(drop = TRUE) |>
      st_snap(lines, tolerance = 2) |>
      st_union() |>
      st_node() |>
      st_polygonize() |>
      st_collection_extract("POLYGON") |>
      st_cast("POLYGON") |>
      st_as_sf()
    polygons_sf <- polygons_sf[!st_is_empty(polygons_sf), ]
    # calculate population per polygon
    polygons_sf$ID <- 1:nrow(polygons_sf)
    polygons_sf <- polygons_sf %>%
      st_join(bf_cent) %>%
      group_by(ID) %>%   # polygon ID column
      summarise(pop_sum = sum(pop_building, na.rm = TRUE))
    # calculate polygon areas
    polygons_sf$area <- st_area(polygons_sf)
    # add empty spaces between clusters, i.e. no/low pop polygons
    # these are added to the neighbour with the longest shared boundary
    # very large spaces are not processed here
    polys <- polygons_sf
    skip_ids <- integer()
    repeat {
      small <- polys[polys$pop_sum < 1 & as.numeric(polys$area) < 50000 & !(polys$ID %in% skip_ids),]
      if (nrow(small) == 0) break
      i <- 1 # process first element in small as small is updates in each iteration/loop
      nbr_idx <- st_touches(small[i, ], polys)[[1]]
      if (length(nbr_idx) == 0) {
        skip_ids <- c(skip_ids, small$ID[i])
        next
      }
      shared_len <- numeric(length(nbr_idx))
      for (k in seq_along(nbr_idx)) {
        j <- nbr_idx[k]
        shared <- st_intersection(st_boundary(small[i, ]),st_boundary(polys[j, ]))
        shared_len[k] <- sum(as.numeric(st_length(shared)))
      }
      target <- nbr_idx[which.max(shared_len)]
      merged <- st_union(polys$x[target], small$x[i])
      merged <- st_union(st_collection_extract(merged, "POLYGON"))
      polys$x[target] <- merged # update polys item with merged polygon
      polys$pop_sum[target] <- polys$pop_sum[target] + small$pop_sum[i]
      #polys$building_count[target] <- polys$building_count[target] + small$building_count[i]
      polys <- polys[polys$ID != small$ID[i], ]
    }

    ### STEP 5: combine any clusters below min pop per EZ to a suitable neighbouring cluster

    # want to add lowest pop ones first to give them best chance in being added
    polys <- polys[order(polys$pop_sum),]
    # polygons eligible for processing
    to_process <- which(polys$pop_sum < min_pop_per_EZ & polys$pop_sum > 0)
    polys$processed <- TRUE
    polys$processed[to_process] <- FALSE
    # loop through iteratively adding
    # if no neighbours, then polygon remains the same
    # only consider merges resulting in pop < 350
    # from the possible valid merges, prioritise merges that result in compactness value >0.5
    # from those, then merge with the one with lowest pop
    while(length(to_process) > 0) {
      i <- to_process[1]
      nbr_idx <- st_intersects(polys[i, ], polys)[[1]]
      nbr_idx <- setdiff(nbr_idx, i)
      # only keep neighbours with a shared border, i.e. don't keep if just touching at a point
      nbr_idx <- nbr_idx[sapply(nbr_idx, function(j) {
        shared <- st_intersection(
          st_boundary(st_make_valid(st_geometry(polys[i, ]))),
          st_boundary(st_make_valid(st_geometry(polys[j, ])))
        )
        sum(as.numeric(st_length(shared))) > 0
      })]
      # only keep neighbours with non-0 pop (after previous clean up of small area non-0 polygons, there should only be large non-0 pop areas now)
      nbr_idx <- nbr_idx[polys$pop_sum[nbr_idx] > 0]
      # if no neighbours, take i off the list to process
      if(length(nbr_idx) == 0) {
        polys$processed[i] <- TRUE
        to_process <- which(polys$pop_sum < min_pop_per_EZ & polys$pop_sum > 0 & !polys$processed)
        next
      }
      # if neighbours exist consider pop and compactness of possible merges
      if(length(nbr_idx) > 0) {
        compact_vals <- numeric(length(nbr_idx))
        merged_pops  <- numeric(length(nbr_idx))
        for(k in seq_along(nbr_idx)) {
          j <- nbr_idx[k]
          merged <- st_union(polys$x[j], polys$x[i])
          merged <- st_union(st_collection_extract(merged, "POLYGON"))
          a <- st_area(merged)
          p <- st_perimeter(merged)
          compact_vals[k] <- (4 * pi * as.numeric(a)) / (as.numeric(p)^2)
          merged_pops[k] <- polys$pop_sum[j] + polys$pop_sum[i]
        }
        # only consider merges resulting in pop < 350, unless engulfed
        engulfed <- "FALSE"
        if(length(nbr_idx)==1){
          shared <- st_intersection(
            st_boundary(st_make_valid(st_geometry(polys[i, ]))),
            st_boundary(st_make_valid(st_geometry(polys[nbr_idx, ])))
          )
          if(sum(as.numeric(st_length(shared))) == sum(as.numeric(st_length(st_boundary(st_make_valid(st_geometry(polys[i, ]))))))){
            engulfed <- "TRUE"
          }
        }
        if(length(nbr_idx)==1 & engulfed == "TRUE"){
          target <- nbr_idx
        }else{
          valid <- which(merged_pops < max_pop_per_EZ)
          if(length(valid) == 0) {
            polys$processed[i] <- TRUE
            to_process <- which(polys$pop_sum < min_pop_per_EZ & polys$pop_sum > 0 & !polys$processed)
            next  # don't merge
          }else{
            nbr_valid <- nbr_idx[valid]
            compact_valid <- compact_vals[valid]
            pop_valid <- merged_pops[valid]
            candidates <- which(compact_valid > 0.5)
            if(length(candidates) > 0) {
              target <- nbr_valid[candidates[which.min(pop_valid[candidates])]]
            }else{
              target <- nbr_valid[which.max(compact_valid)]
            }
          }
        }
      }
      # merge i into target
      merged <- st_union(polys$x[target], polys$x[i])
      merged <- st_union(st_collection_extract(merged, "POLYGON"))
      polys$x[target] <- merged
      polys$pop_sum[target] <- polys$pop_sum[target] + polys$pop_sum[i]
      # remove merged polygon
      polys <- polys[-i, ]
      # rebuild processing list
      to_process <- which(polys$pop_sum < min_pop_per_EZ & polys$pop_sum > 0 & !polys$processed)
    }

    ### STEP 6: clean up boundaries

    # clean up little triangular protrusions that arise from adding small spaces between initial cluster boundaries
    # function to remove these
    remove_shallow_triangles <- function(lines, max_depth = 2, max_apex_angle = 70, digits = 4){
      make_key <- function(a, b, c){
        forward <- paste(round(c(a, b, c), digits), collapse = "_")
        reverse <- paste(round(c(c, b, a), digits), collapse = "_")
        min(forward, reverse)
      }
      coords_list <- lapply(st_geometry(lines), function(x) st_coordinates(x)[, c("X", "Y"), drop = FALSE])
      repeat{
        feature_keys <- lapply(coords_list, function(coords){
          if(nrow(coords) < 3) return(character(0))
          unique(vapply(2:(nrow(coords) - 1), function(i) make_key(coords[i - 1, ], coords[i, ], coords[i + 1, ]), character(1)))
        })
        key_counts <- table(unlist(feature_keys))
        remove_list <- lapply(coords_list, function(coords){
          remove_vertex <- rep(FALSE, nrow(coords))
          if(nrow(coords) < 3) return(remove_vertex)
          for(i in 2:(nrow(coords) - 1)){
            a <- coords[i - 1, ]
            b <- coords[i, ]
            c <- coords[i + 1, ]
            key <- make_key(a, b, c)
            if(is.na(key_counts[key]) || key_counts[key] != 2) next
            base_length <- sqrt(sum((c - a)^2))
            if(!is.finite(base_length) || base_length == 0) next
            triangle_area <- abs((a[1] * (b[2] - c[2]) + b[1] * (c[2] - a[2]) + c[1] * (a[2] - b[2])) / 2)
            depth <- 2 * triangle_area / base_length
            v1 <- a - b
            v2 <- c - b
            denominator <- sqrt(sum(v1^2)) * sqrt(sum(v2^2))
            if(!is.finite(denominator) || denominator == 0) next
            apex_angle <- acos(max(-1, min(1, sum(v1 * v2) / denominator))) * 180 / pi
            if(depth <= max_depth && apex_angle <= max_apex_angle) remove_vertex[i] <- TRUE
          }
          remove_vertex
        })
        if(!any(unlist(remove_list))) break
        coords_list <- Map(function(coords, remove_vertex) coords[!remove_vertex, , drop = FALSE], coords_list, remove_list)
      }
      st_geometry(lines) <- st_sfc(lapply(coords_list, st_linestring), crs = st_crs(lines))
      lines
    }
    # prep polygons and apply function
    lines <- polys |> st_boundary() |> st_cast("LINESTRING") |> st_as_sf()
    lines_no_triangles <- remove_shallow_triangles(lines, max_depth = 20, max_apex_angle = 70)
    polys <- lines_no_triangles |>
      st_geometry() |>
      st_union() |>
      st_node() |>
      st_polygonize() |>
      st_collection_extract("POLYGON") |>
      st_cast("POLYGON") |>
      st_as_sf()
    polys <- polys[!st_is_empty(polys), ]
    # amend any polygons lines to go around buildings
    bf_hits <- st_intersects(bf, polys)
    crossing_buildings <- which(lengths(bf_hits) > 1)
    # >1 here gives blds that intersect more than one poly
    # Note that blds intersecting section boundary here only show up in one (within section) polygon
    # We don't want to amend section polygons as these will often be official admin units
    for(i in crossing_buildings){
      footprint <- st_geometry(bf[i, ])
      owner <- st_intersects(st_centroid(footprint), polys)[[1]][1]
      if(length(owner) == 0) next
      affected <- setdiff(bf_hits[[i]], owner)
      # update the polygon the footprint centroid falls inside
      st_geometry(polys)[owner] <- st_union(st_geometry(polys)[owner], footprint)
      # update other polygons the footprint falls inside
      if(length(affected) > 0){
        for(j in affected) st_geometry(polys)[j] <- st_difference(st_geometry(polys)[j], footprint)
      }
    }
    # straighten out wiggly lines that don't impact footprint inclusion
    straighten_shared_lines <- function(lines, buildings, min_angle = 110, digits = 3){
      make_key <- function(a, b, c){
        forward <- paste(round(c(a, b, c), digits), collapse = "_")
        reverse <- paste(round(c(c, b, a), digits), collapse = "_")
        min(forward, reverse)
      }
      coords_list <- lapply(st_geometry(lines), function(x) st_coordinates(x)[, c("X", "Y"), drop = FALSE])
      repeat{
        occurrences <- list()
        k <- 0
        for(f in seq_along(coords_list)){
          coords <- coords_list[[f]]
          if(nrow(coords) < 3) next
          for(i in 2:(nrow(coords) - 1)){
            k <- k + 1
            occurrences[[k]] <- data.frame(feature = f, vertex = i, key = make_key(coords[i - 1, ], coords[i, ], coords[i + 1, ]))
          }
        }
        occurrences <- do.call(rbind, occurrences)
        if(is.null(occurrences)) break
        key_counts <- table(occurrences$key)
        changed <- FALSE
        for(key in names(key_counts[key_counts == 2])){
          rows <- occurrences[occurrences$key == key, ]
          if(length(unique(rows$feature)) != 2) next
          f <- rows$feature[1]
          i <- rows$vertex[1]
          coords <- coords_list[[f]]
          a <- coords[i - 1, ]
          b <- coords[i, ]
          c <- coords[i + 1, ]
          v1 <- a - b
          v2 <- c - b
          denominator <- sqrt(sum(v1^2)) * sqrt(sum(v2^2))
          if(!is.finite(denominator) || denominator == 0) next
          angle <- acos(max(-1, min(1, sum(v1 * v2) / denominator))) * 180 / pi
          if(angle <= min_angle) next
          replacement <- st_sfc(st_linestring(rbind(a, c)), crs = st_crs(lines))
          if(any(lengths(st_intersects(replacement, buildings)) > 0)) next
          for(r in seq_len(nrow(rows))){
            f_remove <- rows$feature[r]
            i_remove <- rows$vertex[r]
            coords_list[[f_remove]] <- coords_list[[f_remove]][-i_remove, , drop = FALSE]
          }
          changed <- TRUE
          break
        }
        if(!changed) break
      }
      st_geometry(lines) <- st_sfc(lapply(coords_list, st_linestring), crs = st_crs(lines))
      lines
    }
    lines <- polys |> st_boundary() |> st_cast("LINESTRING") |> st_as_sf()
    lines_straightened <- straighten_shared_lines(lines, buildings = bf, min_angle = 130) # this angle matters - doesn't run if this straighten leads to polygon overlapping itself
    polys <- lines_straightened |>
      st_geometry() |>
      st_union() |>
      st_node() |>
      st_polygonize() |>
      st_collection_extract("POLYGON") |>
      st_cast("POLYGON") |>
      st_as_sf()
    polys <- polys[!st_is_empty(polys), ]
    # clean up final polygons
    #final_polys <- polys[,c("ID","x")]
    final_polys <- st_transform(polys, original_crs)
    final_polys <- st_make_valid(final_polys)
    # trim final_polys to include only area inside section - sometimes the intersection can result in tiny dots or lines and give an error so only trim if st_intersection is clean
    if(nrow(final_polys)==length(st_intersection(st_geometry(final_polys),st_geometry(st_transform(aoi_section, original_crs))))){
      final_polys <- st_intersection(final_polys, st_geometry(st_transform(aoi_section,original_crs)))
    }
    final_polys <- final_polys[!st_is_empty(final_polys), ]
    # previous line is necessary to cut off any protrusions
    # but unfortunately it can turn a polygon into poly+lines so need to clean this up
    for(bad_geom in which(st_geometry_type(final_polys)=="GEOMETRYCOLLECTION")){
      g <- st_collection_extract(st_make_valid(st_geometry(final_polys)[bad_geom]),"POLYGON")
      if (length(g) > 0) st_union(g)
      final_polys$x[bad_geom] <- g
    }
    final_polys <- st_make_valid(final_polys)
    final_polys$ID <- 1:nrow(final_polys)
    # recalculate final EZ area, count of blds & pop
    hits <- st_intersects(final_polys,st_transform(bf_cent, original_crs))
    final_polys$bldCount <- lengths(hits)
    final_polys$pop <- vapply(hits, function(i) sum(bf_cent$pop_building[i], na.rm = TRUE), numeric(1))
    final_polys$area <- st_area(final_polys)
    final_polys$ID <- 1:nrow(final_polys)
    final_polys$section_ID <- section

    # save section clusters
    final_polys_path <- file.path(temp_folder,paste0(run_id, "_", section, ".gpkg"))
    st_write(final_polys, final_polys_path, append = FALSE)

  }, chunk.size = 1L))

  temp_files <- list.files(temp_folder,pattern = "\\.gpkg$",full.names = TRUE)
  cluster_polys <- lapply(temp_files, sf::st_read, quiet = TRUE)
  cluster_polys <- do.call(rbind, cluster_polys)
  cluster_polys$ID <- 1:nrow(cluster_polys)
  cluster_polys_path <- file.path(output_file_path,paste0(run_id, "_clusters.gpkg"))
  st_write(cluster_polys, cluster_polys_path, append = FALSE)

}
