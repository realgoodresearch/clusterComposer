test_that("section worker preserves a below-maximum section", {
  aoi_path <- system.file("extdata", "sectionA.gpkg", package = "clusterComposer")
  bf_path <- system.file("extdata", "bfsA.gpkg", package = "clusterComposer")
  aoi <- sf::st_read(aoi_path, quiet = TRUE)[1, ]
  aoi$section_id <- 1L
  output_dir <- tempfile("clustercomposer-worker-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  clusterComposer:::.cluster_section(
    section = 1L,
    aoi = aoi,
    bf_file_path = bf_path,
    run_id = "worker-test",
    pop_per_EZ = 1000000,
    max_pop_per_EZ = 2000000,
    min_pop_per_EZ = 1,
    initial_buffer_dist_max = 2.5,
    initial_buffer_dist_min = 2.5 / 16,
    d = 1000,
    epsg = 3857,
    temp_folder = output_dir
  )

  result_path <- file.path(output_dir, "worker-test_1.gpkg")
  expect_true(file.exists(result_path))
  result <- sf::st_read(result_path, quiet = TRUE)
  buildings <- sf::st_read(
    bf_path,
    wkt_filter = sf::st_as_text(sf::st_geometry(aoi)),
    quiet = TRUE
  )

  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1L)
  expect_equal(result$section_ID, 1L)
  expect_equal(result$bldCount, nrow(buildings))
  expect_equal(result$pop, sum(buildings$pop_building))
  expect_true(all(sf::st_is_valid(result)))
})

test_that("shared-border filtering handles an empty neighbour set", {
  polygon <- sf::st_polygon(list(rbind(
    c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
  )))
  polygons <- sf::st_sf(
    pop_sum = 1,
    geometry = sf::st_sfc(polygon, crs = 3857)
  )

  neighbours <- clusterComposer:::.shared_border_neighbours(
    polygons,
    source_index = 1L,
    neighbour_indices = integer()
  )

  expect_type(neighbours, "integer")
  expect_length(neighbours, 0L)
})

test_that("section worker handles one building set above the population maximum", {
  output_dir <- tempfile("clustercomposer-one-set-")
  dir.create(output_dir)
  on.exit(unlink(output_dir, recursive = TRUE), add = TRUE)

  aoi <- sf::st_sf(
    section_id = 1L,
    geometry = sf::st_sfc(
      sf::st_polygon(list(rbind(
        c(0, 0), c(20, 0), c(20, 20), c(0, 20), c(0, 0)
      ))),
      crs = 3857
    )
  )
  buildings <- sf::st_sf(
    pop_building = c(67, 67, 67),
    geometry = sf::st_buffer(
      sf::st_sfc(
        sf::st_point(c(9.9, 10)),
        sf::st_point(c(10, 10)),
        sf::st_point(c(10.1, 10)),
        crs = 3857
      ),
      0.5
    )
  )
  building_path <- file.path(output_dir, "one-building.gpkg")
  sf::st_write(buildings, building_path, quiet = TRUE)

  suppressWarnings(clusterComposer:::.cluster_section(
    section = 1L,
    aoi = aoi,
    bf_file_path = building_path,
    run_id = "one-set",
    pop_per_EZ = 175,
    max_pop_per_EZ = 200,
    min_pop_per_EZ = 150,
    initial_buffer_dist_max = 2.5,
    initial_buffer_dist_min = 2.5 / 16,
    d = 1000,
    epsg = 3857,
    temp_folder = output_dir
  ))

  result_path <- file.path(output_dir, "one-set_1.gpkg")
  expect_true(file.exists(result_path))
  result <- sf::st_read(result_path, quiet = TRUE)
  expect_equal(sum(result$pop), 201)
  expect_true(all(sf::st_is_valid(result)))
})
