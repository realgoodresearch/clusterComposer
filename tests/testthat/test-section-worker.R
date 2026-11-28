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
