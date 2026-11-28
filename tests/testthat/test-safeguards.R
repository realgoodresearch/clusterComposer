make_test_inputs <- function(directory, populations = c(100, 100, 100)) {
  polygons <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(10, 0), c(10, 10), c(0, 10), c(0, 0)))),
    sf::st_polygon(list(rbind(c(10, 0), c(20, 0), c(20, 10), c(10, 10), c(10, 0)))),
    crs = 3857
  )
  aoi <- sf::st_sf(name = c("west", "east"), geometry = polygons)
  buildings <- sf::st_sf(
    pop_building = populations,
    geometry = sf::st_buffer(
      sf::st_sfc(
        sf::st_point(c(2, 2)),
        sf::st_point(c(12, 2)),
        sf::st_point(c(15, 5)),
        crs = 3857
      ),
      0.1
    )
  )
  building_path <- file.path(directory, "buildings.gpkg")
  sf::st_write(buildings, building_path, quiet = TRUE)
  list(aoi = aoi, building_path = building_path)
}

test_that("preflight records actual section workload", {
  directory <- tempfile("clustercomposer-preflight-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  inputs <- make_test_inputs(directory)
  inputs$aoi$section_id <- seq_len(nrow(inputs$aoi))

  status <- clusterComposer:::.preflight_sections(
    inputs$aoi,
    inputs$building_path,
    "preflight",
    directory
  )

  expect_equal(status$feature_count, c(1L, 2L))
  expect_equal(status$population, c(100, 200))
  expect_true(all(status$vertex_count > 0))
  expect_equal(status$status, rep("pending", 2))
})

test_that("oversized sections can be skipped and successful sections merged", {
  directory <- tempfile("clustercomposer-partial-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  inputs <- make_test_inputs(directory)

  result_path <- clusterComposer(
    aoi = inputs$aoi,
    bf_file_path = inputs$building_path,
    run_id = "partial",
    output_file_path = directory,
    max_features_per_section = 1,
    merge_partial = TRUE,
    delete_temp_files = FALSE
  )

  status <- utils::read.csv(file.path(directory, "partial_section_status.csv"))
  result <- sf::st_read(result_path, quiet = TRUE)
  expect_equal(status$status, c("completed", "skipped"))
  expect_equal(status$included_in_merge, c(TRUE, FALSE))
  expect_equal(unique(result$section_ID), 1L)
  expect_true(file.exists(file.path(directory, "partial_manifest.csv")))
})

test_that("resume reuses a valid section output", {
  directory <- tempfile("clustercomposer-resume-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  inputs <- make_test_inputs(directory)
  aoi <- inputs$aoi[1, ]

  clusterComposer(
    aoi = aoi,
    bf_file_path = inputs$building_path,
    run_id = "resume",
    output_file_path = directory,
    delete_temp_files = FALSE
  )
  temporary_output <- file.path(directory, "resume_temp", "resume_1.gpkg")
  modified_before <- file.info(temporary_output)$mtime

  clusterComposer(
    aoi = aoi,
    bf_file_path = inputs$building_path,
    run_id = "resume",
    output_file_path = directory,
    resume = TRUE,
    delete_temp_files = FALSE
  )
  status <- utils::read.csv(file.path(directory, "resume_section_status.csv"))

  expect_true(status$resumed)
  expect_equal(status$status, "completed")
  expect_equal(file.info(temporary_output)$mtime, modified_before)
})

test_that("a section timeout is recorded without losing diagnostics", {
  directory <- tempfile("clustercomposer-timeout-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  inputs <- make_test_inputs(directory)

  expect_error(
    clusterComposer(
      aoi = inputs$aoi[1, ],
      bf_file_path = inputs$building_path,
      run_id = "timeout",
      output_file_path = directory,
      section_timeout = 0,
      delete_temp_files = FALSE
    ),
    "did not complete"
  )
  status <- utils::read.csv(file.path(directory, "timeout_section_status.csv"))
  expect_equal(status$status, "timeout")
  expect_true(grepl("exceeded timeout", status$error_message))
})

test_that("a child-process error is recorded and other R work can continue", {
  directory <- tempfile("clustercomposer-error-")
  dir.create(directory)
  on.exit(unlink(directory, recursive = TRUE), add = TRUE)
  inputs <- make_test_inputs(directory, populations = c(1000, 100, 100))

  expect_error(
    clusterComposer(
      aoi = inputs$aoi[1, ],
      bf_file_path = inputs$building_path,
      run_id = "error",
      output_file_path = directory,
      epsg = 999999,
      delete_temp_files = FALSE
    ),
    "did not complete"
  )
  status <- utils::read.csv(file.path(directory, "error_section_status.csv"))
  expect_equal(status$status, "error")
  expect_true(nzchar(status$error_message))
  expect_equal(1 + 1, 2)
})
