

test_that("All local R scripts run without error", {
  rver <- base::getRversion()
  testthat::skip_if(rver == "4.2.3", "Skipping local tests for R version 4.2.3 for DAC")
  testthat::expect_no_error({
    r_folder <- here::here("R","localCode")
    script_files <- list.files(r_folder, pattern = "\\.R$", full.names = TRUE)
    for (f in script_files) {
      source(f, echo = TRUE, max.deparse.length = Inf)
    }
  })
})


test_that("All DAC R scripts run without error", {
  rver <- base::getRversion()
  testthat::skip_if(rver != "4.2.3", "Skipping DAC tests because R version is not 4.2.3 as required for DAC")
  # known warnings from the DAC scripts; any other warning fails the test
  known_warnings <- c(
    # survey::svyvar internals on R 4.2.3
    "Recycling array of length 1 in vector-array arithmetic is deprecated",
    # geom_sf: counties with no climate anxiety value
    "rows containing missing values",
    "st_centroid assumes attributes are constant over geometries",
    # build_table1 relevels "Gender" while the variable is still "Sex"
    "unknown level in `f`: Gender"
  )
  testthat::expect_no_error({
    r_folder <- here::here("R")
    script_files <- list.files(r_folder, pattern = "\\.R$", full.names = TRUE)
    testthat::expect_warning(
      for (f in script_files) {
        source(f, echo = TRUE, max.deparse.length = Inf)
      },
      paste(known_warnings, collapse = "|"),
      all = TRUE
    )
  })
})