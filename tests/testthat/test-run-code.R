

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
  testthat::expect_no_error({
    r_folder <- here::here("R")
    script_files <- list.files(r_folder, pattern = "\\.R$", full.names = TRUE)
    for (f in script_files) {
      source(f, echo = TRUE, max.deparse.length = Inf)
    }
  })
})