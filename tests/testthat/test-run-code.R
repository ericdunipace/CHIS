

test_that("All R scripts run without error", {
  testthat::expect_no_error({
    r_folder <- here::here("R")
    script_files <- list.files(r_folder, pattern = "\\.R$", full.names = TRUE)
    for (f in script_files) {
      source(f, echo = TRUE, max.deparse.length = Inf)
    }
  })
})
