

test_that("All R scripts run without error", {
  script_files <- list.files("Code", pattern = "\\.R$", full.names = TRUE)
  for (f in script_files) {
    expect_no_error(source(f, echo = TRUE, max.deparse.length = Inf), NA)
  }
})
