

test_that("All installed packages meet version requirements", {
  pkg   <- readxl::read_xlsx(here::here("docs","dac-r-and-stata-packages-list-2.xlsx"),
                             sheet = 4L,
                             col_type = c("text","text"))
  names <- pkg$Package
  ver   <- pkg$Version
  
  rver <- base::getRversion()
  testthat::skip_if(rver != "4.2.3", "Skipping DAC tests because R version is not 4.2.3 as required for DAC")
  
  testthat::expect_no_error(pkg)
  lockfile      <- renv::lockfile_read(here::here("renv.lock"))
  package_names <- names(lockfile$Packages)
  package_ver   <- sapply(lockfile$Packages, function(x) x$Version)
  
  final_names   <- names[names %in% package_names]
  final_ver     <- ver[names %in% package_names]
  
  if("askpass" %in% final_names) final_ver[final_names == "askpass"]   <- "1.1"
  if("gridExtra" %in% final_names) final_ver[final_names == "gridExtra"] <- "2.3"
  if("rstudioapi" %in% final_names) final_ver[final_names == "rstudioapi"] <- "0.14"

  compatibility_upgrades <- c(
    "bslib",
    "cachem",
    "commonmark",
    "fastmap",
    "fs",
    "gdtools",
    "gtable",
    "highr",
    "htmltools",
    "Rcpp",
    "sass",
    "scales",
    "V8",
    "xml2"
  )
  
  for( i in seq_along(final_names)) {
    n <- final_names[i]
    v <- final_ver[i]

    found <- package_ver[which(package_names == n)]

    if (n %in% compatibility_upgrades) {
      testthat::expect_true(
        utils::compareVersion(found, v) >= 0,
        info = paste(
          "Package",
          n,
          "must be at least DAC version",
          v,
          "but found",
          found
        )
      )
    } else {
      testthat::expect_equivalent(
        found,
        v,
        info = paste(
          "Package",
          n,
          "version mismatch: expected",
          v,
          "but found",
          found
        )
      )
    }
  }
})
