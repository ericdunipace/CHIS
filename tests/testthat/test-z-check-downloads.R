

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
  
  for( i in seq_along(final_names)) {
    n <- final_names[i]
    v <- final_ver[i]
    testthat::expect_equivalent(package_ver[which(package_names == n)], v,
                             info = paste("Package", n, "version mismatch: expected", v, "but found", package_ver[which(package_names == n)]))
  }
  
  # inst_packages <- installed.packages()
  # 
  # package_names <- inst_packages[,"Package"]
  # package_ver   <- inst_packages[,"Version"]
  # 
  # final_names   <- names[names %in% package_names]
  # final_ver     <- ver[names %in% package_names]
  # 
  # if("askpass" %in% final_names) final_ver[final_names == "askpass"]   <- "1.1"
  # if("gridExtra" %in% final_names) final_ver[final_names == "gridExtra"] <- "2.3"
  # 
  # for( i in seq_along(final_names)) {
  #   n <- final_names[i]
  #   required_version <- final_ver[i]
  #   installed_version <- package_ver[which(package_names == n)]
  #   testthat::expect_equivalent(installed_version, 
  #                               required_version,
  #                               info = paste("Package", n, "version mismatch: expected", required_version, "but found", installed_version))
  # }
  
})
