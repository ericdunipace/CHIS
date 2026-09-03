library(readxl)
library(glue)
library(here)

dac_package_file <- here::here("docs", "dac-r-and-stata-packages-list-2.xlsx")

dac_packages <- readxl::read_xlsx(
  dac_package_file,
  sheet = 4L,
  col_type = c("text", "text")
)

dac_packages <- dac_packages[
  !is.na(dac_packages$Package) & !is.na(dac_packages$Version),
]
dac_names <- trimws(dac_packages$Package)
dac_versions <- trimws(dac_packages$Version)

# These override DAC spreadsheet/import artifacts or package constraints that
# otherwise prevent a local renv restore from completing on macOS.
version_overrides <- c(
  askpass = "1.1",
  gridExtra = "2.3",
  highr = "0.10",
  rstudioapi = "0.14",
  xml2 = "1.6.0",
  bslib = "0.12.0",
  cachem = "1.1.0",
  fastmap = "1.2.0",
  htmltools = "0.5.9",
  sass = "0.4.10",
  commonmark = "2.0.0",
  Rcpp = "1.1.0",
  V8 = "4.4.2",
  gdtools = "0.5.1",
  fs = "1.6.6",
  systemfonts = "1.3.1",
  ggplot = "3.5.0",
  gtable = "0.3.6",
  scales = "1.4.0"
)

normalize_versions <- function(package_names, package_versions) {
  package_versions <- as.character(package_versions)
  package_versions <- trimws(package_versions)

  matched_overrides <- intersect(package_names, names(version_overrides))
  package_versions[match(
    matched_overrides,
    package_names
  )] <- version_overrides[matched_overrides]

  package_versions
}

record_dac_versions <- function(package_names, package_versions) {
  final_names <- dac_names[dac_names %in% package_names]
  final_versions <- dac_versions[dac_names %in% package_names]
  final_versions <- normalize_versions(final_names, final_versions)

  current_versions <- package_versions[match(final_names, package_names)]
  needs_update <- is.na(current_versions) | current_versions != final_versions

  for (i in which(needs_update)) {
    renv::record(glue("{final_names[i]}@{final_versions[i]}"))
  }
}

lockfile <- renv::lockfile_read("renv.lock")
lockfile_names <- names(lockfile$Packages)
lockfile_versions <- vapply(lockfile$Packages, `[[`, character(1), "Version")
record_dac_versions(lockfile_names, lockfile_versions)

installed_packages <- installed.packages()
installed_names <- installed_packages[, "Package"]
installed_versions <- installed_packages[, "Version"]
record_dac_versions(installed_names, installed_versions)

old_options <- options(pkgType = "binary")
on.exit(options(old_options), add = TRUE)

renv::restore(prompt = FALSE)
