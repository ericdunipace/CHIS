# Builds the data files in data/ that the local analysis reads.
# Run from the repository root: Rscript data-raw/00_build_data.R

# 1. data/auxiliary_data.rds
# 01-03 download Census, county and PRISM files, so each runs only if its
# outputs in data-raw/intermediate/ are missing
if (!file.exists("data-raw/intermediate/ca_tract_2010.rds")) {
  callr::rscript("data-raw/01_census_tracts.R")
}
if (!file.exists("data-raw/intermediate/ca_county.rds")) {
  callr::rscript("data-raw/02_county_shapes.R")
}
if (
  !all(file.exists(c(
    "data-raw/intermediate/prism_vars_census.rds",
    "data-raw/intermediate/prism_vars_county.rds",
    "data-raw/intermediate/map_census_temp.rds",
    "data-raw/intermediate/map_census_heat.rds"
  )))
) {
  callr::rscript("data-raw/03_prism.R")
}

# 2. data/tmax_annual_ca_youth.rds (needs a Census API key in CENSUS_API_KEY)
callr::rscript("data-raw/04_adolescent_tmax.R")


callr::rscript("data-raw/05_auxiliary_data.R")

# 3. data/chis_puf_combined.Rds (needs the CHIS teen PUF .dta files in data/;
#    skips itself if the file already exists)
callr::rscript("data-raw/06_chis_puf.R")
