# Builds the data files in data/ that the local analysis reads.
# Run from the repository root: Rscript data-raw/build_data.R

# 1. data/auxiliary_data.rds (downloads Census, county and PRISM files)
callr::rscript("data-raw/01_census_tracts.R")
callr::rscript("data-raw/02_county_shapes.R")
callr::rscript("data-raw/03_prism.R")
callr::rscript("data-raw/04_auxiliary_data.R")

# 2. data/tmax_annual_ca_youth.rds (needs a Census API key in CENSUS_API_KEY)
callr::rscript("data-raw/05_adolescent_tmax.R")

# 3. data/chis_puf_combined.Rds (needs the CHIS teen PUF .dta files in data/)
callr::rscript("data-raw/chis_puf.R")
