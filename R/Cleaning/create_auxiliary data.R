#### File to combine auxillary data files and save as an RDS ####
# need to run two files first
# source(here::here("R", "Census_2010_clean.R"))
# source(here::here("R", "prism_download.R"))

library(here)

here::i_am("R/Cleaning/create_auxiliary data.R")  # adjust this to your actual file location

# load data
prism_census   <- readRDS(here::here("Data",
                                "prism_vars_census.rds"))
prism_county   <- readRDS(here::here("Data",
                                     "prism_vars_county.rds"))
census_ca      <- readRDS(here::here("Data", "ca_tract_2010.rds")) 
county_ca      <- readRDS(here::here("Data", "ca_county.rds"))
cens_temp_save <- readRDS(here::here("Data","map_census_temp.rds"))
cens_heat_save <- readRDS(here::here("Data","map_census_heat.rds"))

# make list
aux_data <- list(
  prism           = list(census = prism_census,
                         county = prism_county),
  census_ca       = census_ca,
  county_ca       = county_ca,
  map_census_temp = cens_temp_save,
  map_census_heat = cens_heat_save
)

# save auxiliary data
saveRDS(aux_data, file = here::here("Data","auxillary_data.rds"))
