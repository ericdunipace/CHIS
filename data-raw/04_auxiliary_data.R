#### File to combine auxiliary data files and save as an RDS ####
# needs the intermediate files from 01_census_tracts.R, 02_county_shapes.R and
# 03_prism.R

library(here)

here::i_am("data-raw/04_auxiliary_data.R")

# load data
prism_census <- readRDS(here::here("data-raw", "intermediate", "prism_vars_census.rds"))
prism_county <- readRDS(here::here("data-raw", "intermediate", "prism_vars_county.rds"))
census_ca <- readRDS(here::here("data-raw", "intermediate", "ca_tract_2010.rds"))
county_ca <- readRDS(here::here("data-raw", "intermediate", "ca_county.rds"))
cens_temp_save <- readRDS(here::here("data-raw", "intermediate", "map_census_temp.rds"))
cens_heat_save <- readRDS(here::here("data-raw", "intermediate", "map_census_heat.rds"))

# make list
aux_data <- list(
  prism = list(census = prism_census, county = prism_county),
  census_ca = census_ca,
  county_ca = county_ca,
  map_census_temp = cens_temp_save,
  map_census_heat = cens_heat_save
)

# save auxiliary data
saveRDS(aux_data, file = here::here("data", "auxiliary_data.rds"))
