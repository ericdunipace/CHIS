#### File to combine auxiliary data files and save as an RDS ####
# needs the intermediate files from 01_census_tracts.R, 02_county_shapes.R and
# 03_prism.R, plus the DAC-approved variable list and the dummy Stata files
# (for SAS value labels). The DAC run gets these lookups from this RDS.

library(here)

here::i_am("data-raw/04_auxiliary_data.R")

# approved variable list (varnames_teeny) and Stata value labels for SAS inputs
source(here::here("tools", "approved_var_select.R"))
source(here::here("tools", "chis_stata_labels_for_sas.R"))

# load data
prism_census <- readRDS(here::here(
  "data-raw",
  "intermediate",
  "prism_vars_census.rds"
))
prism_county <- readRDS(here::here(
  "data-raw",
  "intermediate",
  "prism_vars_county.rds"
))
census_ca <- readRDS(here::here(
  "data-raw",
  "intermediate",
  "ca_tract_2010.rds"
))
county_ca <- readRDS(here::here("data-raw", "intermediate", "ca_county.rds"))
cens_temp_save <- readRDS(here::here(
  "data-raw",
  "intermediate",
  "map_census_temp.rds"
))
cens_heat_save <- readRDS(here::here(
  "data-raw",
  "intermediate",
  "map_census_heat.rds"
))

# not to be added to auxiliary file but used for means
ca_youth_2010 <- readRDS(here::here("data-raw", "ca_youth_2010.rds"))

# demean census data
id_vars <- c("survey_years", "survey_months", "year", "survey_dates")

# tract -> county lookup (first 5 chars of tract10 = state + county FIPS)
county_key <- county_ca |>
  sf::st_drop_geometry() |>
  dplyr::select(GEOID, county)


# rename county vars to match tract names, then tag with _cty
prism_county_wide <- prism_county |>
  dplyr::rename_with(~ sub("_county_", "_tract10_", .x)) |>
  dplyr::rename_with(
    ~ paste0(.x, "_cty"),
    .cols = -dplyr::all_of(c("county", id_vars))
  )

demean_vars <- setdiff(names(prism_census), c("tract10", id_vars))

prism_census <- prism_census |>
  dplyr::mutate(GEOID = substr(tract10, 1, 5)) |>
  dplyr::left_join(county_key, by = "GEOID") |>
  dplyr::left_join(
    ca_youth_2010 |> dplyr::select(tract10, year, pop12_17),
    by = c("tract10", "year")
  ) |>
  dplyr::group_by(county, dplyr::across(dplyr::all_of(id_vars))) |>
  dplyr::mutate(dplyr::across(
    dplyr::all_of(demean_vars),
    ~ .x - weighted.mean(.x, pop12_17, na.rm = TRUE)
  )) |>
  dplyr::ungroup() |>
  dplyr::select(-GEOID, -pop12_17)

# make list
aux_data <- list(
  prism = list(census = prism_census, county = prism_county),
  census_ca = census_ca,
  county_ca = county_ca,
  map_census_temp = cens_temp_save,
  map_census_heat = cens_heat_save,
  approved_vars = varnames_teeny,
  sas_labels = list(
    "2021" = d_chis_2021_stata_to_sas_list,
    "2022" = d_chis_2022_stata_to_sas_list,
    "2023" = d_chis_2023_stata_to_sas_list
  )
)

# save auxiliary data
saveRDS(
  aux_data,
  file = here::here("data", "auxiliary_data.rds"),
  compress = "xz"
)
