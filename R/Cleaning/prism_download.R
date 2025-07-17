library(prism)
library(here)
library(sf)
library(terra)
library(dplyr)
library(glue)
library(lubridate)

source(here::here("R","Functions.R"))

prism::prism_set_dl_dir(path = here::here("Data","prism"), create = TRUE)


sapply(c("tmean","tmax","ppt","vpdmax"), function(x) prism::get_prism_annual(type = x, years = 2000:2023,keepZip = FALSE))

sapply(c("tmean","tmax","ppt","vpdmax"), function(var) prism::get_prism_dailys(
  type = var,
  minDate = "2020-01-01",
  maxDate = "2023-12-31",
  keepZip = FALSE)
)

prism::get_prism_dailys(
  type = "tmax",
  minDate = "2000-01-01",
  maxDate = "2019-12-31",
  keepZip = FALSE
)


# for each tract10 value, for each county
# [X] variable 15 year normals annual (2004 - 2019)
# [X] variable 15 year normals 90 day periods (2004 - 2019)
# [X] variable 1 year avg for each month and year.... 
# [X] variable 1 year lag avg for each month and year.... 
# [X] variable 90 day avg for each month and year ....
# [X] create delta 1 year - normal
# [X] create delta 1 year lag - normal
# [X] create delta 90 day - normal
# [X] also do for heatwaves

#### Prism data for normals ####
# note must have already downloaded the census shapefile and cleaned
# source(here::here("R","Census_2010_clean.R"))
stopifnot("census file does not exist" = file.exists(here::here("Data","ca_tract_2010.rds")))

prism_vars.fn <- here::here("Data","prism_vars.rds")

if(!file.exists(prism_vars.fn)) {
  message("prism_vars being created...")
  
  # read in census shapefile
  census_shapefile <- readRDS(here::here("Data","ca_tract_2010.rds"))
  
  tracts <- census_shapefile$tract10
  ntracts<- length(census_shapefile$tract10)
  
  survey_years <- rep(2021:2023, each = 12)
  survey_months <- rep(1:12, 3)
  
  dummy_df <- data.frame(tract10 = rep(tracts,
                                       each = length(survey_years)),
                         survey_years = rep(survey_years, ntracts),
                         survey_months = rep(survey_months, ntracts),
                         year = rep(survey_years, ntracts)
  ) %>% 
    mutate(survey_dates = paste0(survey_years, "-", survey_months, "-15") %>% lubridate::date()
    )
  
  prism_vars <- dummy_df %>% 
    data_var_from_prism("tmax", census_shapefile, admin.level = "census", cutoff = 32)  %>% 
    data_var_from_prism("ppt", census_shapefile, admin.level = "census") %>%
    data_var_from_prism("vpdmax", census_shapefile, admin.level = "census") 
  
  saveRDS(prism_vars,
          file = prism_vars.fn)
} else {
  prism_vars <- readRDS(prism_vars.fn)
  message(glue::glue("prism_vars loaded from {prism_vars.fn}"))
}


##### create 2023,2022,2021 tmax avg and heatwave avg for maps ####
census_temp.fn <- here::here("Data","map_census_temp.rds")
census_heat.fn <- here::here("Data","map_census_heat.rds")

if ( !file.exists(census_temp.fn) || !file.exists(census_heat.fn) ) {
  # read in census shapefile
  census_shapefile <- readRDS(here::here("Data","ca_tract_2010.rds"))
  
  # create avg of vars by years for tmax
  census_temp <- prism_to_map("tmax", census_shapefile, 2021:2023, "census", cutoff = 32)
  
  # create avg of vars by years for heatwave
  census_heatwave <- prism_to_map("tmax", census_shapefile, 2021:2023, "census", heatwave = TRUE, cutoff = 32)
  
  # select columns to save for memory purposes
  cens_temp_save <- census_temp %>% 
    as.data.frame() %>% 
    mutate(map_tmax = tmax) %>% 
    select(tract10, year, map_tmax)
  cens_heat_save <- census_heatwave %>% 
    as.data.frame() %>% 
    mutate(map_days_above32 = days_above32) 
    select(tract10, year, map_days_above32)
  
  # save the variables as RDS files
  saveRDS(cens_temp_save, file = census_temp.fn)
  saveRDS(cens_heat_save, file = census_heat.fn)

} else {
  cens_temp_save <- readRDS(census_temp.fn)
  cens_heat_save <- readRDS(census_heat.fn)
  message(glue::glue("census temp and heatwave data loaded from {census_temp.fn} and {county_temp.fn}"))
}

# #### combine all data into one ####
# 
# prism_data <- prism_vars %>%
#   left_join(y = cens_temp_save, by = c("tract10","year")) %>%
#   left_join(y = cens_heat_save, by = c("tract10","year"))
# 
# saveRDS(prism_data, file = here::here("Data",
#                                       "prism_data_by_tract10.rds"))
