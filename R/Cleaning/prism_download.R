library(prism)
library(here)
library(sf)
library(terra)

source("R/Functions.R")

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


# for each tract10 value
# [ ] variable 15 year normals annual (2004 - 2019)
# [ ] variable 15 year normals 90 day periods (2004 - 2019)
# [ ] variable 1 year avg for each month and year.... 
# [ ] variable 1 year lag avg for each month and year.... 
# [ ] variable 90 day avg for each month and year ....
# [ ] create delta 1 year - normal
# [ ] create delta 1 year lag - normal
# [ ] create delta 90 day - 90 day historical
# [ ] also do for heatwaves
# [ ] create 2023,2022,2021 tmax avg and heatwave avg for maps

census_shapefile <- readRDS(here::here("Data","ca_tract_2010.rds"))

census_temp <- prism_to_map("tmax", census_shapefile, 2021:2023, "census", cutoff = 32)
census_heatwave <- prism_to_map("tmax", census_shapefile, 2021:2023, "census", heatwave = TRUE, cutoff = 32)

cens_temp_save <- cenus_temp %>% select(tract10, year, tmax)