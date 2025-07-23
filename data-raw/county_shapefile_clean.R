library(sf)
library(here)
library(dplyr)

here::i_am("data-raw/county_shapefile_clean.R")

ca_county <- sf::st_read(here::here("data-raw"
                       , "tl_2024_us_county/tl_2024_us_county.shp")) %>% 
  filter(STATEFP == "06") %>%
  rename(county = NAME)

# save as rds
saveRDS(ca_county, file = here::here("data-raw","ca_county.rds"))