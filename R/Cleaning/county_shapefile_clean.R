library(sf)
library(here)
library(dplyr)


ca_county <- sf::st_read(here::here("Data"
                       , "tl_2024_us_county/tl_2024_us_county.shp")) %>% 
  filter(STATEFP == "06") %>%
  rename(county = NAME)

# save as rds
saveRDS(ca_county, file = here::here("Data","ca_county.rds"))