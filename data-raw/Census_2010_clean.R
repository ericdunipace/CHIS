library(sf)
library(dplyr)


census_shp_file_name <- here::here("data-raw","california_census_2010","tabblock2010_06_pophu.shp")

if ( !file.exists(census_shp_file_name) ) {
  dl_census_url <- "https://www2.census.gov/geo/tiger/TIGER2010BLKPOPHU/tabblock2010_06_pophu.zip"
  dest_dir <- here::here("data-raw","california_census_2010")
  dir.create(dest_dir)
  fn <- curl::curl_download(dl_census_url,
                      here::here(dest_dir, "census.zip"), 
                                 quiet = FALSE)
  unfn <- unzip(fn, exdir = dest_dir)
  file.remove(fn)
}

# Clean the census layers
ca_census <- census_shp_file_name %>% 
  sf::st_read() %>% 
  mutate(tract10 = paste(STATEFP10, 
                         COUNTYFP10, 
                         TRACTCE10, 
                         sep = "")) %>% 
  group_by( tract10 ) %>%
  summarize(
    geometry = sf::st_union(geometry), # Dissolve boundaries
    PARTFLG = "N", # all groups have same flag
    HOUSING10 = sum(HOUSING10), # Sum housing units for each tract
    POP10 = sum(POP10), # Sum population for each tract
    .groups = "drop"
  )

# write as gpkg if want
# ca_census %>% sf::st_write(dsn = "Data/ca_tract_2010.gpkg", delete_layer = TRUE)
ca_census %>% saveRDS(file = here::here("data-raw","ca_tract_2010.rds"))
