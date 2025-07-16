# Clean the census layers
ca_census <- sf::st_read("Data/california_census_2010/california_census_2010.shp") %>% 
  mutate(tract10 = paste(STATEFP10, COUNTYFP10, TRACTCE10, sep = "")) %>% 
  group_by(tract10) %>%
  summarize(
    geometry = sf::st_union(geometry), # Dissolve boundaries
    PARTFLG = "N", # all groups have same flag
    HOUSING10 = sum(HOUSING10), # Sum housing units for each tract
    POP10 = sum(POP10), # Sum population for each tract
    .groups = "drop"
  )

ca_census %>% sf::st_write(dsn = "Data/ca_tract_2010.gpkg", delete_layer = TRUE)
ca_census %>% saveRDS(file = "Data/ca_tract_2010.rds")
