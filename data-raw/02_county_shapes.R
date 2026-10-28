library(sf)
library(here)
library(dplyr)

here::i_am("data-raw/02_county_shapes.R")

county_shp_file_name <- here::here(
  "data-raw",
  "downloads",
  "tl_2024_us_county",
  "tl_2024_us_county.shp"
)

if (!file.exists(county_shp_file_name)) {
  dl_county_url <- "https://www2.census.gov/geo/tiger/TIGER2024/COUNTY/tl_2024_us_county.zip"
  dest_dir <- here::here("data-raw", "downloads", "tl_2024_us_county")
  dir.create(dest_dir, recursive = TRUE)
  fn <- curl::curl_download(
    dl_county_url,
    here::here(dest_dir, "county.zip"),
    quiet = FALSE
  )
  unfn <- unzip(fn, exdir = dest_dir)
  file.remove(fn)
}

ca_county <- sf::st_read(county_shp_file_name) %>%
  filter(STATEFP == "06") %>%
  rename(county = NAME)

# save as rds
dir.create(here::here("data-raw", "intermediate"), showWarnings = FALSE)
saveRDS(ca_county, file = here::here("data-raw", "intermediate", "ca_county.rds"))