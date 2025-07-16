### Disparities research in barriers to mental health #############################################

### Import and read stuff in ######################################################################
# Load required libraries (ensure they are installed)
library(dplyr)       # Data manipulation
library(haven)       # Reading .dta files
library(survey)      # Survey data analysis
library(ggplot2)     # Data visualization
library(sf)          # Spatial data handling and mapping
library(prism)       # Prism data visualization, for temperatures
library(purrr)       # Functional programming
library(gtsummary)   # for summary tables
library(RColorBrewer)# map colors
library(lme4)        # Linear mixed-effects models
library(forcats)     # Factor manipulation
library(rstudioapi)  # For setting working directory to script location
library(glue)
library(exactextractr)
library(raster)
library(lubridate)
library(stringr)
library(terra)

#### load data ####

# Step 1: 
# Set Wd

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Sets to script location
setwd("..")  # Moves up to the project root
getwd()
prism::prism_set_dl_dir(path = "Data/prism", create = TRUE)

# Import confidential dummy datasets
d_chis_2023 <- haven::read_dta('Data/dummyfile_2023_teen_stata/TEEN_with_format.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2023) %>% 
  haven::as_factor()

d_chis_2022 <- haven::read_dta('Data/teen 2022 dummy STATA/TEEN_with_format.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2022) %>% 
  haven::as_factor()

d_chis_2021 <- haven::read_dta('Data/dummyfile_2021_teen_stata/TEEN_with_format.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2021) %>% 
  haven::as_factor()

# Add dummy data to list
my_chis_list <- list(d_chis_2021, d_chis_2022, d_chis_2023) # work with confidential data
# my_chis_list <- list(chis_2021, chis_2022, chis_2023) # work with PUF data

# Load shapefile of the county
ca_counties <- sf::st_read("Data/tl_2024_us_county/tl_2024_us_county.shp") %>% 
  filter(STATEFP == "06") %>%
  rename(county = NAME)

# load census tract shapefile
# ca_census <- sf::st_read(
#   "Data/ca_tract_2010.gpkg"
# )
ca_census <- readRDS("Data/ca_tract_2010.rds")


#### Load functions ####
source("R/Functions.R")

#### Outcome Variables ####
mental_health_issues <- c(
  "te68_13",  # Reasons use e-cigs: Reduce stress, anxiety, or pain
  "tf11",     # Received psychological/emotional counseling in the past 12 months
  "ti11",     # Needed help for emotional problems in the past 12 months
  "tg11",     # Felt nervous in the past 30 days
  "tg12",     # Felt hopeless in the past 30 days
  "tg14",     # Felt depressed in the past 30 days
  "dstrs30",  # Likely has psychological distress in the past month
  "dstrsyr",  # Serious psychological distress for worst month in the past year (K6 score)
  "dstrs12",  # Likely has had psychological distress in the last year
  "distress", # Serious psychological distress
  "tf31",     # Felt nervous in the worst month
  "tf32",     # Felt hopeless in the worst month
  "tf34"      # Felt depressed in the worst month
);

#### Outcome Variables ####
K6_vars        <- c("tg11",
                    "tg12",
                    "tg13",
                    "tg14",
                    "tg15",
                    "tg16")

# subset tf30 == "Yes" (roughly corresponds to Inapplicable)
worst_K6_vars  <- c("tf31",
                    "tf32",
                    "tf33",
                    "tf34",
                    "tf35",
                    "tf36")

suicide_vars   <- c("EVER THOUGHT TO COMMIT SUICIDE" = "tk1",
                    # for these other ones tk1 == "Yes", 
                    # can just screen by not "Inapplicable"
                    "EVER THOUGHT TO COMMIT SUICIDE PAST 12 M" = "tk2",
                    "THOUGHT TO COMMIT SUICIDE PAST 2 MOS"
                    = "tk3",
                    "EVER ATTEMPTED SUICIDE" = "tk4",
                    "ATTEMPTED SUICIDE PAST 12 MOS" = "tk5")

#### Clean data ####
# Get the temperature data for the years of interest
# debugonce(data_var_from_prism)
chis_geo <- pooling(my_chis_list) %>%
  mutate(across(where(is.factor), fct_drop)) %>% 
  mutate(racecn_p = forcats::fct_recode(racecn_p,
                                        "PI/other Single Race"="Pi/other Single Race",
                                        "American Indian/Alaska Native" = "American Indian/alaska Native"),
         povll = forcats::fct_recode(povll,
                                     "0-99% FPL" = "0-99% Fpl",
                                     "100-199% FPL" = "100-199% Fpl",
                                     "200-299% FPL" = "200-299% Fpl",
                                     "300% FPL And Above" = "300% Fpl And Above")
  ) %>% 
  # mutate(K6 = create_K6_score(across(all_of(K6_vars))),
  #        worst_K6 = create_K6_score(across(all_of(worst_K6_vars))),
  #        K6_ge_13 = as.integer(K6 >= 13),
  #        worst_K6_ge_13 = as.integer(worst_K6 >= 13),
  #        max_K6 = pmax(K6,worst_K6)
  # ) %>% 
  mutate(alcohol = factor(ifelse(as.character(te24a) =="Inapplicable", 
                                 as.character(te24),
                                 as.character(te24a)),
                          levels = levels(te24a))) %>% 
  mutate(tf29v2 = ifelse(tf29v2 == 997, 0.5, tf29v2)) %>% 
  mutate(survey_dates = parse_my_to_date(tadate_mm),
         survey_years  = lubridate::year(survey_dates),
         survey_months = lubridate::month(survey_dates)) %>% 
  mutate(county = fips_cnt) %>% 
  data_var_from_prism("tmax", ca_census, admin.level = "census", cutoff = 32) %>% 
  # data_var_from_prism("tmean", ca_census, admin.level = "census", cutoff = 32) %>%
  data_var_from_prism("ppt", ca_census, admin.level = "census") %>%
  data_var_from_prism("vpdmax", ca_census, admin.level = "census") %>% 
  as.data.frame()# %>% 
  # dplyr::select(-geometry)

for(j in colnames(d_chis_2023)) {
  attr(chis_geo[[j]], "label") <- attr(d_chis_2023[[j]], "label")
}

#### Save Data ####
saveRDS(chis_geo, file = "Data/chis_combined.Rds")
