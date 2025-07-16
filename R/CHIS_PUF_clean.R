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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Sets to script location
setwd("..")  # Moves up to the project root
getwd()


# Import PUF datasets
chis_2023 <- haven::read_dta('Data/teen_2023_stata/TEEN.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2023) %>% 
  haven::as_factor()

chis_2022 <- haven::read_dta('Data/teen_stata_2022/TEEN.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2022) %>% 
  haven::as_factor()

chis_2021 <- haven::read_dta('Data/teen_stata_2021/TEEN.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2021) %>% 
  haven::as_factor()


my_chis_list <- list(chis_2021, chis_2022, chis_2023) # work with PUF data

#### Load functions ####
source("R/Functions.R")

#### Clean data ####

chis <- pooling(my_chis_list) %>%
  mutate(across(where(is.factor), fct_drop))   %>% 
  mutate(
    ombsrtn_p1 = forcats::fct_recode(ombsrtn_p1,
                                     "Hispanic" = "Hispanic",
                                     "White, Non-Hispanic" = "White, Non-hispanic (nh)",
                                     "Asian, Non-Hispanic" = "Asian Only, Nh",
                                     "Two Or More Races, Non-Hispanic" = "Two Or More Races, Nh"),
    povll = forcats::fct_recode(povll,
                                "0-99% FPL" = "0-99% Fpl",
                                "100-199% FPL" = "100-199% Fpl",
                                "200-299% FPL" = "200-299% Fpl",
                                "300% FPL And Above" = "300% Fpl And Above"),
    ahedtc_p1 = forcats::fct_recode(ahedtc_p1,
                                    "Grade 12/H.S. Diploma" = "Grade 12/h.s. Diploma",
                                    "AA/AS Degree Or Vocational School" = "Aa/as Degree Or Vocational School",
                                    "BA Or BS Degree/Some Grad School" = "Ba Or Bs Degree/some Grad School",
                                    "MA Or MS Degree" = "Ma Or Ms Degree",
                                    "Ph.D. Or Equivalent" = "Ph.d. Or Equivalent"
    )
  ) %>% 
  mutate(tf29v2 = ifelse(tf29v2 == 997, 0.5, tf29v2)) %>% 
  mutate(age_group = factor(ifelse(srage_p %in% c("12","13","14"),
                                   "12-14","15-17"))) %>% 
  mutate(survey_dates = parse_my_to_date(tadate_mm),
         survey_years  = lubridate::year(survey_dates),
         survey_months = lubridate::month(survey_dates)) %>% 
  mutate(
    across(
      everything(),
      ~ {
        # grab the name of the column we’re working on
        nm <- cur_column()
        # copy the label attribute from the original
        l2023 <- attr(chis_2023[[nm]], "label")
        l2022 <- attr(chis_2022[[nm]], "label")
        l2021 <- attr(chis_2021[[nm]], "label")
        ll    <- if(!is.null(l2023)) {
          l2023
        } else if (is.null(l2023) && !is.null(l2022)) {
          l2022
        } else if (is.null(l2023) && is.null(l2022) && !is.null(l2021)) {
          l2021
        } else {
          NULL
        }
        attr(.x, "label") <- ll
        # return the (now-labelled) vector
        .x
      }
    )
  )


# labelled::var_label(chis) <- labelled::var_label(chis_2023)

#### Save as RDS ####
saveRDS(chis, file = "Data/chis_puf_combined.Rds")

