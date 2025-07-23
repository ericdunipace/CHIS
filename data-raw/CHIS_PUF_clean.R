### Disparities research in barriers to mental health #############################################

### Import and read stuff in ######################################################################
# Load required libraries (ensure they are installed)
library(dplyr)       # Data manipulation
library(haven)       # Reading .dta files
library(purrr)       # Functional programming
library(forcats)     # Factor manipulation
library(lubridate)

here::i_am("R/Cleaning/CHIS_PUF_clean.R")  # adjust this to your actual file location


# Import PUF datasets
chis_2023 <- haven::read_dta(here::here('Data','teen_2023_stata/TEEN.dta')) %>%
  rename_all(tolower) %>%
  mutate(year = 2023) %>% 
  haven::as_factor()

chis_2022 <- haven::read_dta(here::here('Data','teen_stata_2022/TEEN.dta')) %>%
  rename_all(tolower) %>%
  mutate(year = 2022) %>% 
  haven::as_factor()

chis_2021 <- haven::read_dta(here::here('Data','teen_stata_2021/TEEN.dta')) %>%
  rename_all(tolower) %>%
  mutate(year = 2021) %>% 
  haven::as_factor()


my_chis_list <- list(chis_2021, chis_2022, chis_2023) # work with PUF data

#### Load functions ####
source(here::here("R","Functions.R"))

#### Clean data ####

chis <- chis_clean(my_chis_list) %>% 
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
saveRDS(chis, file = here::here("data","chis_puf_combined.Rds"))

