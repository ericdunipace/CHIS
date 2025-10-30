library(dplyr)
library(here)

#### load teen approved variables
teen_approved_vars <- read.csv(here::here("data-raw","mastervariablelist-2023-teen.csv"), stringsAsFactors = FALSE) %>% 
  select(VARIABLE.NAME, CHIS.2023, CHIS.2022, CHIS.2021)

# get list of approved variables across years
get_approved_vars <- teen_approved_vars %>% 
  filter(if_any(starts_with("CHIS"), ~ .x %in% c("X","x")))

varnames_teeny <- get_approved_vars$VARIABLE.NAME %>% tolower()

saveRDS(varnames_teeny,
        file = here::here("data","teen_approved_vars.rds"))