# code to get variable lables and make a table for lookup when SAS data is used
library(haven)
library(dplyr)
library(here)

#2023
d_chis_2023_stata_to_sas_list <- haven::read_dta(here::here('Data', 'dummyfile_2023_teen_stata', 'TEEN_with_format.dta')) %>%
  rename_all(tolower) %>% sapply(function(x) attr(x, "labels")) %>% 
  Filter(Negate(is.null), .)

for(d in names(d_chis_2023_stata_to_sas_list)) {
  if(!is.null(d_chis_2023_stata_to_sas_list[[d]])) names(d_chis_2023_stata_to_sas_list[[d]]) <- iconv(names(d_chis_2023_stata_to_sas_list[[d]]), from = "latin1", to = "UTF-8")
}

#2022
d_chis_2022_stata_to_sas_list <- haven::read_dta(here::here('Data', 'teen 2022 dummy STATA', 'TEEN_with_format.dta')) %>%
  rename_all(tolower) %>% sapply(function(x) attr(x, "labels")) %>% 
  Filter(Negate(is.null), .)

for(d in names(d_chis_2022_stata_to_sas_list)) {
  if(!is.null(d_chis_2022_stata_to_sas_list[[d]])) names(d_chis_2022_stata_to_sas_list[[d]]) <- iconv(names(d_chis_2022_stata_to_sas_list[[d]]), from = "latin1", to = "UTF-8")
}

#2021
d_chis_2021_stata_to_sas_list <- haven::read_dta(here::here('Data', 'dummyfile_2021_teen_stata', 'TEEN_with_format.dta')) %>%
  rename_all(tolower) %>% sapply(function(x) attr(x, "labels")) %>% 
  Filter(Negate(is.null), .)

for(d in names(d_chis_2021_stata_to_sas_list)) {
  if(!is.null(d_chis_2021_stata_to_sas_list[[d]])) names(d_chis_2021_stata_to_sas_list[[d]]) <- iconv(names(d_chis_2021_stata_to_sas_list[[d]]), from = "latin1", to = "UTF-8")
}


to_file <- here::here("R","Functions.R")

file.copy(from = here::here("R","localCode","BaseFileFunctions.R"),
          to =   to_file,
          overwrite = TRUE
)

dump(
  list = c("d_chis_2021_stata_to_sas_list", "d_chis_2022_stata_to_sas_list", "d_chis_2023_stata_to_sas_list"),
  file = to_file,
  append = TRUE
)
cat("\n", file = to_file, append = TRUE)
