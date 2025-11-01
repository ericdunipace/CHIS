library(dplyr)

source(here::here("data-raw","chis_stata_labels_for_sas.R"))
source(here::here("data-raw","approved_var_select.R"))

to_file <- here::here("R","Functions.R")

file.copy(from = here::here("R","localCode","BaseFileFunctions.R"),
          to =   to_file,
          overwrite = TRUE
)

sas_to_factor_list <- list(
  "2021" = d_chis_2021_stata_to_sas_list,
  "2022" = d_chis_2022_stata_to_sas_list,
  "2023" = d_chis_2023_stata_to_sas_list)

dump(
  list = c("varnames_teeny","sas_to_factor_list"),
  file = to_file,
  append = TRUE
)
cat("\n", file = to_file, append = TRUE)
