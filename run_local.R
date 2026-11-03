# Runs local analyses and then builds the manuscript
# If you need to rebuild some of the intermediate data
# see the README.md

# 1. analysis scripts (the two CHIS PUF scripts skip themselves if the PUF is missing)
callr::rscript("R/localCode/Combine_Remote_Results.R")
callr::rscript("R/localCode/CHIS_PUF.R")
callr::rscript("R/localCode/testing_change.R")

# 2. manuscript (needs the CHIS PUF results, so skipped without the PUF)
if (file.exists("data/chis_puf_combined.Rds")) {
  # Quarto starts R in manuscript/, so point it at the project's renv library
  Sys.setenv(
    RENV_PROJECT = getwd(),
    R_PROFILE_USER = normalizePath("renv/activate.R"),
    QUARTO_R = R.home("bin")
  )
  system("quarto render manuscript/chis_manuscript.qmd --to docx")
  system("quarto render manuscript/chis_supplement.qmd --to pdf")
  system("quarto render manuscript/chis_tables.qmd --to docx")
}
