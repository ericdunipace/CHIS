# CHIS Climate Anxiety Analysis

This repository contains R code and a Quarto manuscript for analyses of climate anxiety among California adolescents using the California Health Interview Survey (CHIS) teen files for 2021, 2022, and 2023. The analysis combines CHIS survey data with county and census-tract climate/geography data, applies CHIS replicate survey weights, and produces descriptive tables, maps, regression outputs, and the manuscript, supplement, and tables documents.

## Environment setup

The project uses `renv` with two lockfiles, selected automatically by `.Rprofile`:

| R version | renv profile | Lockfile | Use |
|---|---|---|---|
| < 4.3 (4.2.3) | `default` | `renv.lock` | DAC environment; matches the DAC package list |
| ≥ 4.3 (4.6.1) | `local` | `renv/profiles/local/renv.lock` | Local analysis and manuscript rendering |

To override the choice, set `RENV_PROFILE` before starting R. From the repository root:

```r
install.packages("renv")
renv::restore()
```

## To reproduce the analyses

The analysis runs in two places:

1. **CHIS Data Access Center (DAC).** `R/CHIS_private.R` runs on the confidential teen files inside the DAC. **Note:** this means that the analysis is not fully reproducible just with this repository alone. The DAC run needs only `R/CHIS_private.R`, `R/Functions.R`, and `data/auxiliary_data.rds`. To run this analysis, do:
```sh
export CHIS_DATA_TYPE=stata   # or sas
Rscript R/CHIS_private.R
```
2. **Local machine.** Scripts in `R/localCode/` read the released DAC outputs in `Outputs_DAC/` and write manuscript tables to `Outputs/tables/` and figures to `Outputs/figures/`. The Quarto documents in `manuscript/` then read those files and generate the manuscript. To run these and render the manuscript:
```sh
Rscript run_local.R
```

You need Quarto (and LaTeX for the PDF supplement) to regenerate the manuscript, and it will only render (at this time) if the public use files are present.

### Building auxiliary data needed for analyses
To build every file needed in `data/` at once run:

```sh
Rscript data-raw/00_build_data.R
```

The other scripts used to build it are in `data-raw/`. You may also need to download the adolescent public use files for 2021, 2022, and 2023 from [CHIS](https://healthpolicy.ucla.edu/our-work/california-health-interview-survey-chis). These are not allowed to be uploaded to github. The other files (such as the PRISM data raw-files) are large and are thus not stored by default on github but can be easily downloaded locally.

To force a step to re-run, delete its output (in `data-raw/intermediate/` for 01-03, or `data/chis_puf_combined.Rds` for the public use files). Please see the individual code files for where the files need to be placed (like the public use files need to be in data with particular folder names, etc.)
