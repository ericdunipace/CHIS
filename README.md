# CHIS Climate Anxiety Analysis

This repository contains R code for analyses of climate anxiety among California adolescents using the California Health Interview Survey (CHIS) teen files for 2021, 2022, and 2023. The analysis combines CHIS survey data with county and census-tract climate/geography data, applies CHIS replicate survey weights, and produces descriptive tables, maps, regression outputs, and manuscript-ready table/figure files.

The main goal of this README is to document how to recreate the analysis as closely as possible from the repository contents and the required source data.

## Repository Structure

```text
.
├── R/
│   ├── CHIS_private.R                 # Main DAC/confidential-data analysis script
│   ├── Functions.R                    # Shared cleaning, modeling, survey, table, and plotting functions
│   └── localCode/
│       ├── CHIS_PUF.R                 # Public-use-file analysis script
│       ├── Climate_Change_Youth_PUF.R # Exploratory/local PUF workflow
│       └── Combine_Remote_Results.R   # Builds manuscript tables/figures from Outputs/
├── data-raw/
│   ├── CHIS_PUF_clean.R               # Creates combined PUF RDS from CHIS teen .dta files
│   ├── approved_var_select.R          # Creates approved-variable list
│   ├── Census_2010_clean.R            # Downloads/cleans 2010 census tract geometries
│   ├── county_shapefile_clean.R       # Cleans county shapefile
│   ├── prism_download.R               # Downloads/processes PRISM climate data
│   └── create_auxiliary data.R        # Bundles climate/geography support data
├── Data/                              # Input data and precomputed RDS files
├── Outputs/                           # Analysis CSV/PDF outputs
├── Documents/                         # Manuscript-ready tables and rendered table artifacts
├── Figures/                           # Manuscript-ready figures
├── tests/testthat/                    # Reproducibility and package/version checks
├── docs/                              # DAC package/version reference files
├── renv.lock                          # R package lockfile
└── CHIS.Rproj                         # RStudio project file
```

## Data Requirements

The analysis is not fully reproducible from public code alone because it depends on CHIS teen data files and precomputed auxiliary climate/geography data. To rerun the full analysis, the following files are expected.

### CHIS Teen Files

For the confidential/DAC workflow, provide CHIS teen data for:

- 2021
- 2022
- 2023

`R/CHIS_private.R` can read either Stata (`.dta`) or SAS (`.sas7bdat`) inputs through `haven`. Set `CHIS_DATA_TYPE` to either `stata` or `sas`, or explicitly edit the `file_name_2021`, `file_name_2022`, and `file_name_2023` variables near the top of `R/CHIS_private.R`.

The repository includes dummy/sample file paths under `Data/`, but a real DAC run should point to the approved CHIS files supplied in the secure environment.

### Auxiliary Climate and Geography Data

The main confidential workflow expects:

```text
Data/auxiliary_data.rds
```

This file is read by `R/CHIS_private.R` and should contain:

- PRISM-derived climate variables by census tract and county
- California census tract geometry
- California county geometry
- map-ready census temperature and heatwave summaries

The scripts used to build these inputs are in `data-raw/`:

1. `data-raw/Census_2010_clean.R`
2. `data-raw/county_shapefile_clean.R`
3. `data-raw/prism_download.R`
4. `data-raw/create_auxiliary data.R`

Recreating the auxiliary data may require downloading external census/TIGER and PRISM data and may take substantial time.

### Public-Use-File Combined Data

The local public-use-file workflow expects:

```text
Data/chis_puf_combined.Rds
```

This can be recreated from the CHIS teen public-use Stata files with:

```r
source("data-raw/CHIS_PUF_clean.R")
```

That script reads:

```text
Data/teen_stata_2021/TEEN.dta
Data/teen_stata_2022/TEEN.dta
Data/teen_2023_stata/TEEN.dta
```

## Environment Setup

The project uses `renv`. The lockfile records R `4.2.3`, which is also the R version used by the DAC-oriented GitHub Actions workflows.

From the repository root:

```r
install.packages("renv")
renv::restore()
```

Some packages, especially `sf`, `terra`, and `units`, require geospatial system libraries. On Linux, the GitHub Actions workflows install:

```text
libcurl4-openssl-dev libssl-dev libxml2-dev
libharfbuzz-dev libfribidi-dev libfreetype6-dev
libpng-dev libtiff5-dev libjpeg-dev
libudunits2-dev libgdal-dev gdal-bin
libgeos-dev libproj-dev libgettextpo-dev gettext
gfortran libx11-dev libnode-dev
```

On macOS, install GDAL/GEOS/PROJ/UDUNITS and a Fortran compiler, for example with Homebrew:

```sh
brew install gcc udunits gettext jq gdal
```

## Reproducing the Main DAC Analysis

The most complete reproduction path is:

1. Restore the R environment.
2. Ensure the CHIS teen files for 2021-2023 are available.
3. Ensure `Data/auxiliary_data.rds` exists.
4. Set the CHIS file type.
5. Run the main analysis.
6. Build final tables and figures from the generated outputs.

Example for Stata inputs:

```sh
export CHIS_DATA_TYPE=stata
Rscript R/CHIS_private.R
Rscript R/localCode/Combine_Remote_Results.R
```

Example for SAS inputs:

```sh
export CHIS_DATA_TYPE=sas
Rscript R/CHIS_private.R
Rscript R/localCode/Combine_Remote_Results.R
```

If the CHIS files are not in the default dummy-data locations, edit the file path block near the top of `R/CHIS_private.R` before running.

## Reproducing the Public-Use-File Analysis

The PUF analysis is less complete than the DAC workflow because it excludes confidential variables and uses the public-use CHIS files. To rerun it:

```sh
Rscript data-raw/CHIS_PUF_clean.R
Rscript R/localCode/CHIS_PUF.R
```

`R/localCode/CHIS_PUF.R` reads `Data/chis_puf_combined.Rds`, creates a CHIS replicate-weight survey design, builds a demographics table, and fits a weighted quasibinomial model for climate anxiety.

## Main Outputs

`R/CHIS_private.R` writes core analysis outputs to `Outputs/`, including:

```text
table1_basic.csv
table1.csv
table1_display.csv
table1_raw_basic.csv
table1_raw.csv
table1_raw_display.csv
climiate_anxiety_map_data.csv
heatwave_map.pdf
tmax_map.pdf
glm_model_summary.csv
glm_vcov.csv
glm_suf_stat.csv
fe_model_summary.csv
fe_vcov.csv
```

`R/localCode/Combine_Remote_Results.R` reads those files and writes manuscript-facing artifacts to `Documents/` and `Figures/`, including:

```text
Documents/table_1.docx
Documents/raw_table_1_appendix.docx
Documents/glm_tab.docx
Figures/forest_plots.pdf
Figures/heatwave_map2.pdf
Figures/tmax_map2.pdf
Figures/maleVfemale.pdf
Figures/missed_school.pdf
```

## Testing

The test suite is built around `testthat`:

```sh
Rscript tests/testthat.R
```

Important behavior:

- On R `4.2.3`, tests run the DAC scripts in `R/`.
- On other R versions, tests run scripts under `R/localCode/`.
- `tests/testthat/test-z-check-downloads.R` checks package versions against the DAC package/version reference in `docs/dac-r-and-stata-packages-list-2.xlsx` when running under R `4.2.3`.

The GitHub Actions workflows in `.github/workflows/` document CI setup for local, DAC, and output-generation runs.

## Analysis Summary

The main confidential workflow performs these steps:

1. Loads CHIS teen data for 2021, 2022, and 2023.
2. Harmonizes variable names and labels across years.
3. Pools the annual CHIS files and adjusts replicate weights across pooled years.
4. Recodes demographic, access-to-care, civic-engagement, school, and climate-anxiety variables.
5. Joins PRISM climate measures by census tract and county.
6. Builds a replicate-weight survey design with `survey::svrepdesign()`.
7. Produces weighted Table 1 outputs by year and climate anxiety status.
8. Applies suppression rules for display tables.
9. Produces county/census-tract climate anxiety and temperature/heatwave maps.
10. Fits survey-weighted quasibinomial models for climate anxiety.
11. Saves coefficient tables, variance-covariance matrices, sufficient statistics for contrasts, and final manuscript figures/tables.

## Known Reproducibility Notes

- Some scripts refer to `Data/` and others to `data/`. On case-insensitive macOS filesystems this may still work, but on Linux or other case-sensitive filesystems paths should be standardized before rerunning from scratch.
- `Data/auxiliary_data.rds` is required by the main DAC workflow. `data-raw/create_auxiliary data.R` saves `auxillary_data.rds` with a different spelling, so verify the final filename before running `R/CHIS_private.R`.
- The file `Outputs/climiate_anxiety_map_data.csv` is intentionally listed with the current repository spelling because downstream code expects that name.
- `R/CHIS_private.R` contains a path-editing section marked with `#***`; review it before running in the DAC environment.
- Recreating PRISM-derived auxiliary files can be slow and requires network access to PRISM data.
- The repository currently contains generated outputs in `Outputs/`, `Documents/`, and `Figures/`. These are useful for checking expected results but should be regenerated after changes to data, cleaning, or models.

## Recommended Reproduction Checklist

For a new analyst trying to recreate the analysis:

1. Open `CHIS.Rproj` or set the repository root as the working directory.
2. Restore packages with `renv::restore()`.
3. Confirm R version `4.2.3` if reproducing the DAC environment.
4. Place approved CHIS 2021-2023 teen files in a known secure location.
5. Confirm or recreate `Data/auxiliary_data.rds`.
6. Set `CHIS_DATA_TYPE` to `stata` or `sas`.
7. Run `R/CHIS_private.R`.
8. Run `R/localCode/Combine_Remote_Results.R`.
9. Compare regenerated files in `Outputs/`, `Documents/`, and `Figures/` to the committed/generated outputs.
10. Run `Rscript tests/testthat.R` as a final check.
