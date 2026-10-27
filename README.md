# CHIS Climate Anxiety Analysis

This repository contains R code and a Quarto manuscript for analyses of climate anxiety among California adolescents using the California Health Interview Survey (CHIS) teen files for 2021, 2022, and 2023. The analysis combines CHIS survey data with county and census-tract climate/geography data, applies CHIS replicate survey weights, and produces descriptive tables, maps, regression outputs, and the manuscript, supplement, and tables documents.

The main goal of this README is to document how to recreate the analysis as closely as possible from the repository contents and the required source data.

## Workflow Overview

The analysis runs in two places:

1. **CHIS Data Access Center (DAC).** `R/CHIS_private.R` runs on the confidential teen files inside the DAC and writes summary outputs (tables, coefficients, variance-covariance matrices, sufficient statistics) to `Outputs/`. Only these outputs leave the DAC.
2. **Local machine.** Scripts in `R/localCode/` read the released DAC outputs in `Outputs_DAC/` (tracked in git), the CHIS public-use files (PUF), and auxiliary climate/population data, and write manuscript tables and figures to `Documents/` and `Figures/`. The Quarto documents in `Documents/Manuscript/` then read those files.

```text
CHIS teen files (DAC) ──> R/CHIS_private.R ──> (DAC release) ──> Outputs_DAC/*.csv
                                                   │
data/auxiliary_data.rds ───────────────────────────┤
                                                   v
                              R/localCode/Combine_Remote_Results.R ──┐
data/chis_puf_combined.Rds ──> R/localCode/CHIS_PUF.R ───────────────┤──> Documents/*.rds, Figures/
data/tmax_annual_ca_youth.rds ──> R/localCode/testing_change.R ──────┘
                                                                      v
                                          Documents/Manuscript/*.qmd ──> manuscript, supplement, tables
```

## Repository Structure

```text
.
├── R/
│   ├── CHIS_private.R                 # Main DAC/confidential-data analysis script (requires R 4.2.3)
│   ├── Functions.R                    # GENERATED shared functions; do not edit directly (see below)
│   └── localCode/
│       ├── BaseFileFunctions.R        # Source of truth for shared cleaning, modeling, table, and plotting functions
│       ├── Combine_Remote_Results.R   # Builds manuscript tables/figures from DAC outputs in Outputs_DAC/
│       ├── CHIS_PUF.R                 # Public-use-file analyses (prevalence, sensitivity analyses, secondary outcomes)
│       └── testing_change.R           # Post hoc decomposition of the 2022–2023 change in climate anxiety
├── data-raw/
│   ├── CHIS_PUF_clean.R               # Creates combined PUF RDS from CHIS teen .dta files
│   ├── approved_var_select.R          # Creates approved-variable list (data/teen_approved_vars.rds)
│   ├── chis_stata_labels_for_sas.R    # Builds Stata value-label lookups used when SAS inputs are supplied
│   ├── create_Functions_file.R        # Generates R/Functions.R from BaseFileFunctions.R plus label lookups
│   ├── set_package_versions.R         # Pins package versions to the DAC package list
│   ├── Census_2010_clean.R            # Downloads/cleans 2010 census tract geometries
│   ├── county_shapefile_clean.R       # Cleans county shapefile
│   ├── prism_download.R               # Downloads/processes PRISM climate data
│   ├── create_auxiliary data.R        # Bundles climate/geography support data
│   └── get_adolescent_pop.R           # ACS adolescent population and population-weighted annual Tmax
├── data/                              # Input data, dummy CHIS files, and precomputed RDS files
├── Outputs_DAC/                       # Released DAC outputs read by the local scripts (tracked)
├── Outputs/                           # DAC script output inside the DAC; local intermediate outputs (git-ignored)
├── Documents/                         # Manuscript-ready tables and table objects (mostly git-ignored)
│   └── Manuscript/                    # Quarto manuscript, supplement, tables, bibliography, styles
├── Figures/                           # Manuscript-ready figures (git-ignored)
├── tests/testthat/                    # Script-run, Table 1 consistency, and package/version checks
├── docs/                              # DAC package/version reference files
├── renv.lock                          # R 4.2.3 lockfile (DAC environment)
├── renv/profiles/local/renv.lock      # R 4.6.1 lockfile (local environment)
└── CHIS.Rproj                         # RStudio project file
```

### `R/Functions.R` is generated

`R/Functions.R` is the file every script sources, but it is built by `data-raw/create_Functions_file.R`, which copies `R/localCode/BaseFileFunctions.R` over it and appends the SAS-to-Stata value-label lookups (built from the dummy Stata files in `data/` by `data-raw/chis_stata_labels_for_sas.R`). The lookups are embedded so the DAC run needs only `R/CHIS_private.R`, `R/Functions.R`, and `data/auxiliary_data.rds`, not the dummy files.

To change a shared function:

1. Edit `R/localCode/BaseFileFunctions.R`.
2. Regenerate the shared file:

   ```sh
   Rscript data-raw/create_Functions_file.R
   ```

Edits made directly to `R/Functions.R` are overwritten the next time it is regenerated.

## Data Requirements

The analysis is not fully reproducible from public code alone because it depends on CHIS teen data files and precomputed auxiliary climate/geography data. To rerun the full analysis, the following files are expected.

### CHIS Teen Files (DAC)

For the confidential/DAC workflow, provide CHIS teen data for 2021, 2022, and 2023.

`R/CHIS_private.R` can read either Stata (`.dta`) or SAS (`.sas7bdat`) inputs through `haven`. Set the `CHIS_DATA_TYPE` environment variable to `stata` or `sas`, or edit `stata_or_sas` directly. If neither is set, the script tries to detect the type from the file names. Set the `file_name_2021`, `file_name_2022`, and `file_name_2023` variables in the path block near the top of the script; the lines to edit are marked with `#***`.

The repository includes dummy Stata and SAS files under `data/` so the script can be run end to end outside the DAC. A real DAC run should point to the approved CHIS files supplied in the secure environment.

### Auxiliary Climate and Geography Data

Both `R/CHIS_private.R` and `R/localCode/Combine_Remote_Results.R` expect:

```text
data/auxiliary_data.rds
```

This file should contain:

- PRISM-derived climate variables by census tract and county
- California census tract geometry
- California county geometry
- map-ready census temperature and heatwave summaries

The scripts used to build it are in `data-raw/`:

1. `data-raw/Census_2010_clean.R`
2. `data-raw/county_shapefile_clean.R`
3. `data-raw/prism_download.R`
4. `data-raw/create_auxiliary data.R`

Recreating the auxiliary data requires downloading external census/TIGER and PRISM data and may take substantial time.

### Adolescent Population and Annual Temperature

`R/localCode/testing_change.R` expects:

```text
data/tmax_annual_ca_youth.rds
```

Create it with:

```sh
Rscript data-raw/get_adolescent_pop.R
```

This script downloads ACS 5-year tract estimates of the population aged 12–17 for 2021–2023 with `tidycensus`, maps 2020 tracts to 2010 tracts with the Census Bureau relationship file, and combines the result with the PRISM data in `data/auxiliary_data.rds` to compute adolescent-population-weighted annual maximum temperature. It requires network access and a Census API key (`tidycensus::census_api_key()`).

### Public-Use-File Combined Data

`R/localCode/CHIS_PUF.R` and `R/localCode/testing_change.R` expect:

```text
data/chis_puf_combined.Rds
```

This can be recreated from the CHIS teen public-use Stata files with:

```sh
Rscript data-raw/CHIS_PUF_clean.R
```

That script reads:

```text
data/teen_stata_2021/TEEN.dta
data/teen_stata_2022/TEEN.dta
data/teen_2023_stata/TEEN.dta
```

The PUF files are git-ignored and must be downloaded from CHIS.

## Environment Setup

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

`R/CHIS_private.R` stops unless it is run under R 4.2.3.

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

Rendering the manuscript additionally requires [Quarto](https://quarto.org) and, for PDF output, a LaTeX distribution with `lualatex` (for example TinyTeX via `quarto install tinytex`).

## Step 1: Run the DAC Analysis

Under R 4.2.3, with the CHIS teen files and `data/auxiliary_data.rds` in place:

```sh
export CHIS_DATA_TYPE=stata   # or sas
Rscript R/CHIS_private.R
```

`R/CHIS_private.R` writes these files to `Outputs/`:

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

After DAC release, replace the contents of `Outputs_DAC/` with the released files and commit them, so the local steps can be rerun from a fresh clone. `Outputs_DAC/` holds only the most recent release; earlier releases are recoverable from git history. Update `Outputs_DAC/RELEASE.md` with the release details and checksums.

## Step 2: Build Tables and Figures Locally

Under the local R environment, run:

```sh
Rscript data-raw/CHIS_PUF_clean.R        # if data/chis_puf_combined.Rds is missing
Rscript data-raw/get_adolescent_pop.R    # if data/tmax_annual_ca_youth.rds is missing
Rscript R/localCode/Combine_Remote_Results.R
Rscript R/localCode/CHIS_PUF.R
Rscript R/localCode/testing_change.R
```

Each local script checks for its inputs and skips with a message if they are missing.

### `Combine_Remote_Results.R`

Reads the DAC outputs and `data/auxiliary_data.rds`, checks that Table 1 counts and percentages are internally consistent, and writes:

- Table 1 (row percentages), a column-percentage alternative, the unweighted raw-count appendix table, and the baseline appendix table, each as `.docx`, `.tex`, `.html`, and flextable/gt/notes `.rds` objects (`Documents/table_1*`, `Documents/table_1_column*`, `Documents/raw_table_1_appendix*`, `Documents/baseline_table_appendix*`, `Documents/baseline_table_gt_appendix*`)
- the main regression table (`Documents/glm_tab.docx`, `.html`, `.tex`, `glm_tab_gt.rds`, `glm_tab_ft.rds`, `glm_tab_notes.rds`) and log-odds-ratio table (`glm_lor_table.rds`)
- joint tests for the climate variables (`Documents/climate_vars_pvalue.rds`) and the fixed-effects model table (`Documents/fe_tab.rds`)
- a combined Word file of tables (`Documents/tables_lancet.docx`)
- `Outputs/table1_display_raw.rds`
- `Figures/forest_plots.pdf`/`.png`, `Figures/heatwave_map2.pdf`/`.png`, and `Figures/tmax_map2.pdf`/`.png`

### `CHIS_PUF.R`

Reads `data/chis_puf_combined.Rds`, builds a CHIS replicate-weight survey design, and writes:

- `Outputs/demographics.tex` and `Outputs/vulnerable_subgroups_model_table.csv`
- overall and yearly prevalence of climate anxiety and tests of change between years (`Documents/prevalence_overall.rds`, `prevalence.rds`, `prev_sig_tests.rds`)
- `Figures/prevalence.pdf`/`.png`
- year sensitivity analyses (`Documents/year_sens.rds`, `year_sens_change.rds`)
- secondary outcomes (`Documents/cares_about_community.rds`, `psych_distress.rds`)

### `testing_change.R`

Combines the PUF, `data/tmax_annual_ca_youth.rds`, and the DAC model outputs (`table1.csv`, `glm_model_summary.csv`, `glm_vcov.csv`, `glm_suf_stat.csv`). For each covariate, it estimates how much of the 2022–2023 change in predicted climate-anxiety prevalence is explained by shifting that covariate's distribution from 2022 to 2023 while holding the others fixed, with confidence intervals from draws of the model coefficients. It writes `Documents/post_hoc_change.rds` and `Documents/post_hoc_change_table.rds`.

## Step 3: Render the Manuscript

The Quarto documents are in `Documents/Manuscript/`:

| File | Contents | Formats |
|---|---|---|
| `chis_manuscript.qmd` | Main manuscript | docx, html, pdf |
| `chis_supplement.qmd` | Supplementary material | pdf, docx |
| `chis_tables.qmd` | Main tables only (`_tbl-baseline.qmd`, `_tbl-glm.qmd`) | docx |

Shared pieces:

- `_setup.qmd`: loads packages, sources `R/Functions.R`, and sets Lancet-style number formatting (midline decimals)
- `_format.yml`: shared Word (`lancet-reference.docx`) and PDF (`lualatex`, `_preamble.tex`) settings
- `CHIS.bib` with the Lancet CSL style
- `_extensions/kapsner/authors-block`: the author/affiliation block filter

The documents read the `.rds` files from `Documents/` produced in Step 2, so Step 2 must be run first. Render from the manuscript folder:

```sh
cd Documents/Manuscript
quarto render chis_manuscript.qmd
quarto render chis_supplement.qmd
quarto render chis_tables.qmd
```

Quarto runs R from `Documents/Manuscript/`, so the project's root `.Rprofile` (and therefore renv) is not loaded automatically. If packages appear to be missing, point `R_PROFILE_USER` at a profile that sets `RENV_PROJECT` to the repository root, sets `RENV_PROFILE="local"`, and sources `<root>/renv/activate.R`.

Rendered documents are git-ignored. Do not use `quarto render --output-dir`, which moves the rendered files out of `Documents/Manuscript/` and can overwrite existing drafts there.

## Testing

The test suite is built around `testthat`:

```sh
Rscript tests/testthat.R
```

- `test-run-code.R`: on R 4.2.3, sources every script in `R/` (the DAC scripts). On other R versions, sources every script in `R/localCode/` in alphabetical order. Local scripts whose inputs are missing skip with a message instead of failing.
- `test-table1-sums.R`: checks the Table 1 consistency functions (`check_table1_long_sums()`, `check_table1_sums()`) on small fixtures.
- `test-z-check-downloads.R`: under R 4.2.3, checks installed package versions against the DAC package/version reference in `docs/dac-r-and-stata-packages-list-2.xlsx`.

The GitHub Actions workflows in `.github/workflows/` run the DAC (R 4.2.3) and local setups and can save generated outputs as artifacts:

- `run-dac-r-code.yml`, `run-r-code.yml`: restore renv and run the tests
- `generate-dac-r-output.yml`, `generate-r-output.yml`, `generate-local-r-output.yml`: run the scripts and upload outputs

## Analysis Summary

The main confidential workflow (`R/CHIS_private.R`) performs these steps:

1. Loads CHIS teen data for 2021, 2022, and 2023.
2. Harmonizes variable names and labels across years (including SAS/Stata label differences).
3. Pools the annual CHIS files and adjusts replicate weights across pooled years.
4. Recodes demographic, access-to-care, civic-engagement, school, and climate-anxiety variables.
5. Joins PRISM climate measures by census tract and county.
6. Builds a replicate-weight survey design with `survey::svrepdesign()`.
7. Produces weighted and unweighted Table 1 outputs by year and climate anxiety status.
8. Applies suppression rules for display tables.
9. Produces county/census-tract climate anxiety and temperature/heatwave maps.
10. Fits survey-weighted quasibinomial models for climate anxiety, plus fixed-effects and mixed-effects models (currently deferred).
11. Saves coefficient tables, variance-covariance matrices, and sufficient statistics for contrasts.

The local workflow then builds manuscript tables and figures, adds PUF-based prevalence, sensitivity, and secondary-outcome analyses, decomposes the 2022–2023 change, and renders the manuscript documents.

## Known Reproducibility Notes

- `data-raw/create_auxiliary data.R` saves `data/auxiliary_data.rds` (lowercase folder).
- The file `Outputs_DAC/climiate_anxiety_map_data.csv` is intentionally listed with the current repository spelling because downstream code expects that name.
- `R/CHIS_private.R` contains path-editing lines marked with `#***`; review them before running in the DAC environment.
- Recreating PRISM-derived auxiliary files can be slow and requires network access to PRISM data. `get_adolescent_pop.R` requires network access and a Census API key.
- `Outputs/`, `Figures/`, most of `Documents/`, rendered manuscript files, and the CHIS PUF files are git-ignored. A fresh clone does not contain the generated outputs; they must be regenerated before running Step 3. The released DAC outputs in `Outputs_DAC/` are tracked, so Step 2 does not require DAC access.
- `R/test/` is git-ignored scratch code and is not part of the analysis.

## Recommended Reproduction Checklist

For a new analyst trying to recreate the analysis:

1. Open `CHIS.Rproj` or set the repository root as the working directory.
2. Restore packages with `renv::restore()` under the appropriate R version (4.2.3 for DAC, 4.6.1 locally).
3. If shared functions changed, regenerate `R/Functions.R` with `Rscript data-raw/create_Functions_file.R`.
4. Confirm or recreate `data/auxiliary_data.rds`.
5. In the DAC: place the approved CHIS 2021-2023 teen files, set `CHIS_DATA_TYPE`, and run `R/CHIS_private.R`.
6. Replace `Outputs_DAC/` with the released DAC outputs (or use the tracked release to skip steps 4–5).
7. Locally: create `data/chis_puf_combined.Rds` and `data/tmax_annual_ca_youth.rds` if needed.
8. Run `R/localCode/Combine_Remote_Results.R`, `R/localCode/CHIS_PUF.R`, and `R/localCode/testing_change.R`.
9. Render `chis_manuscript.qmd`, `chis_supplement.qmd`, and `chis_tables.qmd` from `Documents/Manuscript/`.
10. Run `Rscript tests/testthat.R` as a final check.
