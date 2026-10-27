library(tidycensus)
library(dplyr)
library(sf)
library(tidyr)
library(purrr)
library(readr)
library(stringr)

# ============================================================
# 1. Download California ACS data, 2021–2023
# ============================================================

if (!file.exists(here::here("data-raw", "ca_youth_2010.rds"))) {
  vars <- c(
    total_pop = "B01001_001",
    age12_14 = "B09001_008",
    age15_17 = "B09001_009"
  )

  ca_youth_2020 <- map_dfr(2021:2023, function(yr) {
    get_acs(
      geography = "tract",
      state = "CA",
      year = yr,
      survey = "acs5",
      variables = vars,
      geometry = FALSE,
      cache_table = TRUE
    ) |>
      select(GEOID, variable, estimate) |>
      pivot_wider(
        names_from = variable,
        values_from = estimate
      ) |>
      transmute(
        tract20 = GEOID,
        year = yr,
        total_pop = total_pop,
        pop12_17 = age12_14 + age15_17
      )
  })

  # ============================================================
  # 2. Download Census Bureau 2020 -> 2010 tract crosswalk
  # ============================================================

  crosswalk_url <- paste0(
    "https://www2.census.gov/geo/docs/maps-data/",
    "data/rel2020/tract/",
    "tab20_tract20_tract10_st06.txt"
  )

  rel <- read_delim(
    crosswalk_url,
    delim = "|",
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )

  # ============================================================
  # 3. Calculate land-area allocation weights
  # ============================================================

  # Each 2020 tract may overlap multiple 2010 tracts.
  # Allocate using share of overlapping land area.

  xwalk <- rel |>
    transmute(
      tract20 = GEOID_TRACT_20,
      tract10 = GEOID_TRACT_10,
      overlap_area = as.numeric(AREALAND_PART)
    ) |>
    filter(
      !is.na(tract20),
      !is.na(tract10),
      !is.na(overlap_area),
      overlap_area >= 0
    ) |>
    group_by(tract20, tract10) |>
    summarise(
      overlap_area = sum(overlap_area),
      .groups = "drop"
    ) |>
    group_by(tract20) |>
    filter(sum(overlap_area) > 0) |>
    mutate(
      wt_pop = overlap_area / sum(overlap_area)
    ) |>
    ungroup()

  xwalk |>
    group_by(tract20) |>
    summarise(
      weight_sum = sum(wt_pop),
      .groups = "drop"
    ) |>
    summarise(
      n_tracts = n(),
      min_weight_sum = min(weight_sum, na.rm = TRUE),
      max_weight_sum = max(weight_sum, na.rm = TRUE)
    )

  # Validate allocation weights
  weight_check <- xwalk |>
    group_by(tract20) |>
    summarise(
      weight_sum = sum(wt_pop),
      .groups = "drop"
    )

  stopifnot(
    all(abs(weight_check$weight_sum - 1) < 1e-6, na.rm = TRUE)
  )

  # Report any zero-land-area source tracts
  print(
    weight_check |>
      filter(
        is.na(weight_sum) |
          abs(weight_sum - 1) >= 1e-6
      ),
    n = 1000
  )

  # ============================================================
  # 4. Convert ACS counts to 2010 census tract geography
  # ============================================================

  unmatched <- ca_youth_2020 |>
    anti_join(xwalk, by = "tract20")

  message("Unmatched ACS tract-year records: ", nrow(unmatched))

  ca_youth_2010 <- ca_youth_2020 |>
    left_join(
      xwalk,
      by = "tract20",
      relationship = "many-to-many"
    ) |>
    mutate(
      total_alloc = total_pop * wt_pop,
      youth_alloc = pop12_17 * wt_pop
    ) |>
    filter(!is.na(tract10)) |>
    group_by(tract10, year) |>
    summarise(
      total_pop = if (all(is.na(total_alloc))) {
        NA_real_
      } else {
        sum(total_alloc, na.rm = TRUE)
      },
      pop12_17 = if (all(is.na(youth_alloc))) {
        NA_real_
      } else {
        sum(youth_alloc, na.rm = TRUE)
      },
      .groups = "drop"
    ) |>
    mutate(
      pct12_17 = if_else(
        total_pop > 0,
        100 * pop12_17 / total_pop,
        NA_real_
      )
    ) |>
    arrange(tract10, year)

  # ============================================================
  # 5. Validate resulting data
  # ============================================================

  stopifnot(
    !anyDuplicated(ca_youth_2010[c("tract10", "year")])
  )

  stopifnot(
    all(
      is.na(ca_youth_2010$pct12_17) |
        between(ca_youth_2010$pct12_17, 0, 100)
    )
  )

  ca_youth_2010 |>
    group_by(year) |>
    summarise(
      n_tracts = n(),
      n_missing = sum(is.na(pct12_17)),
      mean_pct = mean(pct12_17, na.rm = TRUE),
      weighted_pct = 100 *
        sum(pop12_17, na.rm = TRUE) /
        sum(total_pop, na.rm = TRUE),
      .groups = "drop"
    ) |>
    print()

  saveRDS(ca_youth_2010, file = here::here("data-raw", "ca_youth_2010.rds"))
} else {
  ca_youth_2010 <- readRDS(file = here::here("data-raw", "ca_youth_2010.rds"))
}

# ============================================================
# 6. Aggregate to county level
# ============================================================
ca_youth_county <- ca_youth_2010 |>
  mutate(
    county_fips = str_sub(tract10, 1, 5)
  ) |>
  group_by(county_fips, year) |>
  summarise(
    total_pop = sum(total_pop, na.rm = TRUE),
    pop12_17 = sum(pop12_17, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    pct12_17 = if_else(
      total_pop > 0,
      100 * pop12_17 / total_pop,
      NA_real_
    )
  )
county_names <- tidycensus::fips_codes |>
  filter(state == "CA") |>
  transmute(
    county_fips = paste0(state_code, county_code),
    county = county
  )

ca_youth_county <- ca_youth_county |>
  left_join(county_names, by = "county_fips") |>
  relocate(county, .after = county_fips)


# ============================================================
# 7. Merge with existing census_heatwave data
# ============================================================
aux_data <- readRDS(here::here("Data", "auxiliary_data.rds"))

tract_vars <- c(
  "tmax_tract10_prior_90_days_count32_delta",
  "tmax_tract10_prior_90_days_mean_delta",
  "tmax_tract10_prior_yr_mean_delta",
  "tmax_tract10_prior_yr_count32_delta"
)
county_vars <- sub("tract10", "county", tract_vars, fixed = TRUE)

tract_annual <- aux_data$prism$census |>
  left_join(
    ca_youth_2010 |> select(tract10, year, pop12_17),
    by = c("tract10", "year"),
    relationship = "many-to-one"
  ) |>
  group_by(year, tract10, pop12_17) |>
  summarise(
    across(all_of(tract_vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  group_by(year) |>
  summarise(
    across(
      all_of(tract_vars),
      ~ weighted.mean(.x, w = coalesce(pop12_17, 0), na.rm = TRUE)
    ),
    .groups = "drop"
  )

county_annual <- aux_data$prism$county |>
  left_join(
    ca_youth_county |>
      mutate(county = str_remove(county, " County$")) |>
      select(county, year, pop12_17),
    by = c("county", "year"),
    relationship = "many-to-one"
  ) |>
  group_by(year, county, pop12_17) |>
  summarise(
    across(all_of(county_vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) |>
  group_by(year) |>
  summarise(
    across(
      all_of(county_vars),
      ~ weighted.mean(.x, w = pop12_17, na.rm = TRUE)
    ),
    .groups = "drop"
  )

tmax_annual_ca <- tract_annual |>
  full_join(county_annual, by = "year") |>
  arrange(year)

saveRDS(tmax_annual_ca, here::here("Data", "tmax_annual_ca_youth.rds"))
