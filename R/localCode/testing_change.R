library(dplyr) # data manipulation
library(survey) # Survey data analysis
library(gtsummary) # for summary tables
library(stringr)
library(here)
library(broom)
library(ggplot2)
library(scales)
library(emmeans)

source(here::here("R", "Functions.R"))
puf_file <- here::here("data", "chis_puf_combined.Rds")

if (!file.exists(puf_file)) {
  message(
    "Skipping CHIS_PUF.R because the CHIS public-use file is missing: ",
    puf_file,
    ". See README for how to obtain it."
  )
} else {
  chis <- readRDS(here::here("data", "chis_puf_combined.Rds"))

  # aux_data <- readRDS(here::here("data", "auxiliary_data.rds"))

  if (
    file.exists(tmax_file <- here::here("data", "tmax_annual_ca_youth.rds"))
  ) {
    tmax_annual_ca <- readRDS(tmax_file)
  } else {
    stop("You must run `get_adolescent_pop.R` in the 'data-raw/' folder first")
  }

  # extract covariate values from baseline table
  tab1_long <- utils::read.csv(here::here("Outputs_DAC", "table1.csv")) |>
    mutate(variable = if_else(variable == "Age*", "Age", variable)) |>
    filter(status != "Total") |>
    group_by(variable, label, year) |>
    summarize(
      sd = pooled_sd(N, mean, sd),
      mean = pooled_mean(N, mean),
      N = sum(N),
      N_upper = sum(N_upper),
      total_N = sum(total_N)
    ) |>
    mutate(
      perc = N / total_N * 100,
      status = "Yes",
      suppress_final = FALSE,
      suppress_primary = FALSE,
      bound = FALSE,
      N_display = N,
      total_N_display = total_N
    ) |>
    ungroup()

  suf_stat <- read.csv(here::here("Outputs_DAC", "glm_suf_stat.csv"))

  #coef
  glm_est <- utils::read.csv(
    file = here::here("Outputs_DAC", "glm_model_summary.csv")
  )
  beta <- glm_est$Estimate

  # vcov for predictions
  V <- utils::read.csv(
    file = here::here("Outputs_DAC", "glm_vcov.csv"),
    row.names = 1
  )
  #### Clean and combine CHIS data ####
  library(dplyr)
  library(sf)

  # Variables
  tract_vars <- c(
    "tmax_tract10_prior_90_days_count32_delta",
    "tmax_tract10_prior_90_days_mean_delta",
    "tmax_tract10_prior_yr_mean_delta",
    "tmax_tract10_prior_yr_count32_delta"
  )

  county_vars <- c(
    "tmax_county_prior_90_days_count32_delta",
    "tmax_county_prior_90_days_mean_delta",
    "tmax_county_prior_yr_mean_delta",
    "tmax_county_prior_yr_count32_delta"
  )

  #### Vars to recreate matrices ####

  baseline_demographics <- c(
    "age_group", # SELF-REPORTED AGE
    "srsex", # SELF-REPORTED GENDER
    "ombsrreo", # OMB/CURRENT DOF RACE - ETHNICITY
    "sch_typ", # TYPE OF SCHOOL ATTENDED
    "ahedtc_binary", # ADULT EDUCATIONAL ATTAINMENT
    "povll_binary", # POVERTY LEVEL
    "lnghmt_binary", # LANGUAGE SPOKEN AT HOME,
    "ur_clrt2" # URBAN/RURAL CLASSIFICATION
  )

  modifiable_protective <- c(
    "tl25_pos", # CARES DEEPLY ABOUT ISSUES IN COMMUNITY strong agree -> strong dis
    "tl27_pos", # BELIEVES CAN MAKE A DIFFERENCE IN THE COMMUNITY strong agree -> strong dis
    "tl50", # EVER VOLUNTEERED TO SOLVE PROBLEM IN THE COMMUNITY Yes, No
    "tl53_pos", # CONFIDENCE TO CONTACT SOMEONE IN THE GOVT WHO REPRESENTS COMMUNITY Definitely Could -> Definitely Could Not
    "tq10_pos", # HOW OFTEN FELT ABLE TO TALK TO FAMILY ABOUT FEELINGS All Of The Time -> Never
    "tq11_pos", # HOW OFTEN FELT FAMILY STOOD BY YOU DURING DIFFICULT TIMES All Of The Time -> Never
    "tq14_pos", # HOW OFTEN FELT SUPPORTED BY FRIENDS  All Of The Time -> Never
    "tq16_pos" # HOW OFTEN ENJOYED PARTICIPATING IN COMMUNITY TRADITIONS  All Of The Time -> Never
  )

  access_to_care <- c(
    "uninsured", # INSURANCE TYPE, Yes, No
    "health_office", # KIND OF PLACE MOST OFTEN GO FOR HEALTH CARE
    "tf9" # DELAYED/DID NOT GET MEDICAL CARE FELT NEEDED IN PAST 12 MOS, Yes, No
  )

  civic_engagement <- c(
    "school_last_week", # ATTENDED SCHOOL LAST WEEK Yes, No
    # "ta4c_p1",         # ATTENDED SCHOOL DURING LAST SCHOOL YR, only in 2023 data
    "health_missed_days", # Number OF DAYS OF SCHOOL MISSED FOR HEALTH PROBLEM PAST MO;
    # there's also the fact that for 2022 and 2021, it was not asked if TA4 is "no"
    # so 2023 was coded the same in our analysis to make results comparable
    "tl10", # PARTICIPATE IN CLUBS/ORGS OUTSIDE SCHOOL PAST YR, Yes, No
    "tq15_pos" # HOW OFTEN FELT SENSE OF BELONGING AT SCHOOL All Of The Time -> Never
  )

  climate_variables <- c(
    "tmax_tract10_prior_90_days_count32_delta", # Tmax above 32 for the 90 days prior to survey date
    "tmax_tract10_prior_90_days_mean_delta", # Mean Tmaxfor the 90 days prior to survey date
    "tmax_tract10_prior_yr_mean_delta", # Mean Tmax for the year prior to survey date
    "tmax_tract10_prior_yr_count32_delta", # Count of Tmax above 32 for the year prior to survey date
    "tmax_county_prior_90_days_count32_delta", # Tmax above 32 for the 90 days prior to survey date
    "tmax_county_prior_90_days_mean_delta", # Mean Tmaxfor the 90 days prior to survey date
    "tmax_county_prior_yr_mean_delta", # Mean Tmax for the year prior to survey date
    "tmax_county_prior_yr_count32_delta" # Count of Tmax above 32 for the year prior to survey date
  )

  # Aggregate all the variables
  characteristics <- c(
    baseline_demographics,
    modifiable_protective,
    access_to_care,
    civic_engagement,
    climate_variables
  )

  #### Joins ####
  chis <- chis |>
    left_join(tmax_annual_ca, by = "year")

  # optional check: every row should have the climate values
  stopifnot(!anyNA(chis[climate_variables]))

  to_replace <- !(characteristics %in% colnames(chis))
  char_to_replace <- characteristics[to_replace]

  chis <- chis |>
    mutate(
      !!!setNames(
        purrr::map_dbl(char_to_replace, \(x) {
          suf_stat$mean[which(startsWith(suf_stat$names, x))[1]]
        }),
        char_to_replace
      )
    )

  # formula
  formula <- as.formula(paste0(
    "I(tf45 == 'Yes') ~ ",
    paste0(characteristics, collapse = " + ")
  ))

  mm22 <- reconstruct_mm(
    formula,
    chis |> filter(year == 2022),
    replace = char_to_replace,
    suf_stat,
    tab1 = tab1_long
  ) |>
    as_tibble() |>
    select(all_of(suf_stat$names)) |>
    as.matrix()

  mm23 <- reconstruct_mm(
    formula,
    chis |> filter(year == 2023),
    replace = char_to_replace,
    suf_stat,
    tab1 = tab1_long
  ) |>
    as_tibble() |>
    select(all_of(suf_stat$names)) |>
    as.matrix()

  w23 <- chis |> filter(year == 2023) |> pull(fnwgt0)
  w22 <- chis |> filter(year == 2022) |> pull(fnwgt0)

  # cbind(
  #   beta,
  #   matrixStats::colWeightedMeans(mm22, w22),
  #   matrixStats::colWeightedMeans(mm23, w23)
  # ) |>
  #   `rownames<-`(glm_est$X) |>
  #   (\(x) {
  #     x[glm_est$X[order(abs(beta), decreasing = TRUE)][1:10], , drop = FALSE]
  #   })()

  #### Grouped one-at-a-time counterfactual ####
  x22 <- matrixStats::colWeightedMeans(mm22, w22)
  x23 <- matrixStats::colWeightedMeans(mm23, w23)
  coef_names <- glm_est$X
  # beta is in glm_est order and mm is in suf_stat order; they must line up
  stopifnot(identical(coef_names, colnames(mm22)))

  # map each model column to the covariate that created it, using the longest
  # prefix match so e.g. "ombsrreoWhite" -> "ombsrreo"
  term_group <- vapply(
    coef_names,
    \(nm) {
      hits <- characteristics[startsWith(nm, characteristics)]
      if (length(hits) == 0) nm else hits[which.max(nchar(hits))]
    },
    character(1),
    USE.NAMES = FALSE
  )

  groups <- setdiff(unique(term_group), "(Intercept)") # stays in GLM order
  base_p <- c(plogis(x22 %*% beta))

  # shift all columns belonging to one covariate together
  contrib <- vapply(
    groups,
    \(g) {
      x <- x22
      idx <- term_group == g
      x[idx] <- x23[idx]
      c(plogis(x %*% beta)) - base_p
    },
    numeric(1)
  )

  # "Change" column: one number for single-column terms; level-by-level for factors
  change_txt <- tibble::tibble(
    term = coef_names,
    group = term_group,
    delta = x23 - x22
  ) |>
    mutate(
      delta = if_else(
        group %in% c("tl50", "lnghmt_binary", "tl10"),
        -delta,
        delta
      )
    ) |>
    filter(group != "(Intercept)") |>
    group_by(group) |>
    summarise(
      change = if (n() == 1) {
        sprintf("%+.3f", delta)
      } else {
        paste(
          sprintf(
            "%s %+.3f",
            sub(cur_group()$group, "", term, fixed = TRUE),
            delta
          ),
          collapse = "; "
        )
      },
      .groups = "drop"
    )

  group_labels <- c(
    age_group = "Age group (15–17)",
    srsex = "Gender (female)",
    ombsrreo = "Race/ethnicity",
    sch_typ = "School type",
    ahedtc_binary = "Parents' educational attainment",
    povll_binary = "Federal poverty level",
    lnghmt_binary = "Language spoken at home",
    ur_clrt2 = "Rural/urban residence",
    tl25_pos = "Cares deeply about community issues",
    tl27_pos = "Believes can make a difference",
    tl50 = "Volunteered to solve community problem",
    tl53_pos = "Can contact government to solve problem",
    tq10_pos = "Able to talk to family about feelings",
    tq11_pos = "Felt supported by family",
    tq14_pos = "Felt supported by friends",
    tq16_pos = "Enjoyed community traditions",
    uninsured = "Uninsured",
    health_office = "Goes to primary care for health care",
    tf9 = "Did not delay medical care",
    school_last_week = "Attended school last week",
    health_missed_days = "Number of school days missed for health",
    tl10 = "Participated in clubs in last year",
    tq15_pos = "Often felt a sense of belonging at school",
    tmax_tract10_prior_90_days_count32_delta = "Census tract: heatwave anomaly (prev 90 days)",
    tmax_tract10_prior_90_days_mean_delta = "Census tract: avg temp anomaly (prev 90 days)",
    tmax_tract10_prior_yr_count32_delta = "Census tract: heatwave anomaly (prev year)",
    tmax_tract10_prior_yr_mean_delta = "Census tract: avg temp anomaly (prev year)",
    tmax_county_prior_90_days_count32_delta = "County: heatwave anomaly (prev 90 days)",
    tmax_county_prior_90_days_mean_delta = "County: avg temp anomaly (prev 90 days)",
    tmax_county_prior_yr_count32_delta = "County: heatwave anomaly (prev year)",
    tmax_county_prior_yr_mean_delta = "County: avg temp anomaly (prev year)"
  )
  stopifnot(all(groups %in% names(group_labels)))

  # totals: everything changed jointly vs. the sum of the one-at-a-time pieces
  total_joint <- c(plogis(x23 %*% beta)) - base_p

  #### Conf int ####
  V_mat <- as.matrix(V)
  stopifnot(identical(rownames(V_mat), coef_names)) # same order as beta

  set.seed(20261019)
  B <- 5000
  beta_draws <- MASS::mvrnorm(B, mu = beta, Sigma = V_mat) # B x p

  p_base_draws <- plogis(beta_draws %*% x22) # B x 1

  contrib_draws <- sapply(groups, \(g) {
    x <- x22
    idx <- term_group == g
    x[idx] <- x23[idx]
    plogis(beta_draws %*% x) - p_base_draws
  }) # B x length(groups)

  total_draws <- cbind(
    "Sum of individual contributions" = rowSums(contrib_draws),
    "All covariates changed jointly" = c(
      plogis(beta_draws %*% x23) - p_base_draws
    )
  )

  ci <- apply(cbind(contrib_draws, total_draws), 2, quantile, c(0.025, 0.975))

  #### create final matrix ####

  change_df_raw <- tibble::tibble(group = groups, contribution = contrib) |>
    left_join(change_txt, by = "group") |>
    arrange(desc(abs(contribution)), match(group, groups)) |>
    mutate(Variable = unname(group_labels[group])) |>
    tibble::add_row(
      group = colnames(total_draws), # matches ci's column names
      Variable = colnames(total_draws),
      contribution = c(sum(contrib), total_joint),
      change = ""
    ) |>
    mutate(
      lwr = ci[1, group],
      upr = ci[2, group]
    )

  change_df <- change_df_raw |>
    transmute(
      Variable,
      `Change, 2022–2023` = change,
      `Estimated Contribution, pp (95% CI)` = sprintf(
        "%+.1f (%+.1f to %+.1f)",
        100 * contribution,
        100 * lwr,
        100 * upr
      )
    )

  change_df_raw |>
    saveRDS(here::here("Documents", "post_hoc_change.rds"))

  change_df |>
    saveRDS(here::here("Documents", "post_hoc_change_table.rds"))
}
