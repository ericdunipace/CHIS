### Disparities research in barriers to mental health #############################################

### Import and read stuff in ######################################################################
# Load required libraries (ensure they are installed)
library(dplyr) # data manipulation
library(survey) # Survey data analysis
library(gtsummary) # for summary tables
library(stringr)
library(here)
library(broom)
library(ggplot2)
library(scales)
library(emmeans)

here::i_am(file.path("R", "localCode", "CHIS_PUF.R")) # adjust this to your actual file location

#### Setup output directory ####
if (!dir.exists(output_dir <- here::here("Outputs"))) {
  dir.create(output_dir)
}
dir.create(here::here("Outputs", "tables"), showWarnings = FALSE)
dir.create(here::here("Outputs", "figures"), showWarnings = FALSE)

#### load Functions ####
source(here::here("R", "Functions.R"))

puf_file <- here::here("data", "chis_puf_combined.Rds")

if (!file.exists(puf_file)) {
  message(
    "Skipping CHIS_PUF.R because the CHIS public-use file is missing: ",
    puf_file,
    ". See README for how to obtain it."
  )
} else {
  #### load data ####
  chis <- readRDS(here::here("data", "chis_puf_combined.Rds"))

  #### Set up survey design object to account for weights ####
  # Set up survey design for analysis
  chis_design <- svrepdesign(
    data = chis,
    weights = ~fnwgt0,
    repweights = "fnwgt[1-9]",
    type = "other",
    scale = 1,
    rscales = 1,
    mse = TRUE
  )

  #### Define analysis variables ####

  baseline_demographics <- c(
    # note these differ from private use data
    "age_group", # SELF-REPORTED AGE
    "srsex", # SELF-REPORTED GENDER
    "ombsrtn_p1", # OMB/CURRENT DOF RACE - ETHNICITY
    "schtyp_p1", # TYPE OF SCHOOL ATTENDED
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
    "(tmax_tract10_prior_90_days_count32_delta)", # Tmax above 32 for the 90 days prior to survey date
    "(tmax_tract10_prior_90_days_mean_delta)", # Mean Tmaxfor the 90 days prior to survey date
    "(tmax_tract10_prior_yr_mean_delta)", # Mean Tmax for the year prior to survey date
    "(tmax_tract10_prior_yr_count32_delta)", # Count of Tmax above 32 for the year prior to survey date
    "(tmax_county_prior_90_days_count32_delta)", # Tmax above 32 for the 90 days prior to survey date
    "(tmax_county_prior_90_days_mean_delta)", # Mean Tmaxfor the 90 days prior to survey date
    "(tmax_county_prior_yr_mean_delta)", # Mean Tmax for the year prior to survey date
    "(tmax_county_prior_yr_count32_delta)" # Count of Tmax above 32 for the year prior to survey date
  )

  # Aggregate all the variables
  characteristics <- c(
    baseline_demographics,
    modifiable_protective,
    access_to_care,
    civic_engagement
    # , climate_variables
  )

  # formula
  formula <- paste0(
    "I(tf45 == 'Yes') ~ ",
    paste0(characteristics, collapse = " + ")
  )

  # variables for table 1
  table_demographics <- baseline_demographics
  table_demographics[grep("age_group", baseline_demographics)] <- "cont_age"

  attr(table_demographics, "label") <- list(
    "tf45" = "Climate Anxiety", #CLIMATE CHANGE MAKES YOU NERVOUS/DEPRESSED/STRESSED
    "cont_age" = "Age", # SELF-REPORTED AGE
    "srsex" = "Sex", # SELF-REPORTED GENDER
    "ombsrtn_p1" = "Ethnicity", # OMB/CURRENT DOF RACE - ETHNICITY
    "schtyp_p1" = "Type of School Attended", # TYPE OF SCHOOL ATTENDED
    "ahedtc_p1" = "Parents' Educational Attainment", # ADULT EDUCATIONAL ATTAINMENT
    "povll" = "Poverty Level", # POVERTY LEVEL
    "lnghmt_p1" = "Language Spoken at Home", # LANGUAGE SPOKEN AT HOME
    "ur_clrt2" = "Rural/Urban (Claritas ZIP, 2-level)" # URBAN/RURAL CLASSIFICATION
  )

  attr(table_demographics, "type") <- list(
    "tf45" = "categorical", #CLIMATE CHANGE MAKES YOU NERVOUS/DEPRESSED/STRESSED
    "cont_age" = "continuous", # SELF-REPORTED AGE
    "srsex" = "categorical", # SELF-REPORTED GENDER
    "ombsrtn_p1" = "categorical", # OMB/CURRENT DOF RACE - ETHNICITY
    "schtyp_p1" = "categorical", # TYPE OF SCHOOL ATTENDED
    "ahedtc_p1" = "categorical", # ADULT EDUCATIONAL ATTAINMENT
    "povll" = "categorical", # POVERTY LEVEL
    "lnghmt_p1" = "categorical", # LANGUAGE SPOKEN AT HOME
    "ur_clrt2" = "categorical" # URBAN/RURAL CLASSIFICATION
  )

  #### TABLE 1 - DEMOGRAPHICS #########################################################################

  # Define baseline demographic variables to be included in the table

  climateanx_tot <- svytotal(~tf45, design = chis_design, na.rm = TRUE)

  demo_table <- gtsummary::tbl_custom_summary(
    data = chis_design$variables %>%
      mutate(cont_age = as.numeric(as.character(srage_p))),
    by = "tf45",
    stat_fns = everything() ~ mean_svy_rep,
    label = attr(table_demographics, "label"),
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~
        "{N} ({p}%) "
    ),
    digits = NULL,
    type = attr(table_demographics, "type"),
    missing = c("ifany"),
    missing_text = "Unknown",
    missing_stat = "{N_miss}",
    include = all_of(table_demographics)
  ) %>%
    bold_labels() %>%
    modify_header(
      stat_1 ~ "**Yes**, <br>N = {format(round(climateanx_tot[1], 0),big.mark = ',')}",
      stat_2 ~ "**No**, <br>N = {format(round(climateanx_tot[2], 0),big.mark = ',')}"
    ) %>%
    modify_spanning_header(all_stat_cols() ~ "**Climate Anxiety**") %>%
    modify_footnote_header(
      footnote = "All totals and percentages are based on values from sample weights. There are a total of {format(nrow(chis),big.mark = ',')} observations in the unweighted data. Percentages across all values of one variable sum to approximately 100% due to rounding",
      columns = all_stat_cols(),
      replace = FALSE
    )

  # Display the table
  demo_table %>%
    as_gt() %>%
    gt::gtsave(filename = here::here("Outputs", "demographics.tex"))

  #### analysis 1 -- map data needs the DAC data ####

  #### analysis 2 -- anxiety prevalence ####
  overall.prev <- survey::svymean(~tf45, design = chis_design) |>
    as.data.frame() |>
    tibble::as_tibble() |>
    dplyr::mutate(
      lwr = mean + SE * qnorm(0.025),
      upr = mean + SE * qnorm(0.975)
    )

  overall.prev |>
    saveRDS(
      here::here("Outputs", "tables", "prevalence_overall.rds")
    )

  prev <- survey::svyby(
    ~tf45,
    by = ~year,
    design = chis_design,
    na.rm = FALSE,
    FUN = svymean
    # FUN = svyciprop, vartype = "ci"
  ) |>
    as.data.frame() |>
    dplyr::mutate(
      anx = tf45Yes,
      year = as.factor(year),
      lwr = anx + se1 * -qnorm(0.975),
      upr = anx + se1 * qnorm(0.975),
      group = "all"
    )

  subset_mean <- svymean(
    ~tf45,
    subset(chis_design, year %in% c(2021, 2022))
  )

  prev |>
    mutate(anx_2021_2022 = subset_mean[1]) |>
    saveRDS(
      here::here("Outputs", "tables", "prevalence.rds")
    )

  prev_fig <- prev |>
    ggplot2::ggplot(ggplot2::aes(
      y = anx,
      x = year,
      ymin = lwr,
      ymax = upr,
      group = group
    )) +
    ggplot2::geom_errorbar(width = 0) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::scale_y_continuous(
      labels = scales::label_percent(),
      guide = ggplot2::guide_axis(angle = 90),
      breaks = seq(0, 1, by = .04)
      # , limits = c(0, 1) # Optional: forces the axis to go from 0% to 100%
    ) +
    ggplot2::ylab("Prevalence") +
    ggplot2::xlab("Survey Year") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )

  cairo_pdf(
    here::here("Outputs", "figures", "prevalence.pdf"),
    height = 4,
    width = 4
  )
  print(prev_fig)
  dev.off()

  png(
    here::here("Outputs", "figures", "prevalence.png"),
    res = 300,
    width = 4,
    height = 4,
    units = "in"
  )
  print(prev_fig)
  dev.off()

  # model for differences
  des <- update(chis_design, year = factor(year))
  model <- svyglm(
    I(tf45 == 'Yes') ~ as.factor(year),
    design = chis_design,
    family = "gaussian"
  )

  # Pairwise tests
  pairs(emmeans(model, ~year), adjust = "BH") |>
    tibble::as_tibble() |>
    saveRDS(here::here("Outputs", "tables", "prev_sig_tests.rds"))

  svyglm(
    I(tf45 == 'Yes') ~ I(year == "2023"),
    design = chis_design,
    family = "gaussian"
  ) |>
    summary()

  #### ANALYSIS #3- CLIMATE CHANGE/MENTAL HEALTH #####################################################################################
  # full version with climate vars needs DAC data
  # this was a preliminary version below

  #glm model
  glm.model <- svyglm(
    as.formula(formula),
    family = quasibinomial(),
    design = chis_design
  )

  tidy_model <- broom::tidy(glm.model, conf.int = TRUE, exponentiate = TRUE)
  tidy_model$adj.p.value <- p.adjust(tidy_model$p.value, method = "fdr")

  results_table <- tidy_model %>%
    select(term, estimate, conf.low, conf.high, adj.p.value) %>%
    mutate(adj.p.value = format.pval(adj.p.value, digits = 1, eps = 0.0001)) %>%
    mutate(across(where(is.numeric), ~ round(., 3))) %>%
    rename(
      `Variable` = term,
      `Odds Ratio` = estimate,
      `95% CI Lower` = conf.low,
      `95% CI Upper` = conf.high,
      `P-value` = adj.p.value
    )

  print(results_table)
  write.csv(
    results_table,
    here::here("Outputs", "vulnerable_subgroups_model_table.csv"),
    row.names = FALSE
  )

  #### adding year fixed effects sensitivity ####
  year.glm <- update(glm.model, formula = . ~ . + as.factor(year))

  # log-odds (95% CI) by term. Reverse-coded terms are negated (bounds
  # swapped) so each row reads as its label, as in Table 2.
  flip_terms <- c("tl50No", "tl10No", "lnghmt_binaryNon-English")

  tidy_log_odds <- function(model, col) {
    broom::tidy(model, conf.int = TRUE) |>
      mutate(
        flip = term %in% flip_terms,
        est = ifelse(flip, -estimate, estimate),
        low = ifelse(flip, -conf.high, conf.low),
        high = ifelse(flip, -conf.low, conf.high),
        "{col}" := midline_decimal(sprintf(
          "%.2f (%.2f to %.2f)",
          est,
          low,
          high
        ))
      ) |>
      select(term, all_of(col))
  }

  # PUF race categories (ombsrtn_p1) differ from the restricted-data coding
  tidy_year_model <- tidy_log_odds(year.glm, "Year Model") |>
    left_join(tidy_log_odds(glm.model, "Base Model"), by = "term") |>
    mutate(
      `Base Model` = coalesce(`Base Model`, "··"),
      Variable = case_when(
        term == "(Intercept)" ~ "Intercept",
        term == "age_group15-17" ~ "Age Group: 15–17",
        term == "srsexFemale" ~ "Gender: Female",
        term == "ombsrtn_p1White, Non-Hispanic" ~ "Race/ethnicity: White",
        term == "ombsrtn_p1Other Race" ~ "Race/ethnicity: Other race",
        term == "ombsrtn_p1Asian, Non-Hispanic" ~ "Race/ethnicity: Asian",
        term ==
          "ombsrtn_p1Two or More Races, Non-Hispanic" ~ "Race/ethnicity: Two or more races",
        term == "schtyp_p1Public School" ~ "School Type: Public",
        term ==
          "schtyp_p1Private School (includes Homeschool)" ~ "School Type: Private",
        term ==
          "ahedtc_binaryCollege or more" ~ "Parents' Educational Attainment: Attended College",
        term ==
          "povll_binary300% FPL And Above" ~ "Federal Poverty Level (FPL): ≥300% FPL",
        term == "lnghmt_binaryNon-English" ~ "Language Spoken at Home: English",
        term == "ur_clrt2Rural" ~ "Rural",
        term == "tl25_posYes" ~ "Cares Deeply About Community Issues",
        term == "tl27_posYes" ~ "Believes Can Make a Difference",
        term == "tl50No" ~ "Volunteered to Solve Community Problem",
        term == "tl53_posYes" ~ "Can Contact Government to Solve Problem",
        term == "tq10_posYes" ~ "Able to Talk to Family About Feelings",
        term == "tq11_posYes" ~ "Felt Supported by Family",
        term == "tq14_posYes" ~ "Felt Supported by Friends",
        term == "tq16_posYes" ~ "Enjoyed Community Traditions",
        term == "tl10No" ~ "Participated in Clubs in Last Year",
        term == "tq15_posYes" ~ "Often Felt A Sense of Belonging at School",
        term == "uninsuredYes" ~ "Uninsured",
        term ==
          "health_officePrimary care office" ~ "Goes to Primary Care for Health Care",
        term == "tf9No" ~ "Did Not Delay Medical Care",
        term == "school_last_weekYes" ~ "Attended School Last Week",
        term ==
          "health_missed_days" ~ "Number of School Days Missed for Health",
        term == "as.factor(year)2022" ~ "Survey Year: 2022 (vs 2021)",
        term == "as.factor(year)2023" ~ "Survey Year: 2023 (vs 2021)",
        .default = term
      )
    ) |>
    select(Variable, `Base Model`, `Year Model`)

  tidy_year_model |>
    saveRDS(file = here::here("Outputs", "tables", "year_sens.rds"))

  ftest <- survey::regTermTest(year.glm, ~ as.factor(year))
  yearest <- broom::tidy(year.glm, conf.int = TRUE, exponentiate = TRUE) |>
    filter(term == "as.factor(year)2023")
  abs(
    c(coef(glm.model), NA_real_, NA_real_) -
      (year.glm |> coef())
  ) |>
    sort(decreasing = TRUE) %>%
    .[2] |>
    unname() |>
    c(
      change = _,
      pval = ftest$p,
      stat = ftest$Ftest,
      df1 = ftest$df,
      df2 = ftest$ddf,
      OR = yearest$estimate,
      conf.low = yearest$conf.low,
      conf.high = yearest$conf.high
    ) |>
    saveRDS(here::here("Outputs", "tables", "year_sens_change.rds"))

  #### Analysis 4: demographic shifts ####
  survey::svyby(
    ~tl25_pos,
    by = ~year,
    update(
      chis_design,
      year = factor(year)
    ),
    survey::svymean
  ) |>
    as.data.frame() |>
    tibble::as_tibble() |>
    mutate(cares = tl25_posYes) |>
    mutate() |>
    saveRDS(here::here("Outputs", "tables", "cares_about_community.rds"))

  survey::svyby(
    ~dstrs12,
    by = ~year,
    update(
      chis_design,
      year = factor(year)
    ),
    survey::svymean
  ) |>
    as.data.frame() |>
    tibble::as_tibble() |>
    mutate(distress = dstrs12Yes) |>
    saveRDS(here::here("Outputs", "tables", "psych_distress.rds"))
}
