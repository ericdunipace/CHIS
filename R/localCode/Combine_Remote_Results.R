library(ggplot2)
library(ggdist)
library(dplyr)
library(here)
library(gt)
library(officer)
library(mvtnorm)
library(sf) # Spatial data handling and mapping
library(grid)
library(flextable)
library(purrr)
library(glue)

#### load Functions ####
source(here::here("R", "Functions.R"))

#### output dir ####
doc_dir <- here::here("Documents")
dir.create(doc_dir, showWarnings = FALSE)

fig_dir <- here::here("Figures")
dir.create(fig_dir, showWarnings = FALSE)

required_outputs <- c(
    "table1_basic.csv",
    "table1.csv",
    "table1_raw_basic.csv",
    "table1_raw.csv",
    "table1_raw_display.csv",
    "glm_model_summary.csv",
    "glm_vcov.csv",
    "fe_model_summary.csv",
    "fe_vcov.csv",
    "climiate_anxiety_map_data.csv"
)

missing_outputs <- c(
    required_outputs[!file.exists(here::here("Outputs", required_outputs))],
    if (!file.exists(here::here("data", "auxiliary_data.rds"))) {
        "data/auxiliary_data.rds"
    }
)

linear.combo.flag <- TRUE

if (length(missing_outputs) > 0L) {
    message(
        "Skipping Combine_Remote_Results.R because Outputs/ is missing: ",
        paste(missing_outputs, collapse = ", ")
    )
} else {
    #### load table 1 information ####
    tabl_1_long <- utils::read.csv(here::here("Outputs", "table1_basic.csv"))
    tab1_long <- utils::read.csv(here::here("Outputs", "table1.csv")) |>
        mutate(variable = if_else(variable == "Age*", "Age", variable))
    check_table1_long_sums(tabl_1_long)
    check_table1_sums(tab1_long)

    #### raw number for table 1 in the appendix ####
    tabl_1_long_raw <- utils::read.csv(here::here(
        "Outputs",
        "table1_raw_basic.csv"
    ))
    tab1_long_raw <- utils::read.csv(here::here("Outputs", "table1_raw.csv"))
    check_table1_long_sums(tabl_1_long_raw)
    check_table1_sums(tab1_long_raw)

    # Same layout as Table 1: the Yes column for each year (n (%) who
    # reported climate anxiety), with age for all adolescents (Total column)
    tab1_display_raw <- utils::read.csv(here::here(
        "Outputs",
        "table1_raw_display.csv"
    )) |>
        relabel_table1()
    for (y in c("2021", "2022", "2023")) {
        age_row <- tab1_display_raw$variable == "Age, years"
        tab1_display_raw[age_row, paste0("stat_", y, "_Yes")] <-
            tab1_display_raw[age_row, paste0("stat_", y, "_Total")]
    }

    saveRDS(
        tab1_display_raw,
        file = here::here("Outputs", "table1_display_raw.rds")
    )

    #### Table 1: unweighted n (weighted %) ####
    # Row percentages (prevalence of climate anxiety in each category) are
    # used in the manuscript; column percentages (make-up of each climate
    # anxiety group) are kept as an alternative layout
    tab1_years <- c("2021", "2022", "2023")
    n_unweighted <- tab1_long_raw |>
        dplyr::filter(variable == "Total (%)") |>
        dplyr::mutate(n = format(N, big.mark = ",", trim = TRUE)) |>
        dplyr::select(year, status, n)
    n_for <- function(y, s) {
        n_unweighted$n[n_unweighted$year == y & n_unweighted$status == s]
    }
    rural_note <- paste0(
        "†Rural/urban classification is based on Claritas ZIP code ",
        "classification, which classifies ZIP codes as either rural or ",
        "urban based on population density and other factors."
    )

    # row percentages (prevalence)
    tab1_header <- stats::setNames(
        paste0(
            tab1_years,
            " (n=",
            sapply(tab1_years, n_for, s = "Total"),
            ")*"
        ),
        paste0("stat_", tab1_years, "_Yes")
    )

    tab1_display_pct <- tab1_long |>
        table1_n_pct_display(tab1_long_raw, type = "row") |>
        relabel_table1()

    tab1_out <- render_table1(
        tab1_display_pct,
        doc_dir = doc_dir,
        prefix = "table_1",
        suffix = "",
        notes = c(
            paste0(
                "*Data are unweighted n (weighted %) of adolescents in each ",
                "category who reported climate anxiety, except for age, which is weighted mean (SD) ",
                "for all adolescents. n is the unweighted number of ",
                "respondents; % is weighted with CHIS survey weights, so ",
                "percentages do not equal n divided by the unweighted total. ",
                "SD=standard deviation."
            ),
            rural_note
        ),
        header = tab1_header,
        landscape = FALSE,
        max_width = 6.5
    )

    # column percentages, alternative layout (not in the manuscript)
    tab1_col_header <- unlist(lapply(tab1_years, function(y) {
        stats::setNames(
            c(
                paste0("Climate anxiety: yes (n=", n_for(y, "Yes"), ")*"),
                paste0("Climate anxiety: no (n=", n_for(y, "No"), ")*")
            ),
            paste0("stat_", y, c("_Yes", "_No"))
        )
    }))

    tab1_display_col <- tab1_long |>
        table1_n_pct_display(tab1_long_raw, type = "column") |>
        relabel_table1()

    render_table1(
        tab1_display_col,
        doc_dir = doc_dir,
        prefix = "table_1_column",
        suffix = "",
        notes = c(
            paste0(
                "*Data are unweighted n (weighted %) of adolescents in each ",
                "group reporting climate anxiety ",
                "group, or weighted mean (SD), by year. n is the unweighted number of ",
                "respondents; % is weighted with CHIS survey weights, so ",
                "percentages do not equal n divided by the unweighted total. ",
                "SD=standard deviation."
            ),
            rural_note
        ),
        header = tab1_col_header,
        spanners = stats::setNames(rep(2L, length(tab1_years)), tab1_years),
        landscape = FALSE,
        max_width = 6.5
    )

    # unweighted counts for the appendix, same layout as Table 1
    render_table1(
        tab1_display_raw,
        doc_dir = doc_dir,
        prefix = "raw_table_1",
        suffix = "_appendix",
        notes = c(
            paste0(
                "*Data are unweighted n (%) of adolescents in each category ",
                "who reported climate anxiety, except for age, which is ",
                "unweighted mean (SD) for all adolescents. n is the number ",
                "of respondents. SD=standard deviation."
            ),
            rural_note
        ),
        header = tab1_header,
        landscape = FALSE,
        max_width = 6.5
    )

    # true baseline table
    baseline_weighted <- tab1_long |>
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

    baseline_raw <- tab1_long_raw |>
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

    render_table1(
        baseline_weighted |>
            table1_n_pct_display(
                baseline_raw,
                type = "column"
            ) |>
            relabel_table1(),
        doc_dir = doc_dir,
        prefix = "baseline_table",
        suffix = "_appendix",
        notes = c(
            paste0(
                "*Data are unweighted n (weighted %) of adolescents ",
                "or weighted mean (SD) by survey year. n is the unweighted ",
                "number of ",
                "respondents; % is weighted with CHIS survey weights, so ",
                "percentages do not equal n divided by the unweighted total. ",
                "SD=standard deviation."
            ),
            rural_note
        ),
        header = tab1_header,
        landscape = FALSE,
        max_width = 6.5
    )

    #### Load Regression Results Files ####
    # glm model
    # estimates from model
    glm_coef <- utils::read.csv(
        file = here::here("Outputs", "glm_model_summary.csv")
    )
    # vcov for predictions
    glm_vcov <- utils::read.csv(
        file = here::here("Outputs", "glm_vcov.csv"),
        row.names = 1
    )

    # re model
    # fe <- read.csv(here::here("Outputs", "fixef_coef.csv"))
    # V_fe <- read.csv(here::here("Outputs", "mixef_vcov.csv"))
    # beta_me <- utils::read.csv(file = here::here("Outputs", "mixef_coef.csv"))
    # rep_beta_me <- utils::read.csv(
    #     file = here::here("Outputs", "mixef_replicate_coef.csv")
    # )

    #### generate tables for glm coeficients ####
    group_labels <- dplyr::tribble(
        ~variable            , ~group                         ,
        "tl25_pos"           , "Civic engagement"             , #"Cares Deeply About Community Issues"
        "tl27_pos"           , "Civic engagement"             , #"Believes Can Make a Difference"
        "tl50"               , "Civic engagement"             , #"Volunteered to solve community problem"
        "tl53_pos"           , "Civic engagement"             , #"Can Contact Gov't to Solve Problem"
        "tq10_pos"           , "Civic engagement"             , #"Able to Talk to Family About Feelings"
        "tq11_pos"           , "Civic engagement"             , #"Felt Supported by Family"
        "tq14_pos"           , "Civic engagement"             , #"Felt Supported by Friends"
        "tq16_pos"           , "Civic engagement"             , #"Enjoyed Community Traditions"
        "uninsured"          , "Access to care"               ,
        "health_office"      , "Access to care"               ,
        "tf9"                , "Access to care"               , #"Did Not Delay Medical Care"
        "school_last_week"   , "School attendance/engagement" ,
        # "scale(I(as.numeric(school_last_week ==  'Yes')) * tb4)",
        "health_missed_days" , "School attendance/engagement" ,
        "tl10"               , "Civic engagement"             , #"Participated in clubs in last year"
        "tq15_pos"           , "Civic engagement" # "Often Felt A Sense of Belonging at School"
    )
    glm_lor_table <- glm_coef |>
        rename(
            term = X,
            estimate = Estimate,
            std.error = Std..Error,
            statistic = t.value,
            p.value = Pr...t..
        ) |>
        mutate(
            df = sapply(1:n(), function(i) {
                recover_df(statistic[i], p.value[i])
            })
        ) %>%
        {
            if (isTRUE(linear.combo.flag)) {
                V <- as.matrix(glm_vcov)
                colnames(V) <- rownames(V)

                combo <- c(
                    tmax_county_prior_90_days_count32_delta = "tmax_tract10_prior_90_days_count32_delta",
                    tmax_county_prior_90_days_mean_delta = "tmax_tract10_prior_90_days_mean_delta",
                    tmax_county_prior_yr_count32_delta = "tmax_tract10_prior_yr_count32_delta",
                    tmax_county_prior_yr_mean_delta = "tmax_tract10_prior_yr_mean_delta"
                )
                stopifnot(
                    identical(.$term, rownames(V)), # same order as vcov
                    all(c(names(combo), combo) %in% rownames(V)),
                    isTRUE(all.equal(
                        sqrt(diag(V)),
                        .$std.error,
                        check.attributes = FALSE
                    )) # vcov matches summary
                )
                A <- diag(nrow(V))
                dimnames(A) <- dimnames(V)
                A[cbind(names(combo), combo)] <- 1

                . |>
                    mutate(
                        estimate = drop(A %*% estimate),
                        std.error = sqrt(diag(A %*% V %*% t(A))),
                        statistic = estimate / std.error,
                        p.value = 2 * pt(-abs(statistic), df)
                    )
            } else {
                .
            }
        } %>%
        # note: all df are currently the same so not combining
        mutate(
            lwr = estimate - std.error * qt(.975, df = df),
            upr = estimate + std.error * qt(.975, df = df),
            e_est = exp(estimate),
            e_lwr = exp(lwr),
            e_upr = exp(upr),
            term = factor(term) |>
                forcats::fct_relevel(
                    "(Intercept)",
                    "age_group15-17",
                    "srsexFemale",
                    "ombsrreoAfrican American",
                    "ombsrreoAsian/Pacific Islander",
                    # "ombsrreoAsian",
                    "ombsrreoMultiracial and Other",
                    "ombsrreoWhite",
                    "sch_typPublic School",
                    "sch_typPrivate School",
                    # "sch_typOther", #now in reference
                    "ahedtc_binaryCollege or more",
                    "povll_binary300% FPL And Above",
                    "lnghmt_binaryNon-English",
                    "ur_clrt2Rural",
                    "uninsuredYes",
                    "health_officePrimary care office",
                    "tf9No", #"Did Not Delay Medical Care"
                    "tl25_posYes", #"Cares Deeply About Community Issues"
                    "tl27_posYes", #"Believes Can Make a Difference"
                    "tl50No", #"Volunteered to solve community problem"
                    "tl53_posYes", #"Can Contact Gov't to Solve Problem"
                    "tq10_posYes", #"Able to Talk to Family About Feelings"
                    "tq11_posYes", #"Felt Supported by Family"
                    "tq14_posYes", #"Felt Supported by Friends"
                    "tq16_posYes",
                    "tl10No", #"Participated in clubs in last year"
                    "school_last_weekYes",
                    "health_missed_days",
                    # "scale(I(as.numeric(school_last_week == \"Yes\")) * tb4)",
                    "tq15_posYes", # "Often Felt A Sense of Belonging at School"
                    "tmax_tract10_prior_90_days_count32_delta",
                    "tmax_tract10_prior_90_days_mean_delta",
                    "tmax_tract10_prior_yr_count32_delta",
                    "tmax_tract10_prior_yr_mean_delta",
                    "tmax_county_prior_90_days_count32_delta",
                    "tmax_county_prior_90_days_mean_delta",
                    "tmax_county_prior_yr_count32_delta",
                    "tmax_county_prior_yr_mean_delta"
                ),
            orig_term = term,
            label = term,
            label = forcats::fct_recode(
                label,
                "Intercept" = "(Intercept)",
                "Age group" = "age_group15-17",
                "Gender" = "srsexFemale",
                "Race/ethnicity" = "ombsrreoWhite",
                "Race/ethnicity" = "ombsrreoAfrican American",
                "Race/ethnicity" = "ombsrreoAsian/Pacific Islander",
                # "Race/ethnicity" = "ombsrreoAsian",
                "Race/ethnicity" = "ombsrreoMultiracial and Other",
                "School type" = "sch_typPublic School",
                "School type" = "sch_typPrivate School",
                # "School type" = "sch_typOther",
                "Parents' educational attainment" = "ahedtc_binaryCollege or more",
                "Federal poverty level (FPL)" = "povll_binary300% FPL And Above",
                "Language spoken at home" = "lnghmt_binaryNon-English", # note we flip sign below
                "Rural/urban residence" = "ur_clrt2Rural",
                "Civic engagement" = "tl25_posYes", #"Cares Deeply About Community Issues"
                "Civic engagement" = "tl27_posYes", #"Believes Can Make a Difference"
                "Civic engagement" = "tl50No", #"Volunteered to solve community problem" # note we flip the sign below
                "Civic engagement" = "tl53_posYes", #"Can Contact Gov't to Solve Problem"
                "Civic engagement" = "tq10_posYes", #"Able to Talk to Family About Feelings"
                "Civic engagement" = "tq11_posYes", #"Felt Supported by Family"
                "Civic engagement" = "tq14_posYes", #"Felt Supported by Friends"
                "Civic engagement" = "tq16_posYes",
                "Access to care" = "uninsuredYes",
                "Access to care" = "health_officePrimary care office",
                "Access to care" = "tf9No", #"Did Not Delay Medical Care"
                "School attendance/engagement" = "school_last_weekYes",
                # "Civic engagement" = "scale(I(as.numeric(school_last_week == \"Yes\")) * tb4)",
                "School attendance/engagement" = "health_missed_days",

                "Civic engagement" = "tl10No", #"Participated in clubs in last year"
                "Civic engagement" = "tq15_posYes", # "Often Felt A Sense of Belonging at School"
                "Tract deviation from county‡" = "tmax_tract10_prior_90_days_count32_delta",
                "Tract deviation from county‡" = "tmax_tract10_prior_90_days_mean_delta",
                "Tract deviation from county‡" = "tmax_tract10_prior_yr_mean_delta",
                "Tract deviation from county‡" = "tmax_tract10_prior_yr_count32_delta",
                "County-average temperature‡" = "tmax_county_prior_90_days_count32_delta",
                "County-average temperature‡" = "tmax_county_prior_90_days_mean_delta",
                "County-average temperature‡" = "tmax_county_prior_yr_mean_delta",
                "County-average temperature‡" = "tmax_county_prior_yr_count32_delta"
            ),
            term = forcats::fct_recode(
                term,
                " " = "(Intercept)",
                "15–17" = "age_group15-17",
                "Female" = "srsexFemale",
                "African American" = "ombsrreoAfrican American",
                # "Native American" = "ombsrreoAmerican Indian",
                # "Pacific Islander" = "ombsrreoPacific Islander",
                "Asian/Pacific Islander" = "ombsrreoAsian/Pacific Islander",
                "Multiracial and other" = "ombsrreoMultiracial and Other",
                "White" = "ombsrreoWhite",
                "Public" = "sch_typPublic School",
                "Private" = "sch_typPrivate School",
                # "Other" = "sch_typOther",
                "Attended college" = "ahedtc_binaryCollege or more",
                "≥300% FPL" = "povll_binary300% FPL And Above",
                "English" = "lnghmt_binaryNon-English",
                "Rural" = "ur_clrt2Rural",
                "Cares deeply about community issues" = "tl25_posYes",
                "Believes can make a difference" = "tl27_posYes",
                "Volunteered to solve community problem" = "tl50No", # note we flip the sign below
                "Can contact government to solve problem" = "tl53_posYes",
                "Able to talk to family about feelings" = "tq10_posYes",
                "Felt supported by family" = "tq11_posYes",
                "Felt supported by friends" = "tq14_posYes",
                "Enjoyed community traditions" = "tq16_posYes",
                "Uninsured" = "uninsuredYes",
                "Goes to primary care for health care" = "health_officePrimary care office",
                "Did not delay medical care" = "tf9No",
                "Attended school last week" = "school_last_weekYes",
                # "Number School Days Missed for Health (normalized)" = "scale(I(as.numeric(school_last_week == \"Yes\")) * tb4)",
                "Number of school days missed for health" = "health_missed_days",
                "Participated in clubs in last year" = "tl10No", #note flip sign below
                "Often felt a sense of belonging at school" = "tq15_posYes",
                "Heatwave anomalies (prev 90 days)" = "tmax_tract10_prior_90_days_count32_delta",
                "Average temp anomalies (prev 90 days)" = "tmax_tract10_prior_90_days_mean_delta",
                "Heatwave anomalies (prev year)" = "tmax_tract10_prior_yr_count32_delta",
                "Average temp anomalies (prev year)" = "tmax_tract10_prior_yr_mean_delta",
                "Heatwave anomalies (prev 90 days)" = "tmax_county_prior_90_days_count32_delta",
                "Average temp anomalies (prev 90 days)" = "tmax_county_prior_90_days_mean_delta",
                "Heatwave anomalies (prev year)" = "tmax_county_prior_yr_count32_delta",
                "Average temp anomalies (prev year)" = "tmax_county_prior_yr_mean_delta"
            )
        ) |>
        mutate(
            flip = term %in%
                c(
                    "Volunteered to solve community problem",
                    "English",
                    "Participated in clubs in last year"
                )
        ) |>
        mutate(e_est = ifelse(flip, exp(-estimate), e_est)) |>
        mutate(e_lwr = ifelse(flip, exp(-upr), e_lwr)) |>
        mutate(e_upr = ifelse(flip, exp(-lwr), e_upr)) |>
        mutate(
            bh_pvalue = {
                out <- p.value
                # term is already relabelled here, so test the model term
                non_intercept <- orig_term != "(Intercept)"
                out[non_intercept] <- p.adjust(
                    p.value[non_intercept],
                    method = "BH"
                )
                out
            }
        ) |>
        mutate(
            stars = case_when(
                bh_pvalue < 0.001 ~ "***",
                bh_pvalue < 0.01 ~ "**",
                bh_pvalue < 0.05 ~ "*",
                TRUE ~ ""
            )
        ) |>
        arrange(as.numeric(label), as.numeric(term)) |>
        mutate(
            term = as.character(term),
            label = as.character(label)
        )
    age_before <- which(glm_lor_table$label == "Age group")[1]
    glm_lor_table <- tibble::add_row(
        glm_lor_table,
        term = "12–14 (ref)",
        label = "Age group",
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        df = NA_real_,
        lwr = NA_real_,
        upr = NA_real_,
        e_est = NA_real_,
        e_lwr = NA_real_,
        e_upr = NA_real_,
        flip = NA,
        bh_pvalue = NA_real_,
        stars = "",
        .before = age_before
    )
    sex_before <- which(glm_lor_table$label == "Gender")[1]
    glm_lor_table <- tibble::add_row(
        glm_lor_table,
        term = "Male (ref)",
        label = "Gender",
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        df = NA_real_,
        lwr = NA_real_,
        upr = NA_real_,
        e_est = NA_real_,
        e_lwr = NA_real_,
        e_upr = NA_real_,
        flip = NA,
        bh_pvalue = NA_real_,
        stars = "",
        .before = sex_before
    )
    race_before <- which(glm_lor_table$label == "Race/ethnicity")[1]
    glm_lor_table <- tibble::add_row(
        glm_lor_table,
        term = "Hispanic (ref)",
        label = "Race/ethnicity",
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        df = NA_real_,
        lwr = NA_real_,
        upr = NA_real_,
        e_est = NA_real_,
        e_lwr = NA_real_,
        e_upr = NA_real_,
        flip = NA,
        bh_pvalue = NA_real_,
        stars = "",
        .before = race_before
    )

    school_before <- which(
        glm_lor_table$label == "School type"
    )[1]
    glm_lor_table <- tibble::add_row(
        glm_lor_table,
        term = "Other (ref)",
        label = "School type",
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        df = NA_real_,
        lwr = NA_real_,
        upr = NA_real_,
        e_est = NA_real_,
        e_lwr = NA_real_,
        e_upr = NA_real_,
        flip = NA,
        bh_pvalue = NA_real_,
        stars = "",
        .before = school_before
    )

    par_edu_before <- which(
        glm_lor_table$label == "Parents' educational attainment"
    )[1]
    glm_lor_table <- tibble::add_row(
        glm_lor_table,
        term = "Less than a college degree (ref)",
        label = "Parents' educational attainment",
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        df = NA_real_,
        lwr = NA_real_,
        upr = NA_real_,
        e_est = NA_real_,
        e_lwr = NA_real_,
        e_upr = NA_real_,
        flip = NA,
        bh_pvalue = NA_real_,
        stars = "",
        .before = par_edu_before
    )

    fpl_before <- which(
        glm_lor_table$label == "Federal poverty level (FPL)"
    )[1]
    glm_lor_table <- tibble::add_row(
        glm_lor_table,
        term = "0–299% FPL (ref)",
        label = "Federal poverty level (FPL)",
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        df = NA_real_,
        lwr = NA_real_,
        upr = NA_real_,
        e_est = NA_real_,
        e_lwr = NA_real_,
        e_upr = NA_real_,
        flip = NA,
        bh_pvalue = NA_real_,
        stars = "",
        .before = fpl_before
    )
    language_before <- which(
        glm_lor_table$label == "Language spoken at home"
    )[1]
    glm_lor_table <- tibble::add_row(
        glm_lor_table,
        term = "Other (ref)",
        label = "Language spoken at home",
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        df = NA_real_,
        lwr = NA_real_,
        upr = NA_real_,
        e_est = NA_real_,
        e_lwr = NA_real_,
        e_upr = NA_real_,
        flip = NA,
        bh_pvalue = NA_real_,
        stars = "",
        .before = language_before
    )
    rural_before <- which(
        glm_lor_table$label == "Rural/urban residence"
    )[1]
    glm_lor_table <- tibble::add_row(
        glm_lor_table,
        term = "Urban (ref)",
        label = "Rural/urban residence",
        estimate = NA_real_,
        std.error = NA_real_,
        statistic = NA_real_,
        p.value = NA_real_,
        df = NA_real_,
        lwr = NA_real_,
        upr = NA_real_,
        e_est = NA_real_,
        e_lwr = NA_real_,
        e_upr = NA_real_,
        flip = NA,
        bh_pvalue = NA_real_,
        stars = "",
        .before = rural_before
    )

    glm_lor_table |>
        select(
            term,
            label,
            estimate,
            std.error,
            statistic,
            p.value,
            df,
            lwr,
            upr,
            e_est,
            e_lwr,
            e_upr,
            flip,
            bh_pvalue,
            stars,
            orig_term
        ) |>
        saveRDS(file = here::here(doc_dir, "glm_lor_table.rds"))
    # finally, test climate terms #
    V <- as.matrix(glm_vcov)
    beta <- as.numeric(glm_coef$Estimate)
    names(beta) <- colnames(V) <- rownames(V)

    combo <- c(
        tmax_county_prior_90_days_count32_delta = "tmax_tract10_prior_90_days_count32_delta",
        tmax_county_prior_90_days_mean_delta = "tmax_tract10_prior_90_days_mean_delta",
        tmax_county_prior_yr_count32_delta = "tmax_tract10_prior_yr_count32_delta",
        tmax_county_prior_yr_mean_delta = "tmax_tract10_prior_yr_mean_delta"
    )
    cl <- c(names(combo), combo)
    stopifnot(
        identical(glm_coef$X, rownames(V)), # same order as vcov
        all(c(names(combo), combo) %in% rownames(V)),
        isTRUE(all.equal(
            sqrt(diag(V)),
            glm_coef$Std..Error,
            check.attributes = FALSE
        )) # vcov matches summary
    )
    stat <- c(crossprod(beta[cl], solve(V[cl, cl], beta[cl])) / length(cl))
    df1 <- length(cl)
    df2 <- glm_lor_table$df |> unique() |> min(na.rm = TRUE)
    pval <- pf(
        stat,
        df1,
        df2,
        lower.tail = FALSE
    )
    tibble::tibble(
        stat = stat,
        df1 = df1,
        df2 = df2,
        p.value = pval
    ) |>
        saveRDS(here::here(doc_dir, "climate_vars_pvalue.rds"))

    # Lancet style: one OR (95% CI) column, "1 (ref)" for reference
    # categories, two mid-dots for cells with no data, and no stars
    glm_header <- c(
        or_ci = "Odds ratio (95% CI)†",
        p_value = "p value*"
    )

    glm_notes <- c(
        paste0(
            "*p values are Benjamini–Hochberg corrected for ",
            "non-intercept coefficients."
        ),
        paste0(
            "†Data are odds ratios (95% CI) except for ",
            "the intercept, which is an odds (95% CI)."
        ),
        glue::glue(
            "‡Design-based Wald test that all eight temperature ",
            "coefficients (tract and county; prior-year and prior-90-day ",
            "mean maximum temperature and heatwave days) are jointly zero: ",
            "F={midline_decimal(sprintf('%.2f', stat))} on {df1} and ",
            "{as.integer(round(df2))} df, {fmt_p(pval, prefix = TRUE)}. ",
            "County rows are county-average effects ",
            "and tract rows are deviations from county.",
            .sep = ""
        ),
        paste0(
            "CI=confidence interval. FPL=federal poverty level. ",
            "temp=temperature. prev=previous. ref=reference category."
        )
    )

    glm_rows <- glm_lor_table |>
        dplyr::mutate(
            is_ref = grepl("\\(Ref\\.\\)$", term),
            term = sub("\\s*\\(Ref\\.\\)$", "", term),
            or_ci = dplyr::if_else(
                is_ref,
                "1.00 (ref)",
                sprintf("%.2f (%.2f–%.2f)", e_est, e_lwr, e_upr)
            ),
            p_value = dplyr::case_when(
                is_ref ~ "··",
                TRUE ~ fmt_p(bh_pvalue)
            ),
            dplyr::across(c(or_ci, p_value), midline_decimal)
        ) |>
        lancet_rows(
            group_col = "label",
            label_col = "term",
            value_cols = names(glm_header)
        )

    # Portrait at 10pt, matching Table 1
    glm_font_size <- 10

    # HTML (gt) version
    glm_tab <- lancet_gt(
        glm_rows,
        glm_header,
        notes = glm_notes,
        font_size = glm_font_size
    )

    glm_tab |> saveRDS(file = here::here(doc_dir, "glm_tab_gt.rds"))

    glm_tab |> gt::gtsave(filename = here::here(doc_dir, "glm_tab.html"))

    # LaTeX version
    lancet_latex(
        glm_rows,
        glm_header,
        notes = glm_notes,
        font_size = glm_font_size,
        max_width = 6.5,
        label_width = 3.4,
        # too long for one portrait page at 10pt
        longtable = TRUE
    ) |>
        cat(file = here::here(doc_dir, "glm_tab.tex"))

    # Word version: footnotes go below the table, not in merged rows
    glm_tab_ft <- lancet_flextable(
        glm_rows,
        glm_header,
        font_size = glm_font_size,
        max_width = 6.5
    )

    lancet_docx(
        list(list(ft = glm_tab_ft, notes = glm_notes)),
        path = here::here(doc_dir, "glm_tab.docx")
    )

    # The manuscript embeds the table and writes the notes as paragraphs
    # below it, matching the submission .docx
    glm_tab_ft |>
        saveRDS(file = here::here(doc_dir, "glm_tab_ft.rds"))
    glm_notes |>
        saveRDS(file = here::here(doc_dir, "glm_tab_notes.rds"))

    # adding in fixed effects as sensitivity
    fe_est <- utils::read.csv(
        file = here::here("Outputs", "fe_model_summary.csv")
    ) |>
        rename(
            orig_term = X,
            estimate = Estimate,
            std.error = Std..Error,
            statistic = t.value,
            p.value = Pr...t..
        ) |>
        mutate(
            df = sapply(1:n(), function(i) {
                recover_df(statistic[i], p.value[i])
            }),
            # one shared design df; recover it where |t| is largest, since
            # recover_df() is unstable when |t| is near zero
            df = df[which.max(abs(statistic))]
        )

    # match the main table: county rows become county-average effects
    # (tract + county), tract rows stay as the deviation from county
    fe_V <- as.matrix(utils::read.csv(
        here::here("Outputs", "fe_vcov.csv"),
        row.names = 1
    ))
    colnames(fe_V) <- rownames(fe_V) # read.csv mangles column names
    fe_combo <- c(
        tmax_county_prior_90_days_count32_delta = "tmax_tract10_prior_90_days_count32_delta",
        tmax_county_prior_90_days_mean_delta = "tmax_tract10_prior_90_days_mean_delta",
        tmax_county_prior_yr_count32_delta = "tmax_tract10_prior_yr_count32_delta",
        tmax_county_prior_yr_mean_delta = "tmax_tract10_prior_yr_mean_delta"
    )
    stopifnot(
        identical(fe_est$orig_term, rownames(fe_V)),
        all(c(names(fe_combo), fe_combo) %in% rownames(fe_V)),
        isTRUE(all.equal(
            sqrt(diag(fe_V)),
            fe_est$std.error,
            check.attributes = FALSE
        ))
    )
    fe_A <- diag(nrow(fe_V))
    dimnames(fe_A) <- dimnames(fe_V)
    fe_A[cbind(names(fe_combo), fe_combo)] <- 1

    fe_est <- fe_est |>
        mutate(
            estimate = drop(fe_A %*% estimate),
            std.error = sqrt(diag(fe_A %*% fe_V %*% t(fe_A))),
            statistic = estimate / std.error,
            p.value = 2 * pt(-abs(statistic), df),
            lwr = estimate - std.error * qt(.975, df = df),
            upr = estimate + std.error * qt(.975, df = df)
        ) |>
        mutate(
            flip = orig_term %in%
                c("tl50No", "tl10No", "lnghmt_binaryNon-English")
        ) |>
        mutate(
            estimate = ifelse(flip, -estimate, estimate),
            lwr_new = ifelse(flip, -upr, lwr),
            upr = ifelse(flip, -lwr, upr), # still the original lwr here
            lwr = lwr_new
        ) |>
        select(-lwr_new) |>
        mutate(
            FE = lancet_signs(midline_decimal(fix_zero(sprintf(
                "%.2f (%.2f to %.2f)",
                estimate,
                lwr,
                upr
            ))))
        )

    fe_est |>
        saveRDS(here::here(doc_dir, "fe_tab.rds"))

    #### Combined tables document for submission ####
    # Lancet asks for tables in a Word document separate from the main text
    lancet_docx(
        list(
            list(
                title = paste0(
                    "Table 1: Prevalence of self-reported climate anxiety ",
                    "among adolescents in California by characteristic, ",
                    "California Health Interview Survey 2021–23"
                ),
                ft = tab1_out$flextable,
                notes = tab1_out$notes,
                landscape = tab1_out$landscape
            ),
            list(
                title = paste0(
                    "Table 2: Multivariable logistic regression of factors ",
                    "associated with climate anxiety"
                ),
                ft = glm_tab_ft,
                notes = glm_notes
            )
        ),
        path = here::here(doc_dir, "tables_lancet.docx")
    )

    #### Generate Forest Plots ####
    stopifnot(
        "No BH p values below cutoff" = any(glm_lor_table$bh_pvalue < 0.05)
    )
    # forest plot
    forest_sig <- glm_lor_table |>
        mutate(
            term = forcats::fct_recode(
                term,
                "Age: 15–17" = "15–17",
                "Parents attended college" = "Attended college"
            )
        ) |>
        filter(bh_pvalue < 0.05 & !is.na(bh_pvalue)) |>
        filter(
            !(label %in%
                c(
                    "Tract deviation from county‡",
                    "County-average temperature‡"
                ))
        ) |>
        mutate(label = if_else(term != " ", term, label)) |>
        mutate(
            label = stringr::str_wrap(label, width = 20),
            label = factor(label, levels = rev(sort(unique(label))))
        ) |>
        mutate(label = forcats::fct_reorder(label, e_est)) |>
        ggplot(aes(x = e_est, y = (label))) +
        geom_vline(xintercept = 1, linetype = 2, color = "gray50") +
        geom_errorbar(
            aes(xmin = e_lwr, xmax = e_upr),
            height = 0.0,
            orientation = "y"
        ) +
        geom_point(size = 2) +
        scale_x_log10(labels = label_midline) +
        labs(x = "Odds ratio (95% CI)", y = NULL) +
        theme_bw(10) +
        theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()
        )

    cairo_pdf(here::here(fig_dir, "forest_plots.pdf"), height = 5, width = 4)
    forest_sig |> print()
    dev.off()

    png(
        here::here(fig_dir, "forest_plots.png"),
        height = 5 * 600,
        width = 4 * 600,
        res = 600
    )
    forest_sig |> print()
    dev.off()

    #### Clean up maps ####
    # get prism vars for map
    aux_data <- readRDS(here::here("data", "auxiliary_data.rds"))
    census_shapefile <- aux_data$census_ca
    county_shapefile <- aux_data$county_ca

    census_temp <- aux_data$map_census_temp |>
        select(year, tmax, tract10) |>
        left_join(y = census_shapefile, by = "tract10") |>
        sf::st_as_sf()
    census_heatwave <- aux_data$map_census_heat |>
        select(year, days_above32, tract10) |>
        left_join(y = census_shapefile, by = "tract10") |>
        sf::st_as_sf()

    # load anxiety by country
    county_result_year <- utils::read.csv(here::here(
        "Outputs",
        "climiate_anxiety_map_data.csv"
    ))

    # create shapes
    california_heatmap_year <- full_join(
        county_shapefile,
        county_result_year,
        by = "county"
    )

    # check for all missing counties
    missing_counties <- california_heatmap_year |>
        group_by(county) |>
        filter(length(year) < 3) |>
        dplyr::select(county, year)

    for (i in unique(missing_counties$county)) {
        missing_years <- missing_counties |> filter(county == i) |> pull(year)
        for (j in 2021:2023) {
            if (!(j %in% missing_years)) {
                california_heatmap_year <- california_heatmap_year |>
                    bind_rows(
                        california_heatmap_year |>
                            filter(county == i) |>
                            mutate(year = j, ClimateAnxiety = NA)
                    )
            }
        }
    }

    # get centroids for points
    county_centers_year <- sf::st_centroid(california_heatmap_year)

    county_plot_year_heatwave <- ggplot(
        california_heatmap_year |>
            filter(complete.cases(year)) |>
            mutate(`Climate anxiety` = ClimateAnxiety)
    ) +
        geom_sf(
            data = census_heatwave |>
                mutate(year = as.numeric(year), `Heatwave days` = days_above32),
            aes(fill = `Heatwave days`),
            color = NA
        ) +
        geom_sf(fill = NA, color = "black") +
        geom_sf(
            data = county_centers_year |>
                filter(complete.cases(year)) |>
                mutate(`Climate anxiety` = ClimateAnxiety),
            aes(size = `Climate anxiety`),
            shape = 21,
            fill = "#2f2f2f",
            color = "white",
            stroke = 0.25,
            alpha = 0.85
        ) +
        scale_size(
            range = c(1.2, 7),
            breaks = c(0, .25, .5, .75, 1),
            labels = label_midline
        ) +
        scale_fill_distiller(palette = "Spectral", labels = label_midline) +
        theme_minimal(10) +
        theme(
            panel.grid = element_blank(), # Remove gridlines
            axis.title = element_blank(), # Remove axis titles
            axis.text = element_blank(), # Remove axis text (longitude/latitude labels)
            axis.ticks = element_blank() # Remove axis ticks
        ) +
        facet_wrap(~year, ncol = 2) +
        theme(legend.box = "horizontal")

    county_plot_year_tmax <-
        ggplot(
            california_heatmap_year |>
                filter(complete.cases(year)) |>
                mutate(`Climate anxiety` = ClimateAnxiety)
        ) +
        geom_sf(
            data = census_temp |>
                mutate(year = as.numeric(year), `Avg Tmax` = tmax),
            aes(fill = `Avg Tmax`),
            color = NA
        ) +
        geom_sf(fill = NA, color = "black") +
        geom_sf(
            data = county_centers_year |>
                filter(complete.cases(year)) |>
                mutate(`Climate anxiety` = ClimateAnxiety),
            aes(size = `Climate anxiety`),
            shape = 21,
            fill = "#2f2f2f",
            color = "white",
            stroke = 0.25,
            alpha = 0.85
        ) +
        scale_size(
            range = c(1.2, 7),
            breaks = c(0, .25, .5, .75, 1),
            labels = label_midline
        ) +
        scale_fill_distiller(palette = "Spectral", labels = label_midline) +
        theme_minimal(10) +
        theme(
            panel.grid = element_blank(), # Remove gridlines
            axis.title = element_blank(), # Remove axis titles
            axis.text = element_blank(), # Remove axis text (longitude/latitude labels)
            axis.ticks = element_blank() # Remove axis ticks
        ) +
        facet_wrap(~year, ncol = 2) +
        theme(legend.box = "horizontal")

    final_tmax_plot <- fix_reposition_legend(
        county_plot_year_tmax,
        position = "center",
        panel = "panel-2-2",
        plot = FALSE
    )

    final_heatwave_plot <- fix_reposition_legend(
        county_plot_year_heatwave,
        position = "center",
        panel = "panel-2-2",
        plot = FALSE
    )

    pdf(here::here("Figures", "heatwave_map2.pdf"), width = 6, height = 6)
    grid::grid.draw(final_heatwave_plot)
    dev.off()

    pdf(here::here("Figures", "tmax_map2.pdf"), width = 6, height = 6)
    grid::grid.draw(final_tmax_plot)
    dev.off()

    png(
        here::here("Figures", "heatwave_map2.png"),
        width = 6 * 600,
        height = 6 * 600,
        res = 600
    )
    grid::grid.draw(final_heatwave_plot)
    dev.off()

    png(
        here::here("Figures", "tmax_map2.png"),
        width = 6 * 600,
        height = 6 * 600,
        res = 600
    )
    grid::grid.draw(final_tmax_plot)
    dev.off()
}
