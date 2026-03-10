library(ggplot2)
library(dplyr)
library(here)
library(gt)
library(officer)

#### function ####
recover_df <- function(q, p) {
    pt_val <- p * 0.5
    logp <- log(pt_val)

    obj <- function(x) {
        (logp - stats::pt(abs(q), x, lower.tail = FALSE, log.p = TRUE))^2
    }
    rs <- optimize(f = obj, interval = c(1, 1e6))
    return(rs$minimum)
}

#### output dir ####
doc_dir <- here::here("Documents")
dir.create(doc_dir, showWarnings = FALSE)

#### unzip and transfer outputs ####
if (dir.exists(here::here("Outputs"))) {
    unlink(here::here("Outputs"), recursive = TRUE, force = TRUE)
}
dir.create(here::here("Outputs"))
unzip(
    here::here("Outputs_DAC_original", "DAC250735508_20251105_JR.zip"),
    exdir = here::here("Outputs"),
    junkpaths = TRUE
)

#### load table 1 information ####
tabl_1_long <- utils::read.csv(here::here("Outputs", "table1.csv"))
tab1_raw <- tabl_1_long |>
    tidyr::pivot_wider(
        id_cols = c("label", "variable"),
        names_from = c(year, tf45),
        values_from = c("N", "perc")
    ) |>
    select(variable, label, everything()) |>
    mutate(
        variable = forcats::fct_recode(
            factor(variable),
            "Rural/Urban" = "Rural/Urban (Claritas ZIP, 2-level)"
        )
    ) |>
    mutate(
        label = forcats::fct_recode(
            factor(label),
            "Grade 12/H.S. Diploma" = "Grade 12/h.s. Diploma",
            "AA or AS Degree" = "Aa Or As Degree",
            "BA or BS Degree" = "Ba Or Bs Degree",
            "MA or MS Degree" = "Ma Or Ms Degree",
            "Ph.D. or Equivalent" = "Ph.d. Or Equivalent"
        )
    ) |>
    mutate(
        label_order = dplyr::if_else(
            as.character(variable) == "Parents' Educational Attainment",
            match(
                as.character(label),
                c(
                    "No Formal Education",
                    "Grade 1-8",
                    "Grade 9-11",
                    "Grade 12/H.S. Diploma",
                    "Some College",
                    "Vocational School",
                    "AA or AS Degree",
                    "BA or BS Degree",
                    "Some Grad. School",
                    "MA or MS Degree",
                    "Ph.D. or Equivalent"
                )
            ),
            NA_integer_
        )
    ) |>
    arrange(
        variable,
        dplyr::coalesce(label_order, 9999L),
        as.character(label)
    ) |>
    select(-label_order)

tab1 <- tab1_raw |>
    gt::gt(rowname_col = "label", groupname_col = "variable") |>
    gt::tab_stubhead(label = "") |>
    gt::fmt_percent(
        columns = starts_with("perc_"),
        decimals = 1,
        scale_values = FALSE
    ) |>
    gt::fmt_number(columns = starts_with("N_"), decimals = 1) |>
    gt::cols_merge(
        columns = c(N_2021_Yes, perc_2021_Yes),
        pattern = "{1} ({2})"
    ) |>
    gt::cols_merge(
        columns = c(N_2021_No, perc_2021_No),
        pattern = "{1} ({2})"
    ) |>
    gt::cols_merge(
        columns = c(N_2022_Yes, perc_2022_Yes),
        pattern = "{1} ({2})"
    ) |>
    gt::cols_merge(
        columns = c(N_2022_No, perc_2022_No),
        pattern = "{1} ({2})"
    ) |>
    gt::cols_merge(
        columns = c(N_2023_Yes, perc_2023_Yes),
        pattern = "{1} ({2})"
    ) |>
    gt::cols_merge(
        columns = c(N_2023_No, perc_2023_No),
        pattern = "{1} ({2})"
    ) |>
    gt::tab_spanner(
        label = c("2021"),
        columns = c(N_2021_Yes, N_2021_No)
    ) |>
    gt::tab_spanner(
        label = c("2022"),
        columns = c(N_2022_Yes, N_2022_No)
    ) |>
    gt::tab_spanner(
        label = c("2023"),
        columns = c(N_2023_Yes, N_2023_No)
    ) |>
    gt::cols_label(
        N_2021_Yes = "Climate Anx.: Yes",
        N_2021_No = "No"
    ) |>
    gt::cols_label(
        N_2022_Yes = "Yes",
        N_2022_No = "No"
    ) |>
    gt::cols_label(
        N_2023_Yes = "Yes",
        N_2023_No = "No"
    )

# output to html
tab1 |>
    gt::gtsave(filename = here::here(doc_dir, "table1.html"))

tab1 |>
    gt::gtsave(filename = here::here(doc_dir, "table1.tex"))

tab1 |>
    gt::gtsave(filename = here::here(doc_dir, "table1.docx"))

#### Load Regression Results Files ####
# glm model
# estimates from model
glm_coef <- utils::read.csv(
    file = here::here("Outputs", "glm_model_summary.csv")
)
# vcov for predictions
glm_vcov <- utils::read.csv(file = here::here("Outputs", "glm_vcov.csv")) # un

# re model
fe <- read.csv(here::here("Outputs", "fixef_coef.csv"))
V_fe <- read.csv(here::here("Outputs", "mixef_vcov.csv"))
beta_me <- utils::read.csv(file = here::here("Outputs", "mixef_coef.csv"))
rep_beta_me <- utils::read.csv(
    file = here::here("Outputs", "mixef_replicate_coef.csv")
)

#### generate tables for glm coeficients ####
glm_lor_table <- glm_coef |>
    rename(
        term = X,
        estimate = Estimate,
        std.error = Std..Error,
        statistic = t.value,
        p.value = Pr...t..
    ) |>
    mutate(
        df = sapply(1:n(), function(i) recover_df(statistic[i], p.value[i])),
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
                "ombsrreoAmerican Indian",
                "ombsrreoAsian",
                "ombsrreoPacific Islander",
                "ombsrreoTwo Or More Races",
                "ombsrreoWhite",
                "sch_typPublic School",
                "sch_typPrivate School",
                "sch_typOther",
                "ahedtc_binaryCollege or more",
                "povll_binary300% FPL And Above",
                "lnghmt_binaryNon-English",
                "ur_clrt2Rural",
                "uninsuredYes",
                "health_officePrimary care office",
                "tf9No",
                "scale(as.numeric(tl25))",
                "scale(as.numeric(tl27))",
                "tl50No",
                "scale(as.numeric(tl53))",
                "scale(as.numeric(tq10))",
                "scale(as.numeric(tq11))",
                "scale(as.numeric(tq14))",
                "scale(as.numeric(tq16))",
                "tl10No",
                "school_last_weekYes",
                "scale(I(as.numeric(school_last_week == \"Yes\")) * tb4)",
                "scale(as.numeric(tq15))",
                "scale(tmax_tract10_prior_90_days_count32_delta)",
                "scale(tmax_tract10_prior_90_days_mean_delta)",
                "scale(tmax_tract10_prior_yr_mean_delta)",
                "scale(tmax_tract10_prior_yr_count32_delta)",
                "scale(tmax_county_prior_90_days_count32_delta)",
                "scale(tmax_county_prior_90_days_mean_delta)",
                "scale(tmax_county_prior_yr_mean_delta)",
                "scale(tmax_county_prior_yr_count32_delta)"
            ),
        label = term,
        label = forcats::fct_recode(
            label,
            "Intercept" = "(Intercept)",
            "Age" = "age_group15-17",
            "Sex" = "srsexFemale",
            "Race" = "ombsrreoWhite",
            "Race" = "ombsrreoAfrican American",
            "Race" = "ombsrreoAmerican Indian",
            "Race" = "ombsrreoAsian",
            "Race" = "ombsrreoPacific Islander",
            "Race" = "ombsrreoTwo Or More Races",
            "School Type" = "sch_typPublic School",
            "School Type" = "sch_typPrivate School",
            "School Type" = "sch_typOther",
            "Parent's Education" = "ahedtc_binaryCollege or more",
            "FPL" = "povll_binary300% FPL And Above",
            "Language at Home" = "lnghmt_binaryNon-English",
            "Rural Zip Code" = "ur_clrt2Rural",
            "Cares Deeply About Community Issues" = "scale(as.numeric(tl25))",
            "Believes Can Make a Difference" = "scale(as.numeric(tl27))",
            "Volunteered to Solve Community Problem" = "tl50No",
            "Can Contact Gov't to Solve Problem" = "scale(as.numeric(tl53))",
            "How Often Talk to Family About Feelings" = "scale(as.numeric(tq10))",
            "How Often Felt Supported by Family" = "scale(as.numeric(tq11))",
            "How Often Felt Supported by Friends" = "scale(as.numeric(tq14))",
            "How Often Enjoyed Community Traditions" = "scale(as.numeric(tq16))",
            "Uninsured" = "uninsuredYes",
            "Primary care access" = "health_officePrimary care office",
            "Did not delay care" = "tf9No",
            "Attended School Last Week" = "school_last_weekYes",
            "Number School Days Missed for Health" = "scale(I(as.numeric(school_last_week == \"Yes\")) * tb4)",
            "Did Not Participate in Clubs in Last Year" = "tl10No",
            "How Often Felt A Sense of Belonging at School" = "scale(as.numeric(tq15))",
            "Census Tract Heatwave" = "scale(tmax_tract10_prior_90_days_count32_delta)",
            "Census Tract Heatwave" = "scale(tmax_tract10_prior_90_days_mean_delta)",
            "Census Tract Heatwave" = "scale(tmax_tract10_prior_yr_mean_delta)",
            "Census Tract Heatwave" = "scale(tmax_tract10_prior_yr_count32_delta)",
            "County Heatwave" = "scale(tmax_county_prior_90_days_count32_delta)",
            "County Heatwave" = "scale(tmax_county_prior_90_days_mean_delta)",
            "County Heatwave" = "scale(tmax_county_prior_yr_mean_delta)",
            "County Heatwave" = "scale(tmax_county_prior_yr_count32_delta)"
        ),
        term = forcats::fct_recode(
            term,
            " " = "(Intercept)",
            "15—17" = "age_group15-17",
            "Female" = "srsexFemale",
            "White" = "ombsrreoWhite",
            "African American" = "ombsrreoAfrican American",
            "Native American" = "ombsrreoAmerican Indian",
            "Asian" = "ombsrreoAsian",
            "Pacific Islander" = "ombsrreoPacific Islander",
            "Two or More Races" = "ombsrreoTwo Or More Races",
            "Public" = "sch_typPublic School",
            "Private" = "sch_typPrivate School",
            "Other" = "sch_typOther",
            "College or More" = "ahedtc_binaryCollege or more",
            "300% And Above" = "povll_binary300% FPL And Above",
            "Non-English" = "lnghmt_binaryNon-English",
            "Rural" = "ur_clrt2Rural",
            " " = "scale(as.numeric(tl25))",
            " " = "scale(as.numeric(tl27))",
            "No" = "tl50No",
            " " = "scale(as.numeric(tl53))",
            " " = "scale(as.numeric(tq10))",
            " " = "scale(as.numeric(tq11))",
            " " = "scale(as.numeric(tq14))",
            " " = "scale(as.numeric(tq16))",
            "Yes" = "uninsuredYes",
            "Yes" = "health_officePrimary care office",
            "No" = "tf9No",
            "Yes" = "school_last_weekYes",
            " " = "scale(I(as.numeric(school_last_week == \"Yes\")) * tb4)",
            " " = "tl10No",
            " " = "scale(as.numeric(tq15))",
            "Change in Typical Number of Heatwaves (Prev. 90 days)" = "scale(tmax_tract10_prior_90_days_count32_delta)",
            "Change in Typical Average Temp. (Prev. 90 days)" = "scale(tmax_tract10_prior_90_days_mean_delta)",
            "Change in Typical Average Temp. (Prev. Year)" = "scale(tmax_tract10_prior_yr_mean_delta)",
            "Change in Typical Number of Heatwaves (Prev. Year)" = "scale(tmax_tract10_prior_yr_count32_delta)",
            "Change in Typical Number of Heatwaves (Prev. 90 days)" = "scale(tmax_county_prior_90_days_count32_delta)",
            "Change in Typical Average Temp. (Prev. 90 days)" = "scale(tmax_county_prior_90_days_mean_delta)",
            "Change in Typical Average Temp. (Prev. Year)" = "scale(tmax_county_prior_yr_mean_delta)",
            "Change in Typical Number of Heatwaves (Prev. Year)" = "scale(tmax_county_prior_yr_count32_delta)"
        )
    ) |>
    mutate(
        flip = label %in%
            c(
                "Cares Deeply About Community Issues",
                "Believes Can Make a Difference",
                "Volunteered to Solve Community Problem",
                "Can Contact Gov't to Solve Problem",
                "How Often Talk to Family About Feelings",
                "How Often Felt Supported by Family",
                "How Often Felt Supported by Friends",
                "How Often Enjoyed Community Traditions",
                "How Often Felt A Sense of Belonging at School"
            )
    ) |>
    mutate(e_est = ifelse(flip, exp(-estimate), e_est)) |>
    mutate(e_lwr = ifelse(flip, exp(-upr), e_lwr)) |>
    mutate(e_upr = ifelse(flip, exp(-lwr), e_upr)) |>
    gt::gt(
        groupname_col = "label",
        rowname_col = "term"
    ) |>
    gt::fmt_number(
        columns = c(
            e_est,
            e_lwr,
            e_upr,
            estimate,
            std.error,
            statistic,
            p.value,
            lwr,
            upr
        ),
        decimals = 2
    ) |>
    gt::fmt(
        columns = c(e_est, e_lwr, e_upr),
        fns = function(x) {
            x <- as.numeric(x)
            big <- abs(x) >= 1e3 # pick your cutoff
            small <- x < 0.01
            out <- formatC(x, format = "f", digits = 3)
            out[big] <- formatC(x[big], format = "e", digits = 2)
            out[small] <- formatC(x[small], format = "e", digits = 2)
            out
        }
    ) |>
    gt::fmt(columns = p.value, fns = function(x) {
        x <- as.numeric(x) |> p.adjust(method = "BH")
        ifelse(
            is.na(x),
            NA_character_,
            ifelse(x < 0.001, "<0.001", format(round(x, 3), nsmall = 3))
        )
    }) |>
    gt::cols_move_to_end(p.value)

glm_tab <- glm_lor_table |>
    gt::cols_move_to_start(c(term, e_est)) |>
    gt::cols_merge(columns = c(e_lwr, e_upr), pattern = "({1}, {2})") |>
    gt::cols_hide(
        columns = c(df, std.error, statistic, estimate, upr, lwr, flip)
    ) |>
    gt::cols_label(
        # readable table names
        term = "Variable",
        e_est = "O.R.",
        e_lwr = "Conf. Int",
        p.value = "p-value"
    ) |>
    tab_options(row_group.as_column = TRUE)
print(glm_tab)

glm_tab |> gt::gtsave(filename = here::here(doc_dir, "glm_tab.docx"))
glm_tab |> gt::gtsave(filename = here::here(doc_dir, "glm_tab.tex"))
glm_tab |> gt::gtsave(filename = here::here(doc_dir, "glm_tab.html"))
