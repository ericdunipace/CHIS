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

#### load Functions ####
source(here::here("R", "Functions.R"))

#### output dir ####
doc_dir <- here::here("Documents")
dir.create(doc_dir, showWarnings = FALSE)

fig_dir <- here::here("Figures")
dir.create(fig_dir, showWarnings = FALSE)

#### unzip and transfer outputs ####
if (dir.exists(here::here("Outputs"))) {
    unlink(here::here("Outputs"), recursive = TRUE, force = TRUE)
}
dir.create(here::here("Outputs"))
unzip(
    here::here("Outputs_DAC_original", "DAC250735508_20260327_JR.zip"),
    exdir = here::here("Outputs"),
    junkpaths = TRUE
)

#### load table 1 information ####
tabl_1_long <- utils::read.csv(here::here("Outputs", "table1.csv"))
tab1_raw <- tabl_1_long |>
    mutate(N = if_else(variable != "Age" & is.na(N), 0, N)) |>
    mutate(perc = if_else(variable != "Age" & is.na(perc), 0, perc)) |>
    tidyr::pivot_wider(
        id_cols = c("label", "variable"),
        names_from = c(year, tf45),
        values_from = c("N", "perc", "mean", "sd")
    ) |>
    select(variable, label, everything()) |>
    mutate(
        variable = if_else(
            !is.na(label) & label == "" & variable == "Age",
            "Total (%)",
            variable
        )
    ) |>
    filter(!(variable == "Age" & !is.na(label))) |>
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
    select(-label_order) |>
    slice(c(which(variable != "Total (%)"), which(variable == "Total (%)"))) |>
    mutate(label = if_else(variable == "Age", "", label)) |>
    mutate(variable = ifelse(variable == "Age", "Age*", as.character(variable)))


tab1_display <- tab1_raw |>
    mutate(
        stat_2021_Yes = make_stat_display(
            N_2021_Yes,
            perc_2021_Yes,
            mean_2021_Yes,
            sd_2021_Yes
        ),
        stat_2021_No = make_stat_display(
            N_2021_No,
            perc_2021_No,
            mean_2021_No,
            sd_2021_No
        ),
        stat_2022_Yes = make_stat_display(
            N_2022_Yes,
            perc_2022_Yes,
            mean_2022_Yes,
            sd_2022_Yes
        ),
        stat_2022_No = make_stat_display(
            N_2022_No,
            perc_2022_No,
            mean_2022_No,
            sd_2022_No
        ),
        stat_2023_Yes = make_stat_display(
            N_2023_Yes,
            perc_2023_Yes,
            mean_2023_Yes,
            sd_2023_Yes
        ),
        stat_2023_No = make_stat_display(
            N_2023_No,
            perc_2023_No,
            mean_2023_No,
            sd_2023_No
        )
    ) |>
    select(
        variable,
        label,
        stat_2021_Yes,
        stat_2021_No,
        stat_2022_Yes,
        stat_2022_No,
        stat_2023_Yes,
        stat_2023_No
    )

saveRDS(tab1_display, file = here::here("Outputs", "table1_display.rds"))

tab1 <- tab1_display |>
    gt::gt(rowname_col = "label", groupname_col = "variable") |>
    gt::tab_stubhead(label = "") |>
    gt::tab_spanner(
        label = c("2021"),
        columns = c(stat_2021_Yes, stat_2021_No)
    ) |>
    gt::tab_spanner(
        label = c("2022"),
        columns = c(stat_2022_Yes, stat_2022_No)
    ) |>
    gt::tab_spanner(
        label = c("2023"),
        columns = c(stat_2023_Yes, stat_2023_No)
    ) |>
    gt::cols_label(
        stat_2021_Yes = "Yes",
        stat_2021_No = "No"
    ) |>
    gt::cols_label(
        stat_2022_Yes = "Yes",
        stat_2022_No = "No"
    ) |>
    gt::cols_label(
        stat_2023_Yes = "Yes",
        stat_2023_No = "No"
    ) |>
    gt::fmt_number(sep_mark = ",")
  

# output to formats
col_names <- tab1$`_boxhead`$column_label |>
  unlist()

col_names[1:2] <- " "

tab1 |>
    saveRDS(file = here::here(doc_dir, "table1_gt.rds"))

tab1 |>
    gt::gtsave(filename = here::here(doc_dir, "table1.html"))

tab1df <- tab1 |>
  gt::extract_body() |>
  mutate(`::rowname::` = if_else(grepl("&amp;", `::rowname::`),gsub("&amp;", "&",`::rowname::`),  `::rowname::`) )

group_index <- tab1df %>%
  mutate(row = row_number()) %>%
  summarise(start = min(row), end = max(row), .by = `::group_id::`)

tab1df |>
  setNames(col_names) |>
  subset(select = -1) |>
  kableExtra::kbl(format = "latex", booktabs = TRUE, escape = TRUE,
                  align = c("l", rep("r", 6))) |>
  purrr::reduce(
    seq_len(nrow(group_index)),
    .init = _,
    .f = function(tbl, i) {
      kableExtra::group_rows(
        tbl,
        group_label = group_index$`::group_id::`[i],
        start_row   = group_index$start[i],
        end_row     = group_index$end[i]
      )
    }
  ) |>
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down")) |>
  kableExtra::add_header_above(c(" " = 1, "2021" = 2, "2022" = 2, "2023" = 2)) |>
  kableExtra::row_spec(0, align = "c") |>
  kableExtra::row_spec(seq(1, nrow(tab1df), 2), background = "#f5f5f5") |>
  cat(file = here::here(doc_dir, "table1.tex"))

tab1ft <- tab1df |> 
  flextable::as_grouped_data(groups = "::group_id::") |>
  flextable::flextable() |> 
  flextable::set_header_labels(
    "::group_id::" = "",
    "::rowname::" = "",
    stat_2021_Yes = "Yes",
    stat_2021_No = "No",
    stat_2022_Yes = "Yes",
    stat_2022_No = "No",
    stat_2023_Yes = "Yes",
    stat_2023_No = "No"
  ) |>
  flextable::add_header_row(
    values = c("", "2021", "2022", "2023"),
    colwidths = c(2, 2, 2, 2)
  ) |>
  flextable::vline(j = c(2, 4,6), border = fp_border(color = "gray50", width = 1), part = "all") |>
  flextable::fit_to_width(max_width = 6.5) |>
  flextable::fontsize(size = 8, part = "all")

tab1ft |>
  flextable::save_as_docx(path = here::here(doc_dir, "table1.docx"))

tab1ft |>
  saveRDS(file = here::here(doc_dir, "table1_ft.rds"))

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
  ~variable, ~group,
  "tl25_pos", "Modifiable Protective Factors",
  "tl27_pos", "Modifiable Protective Factors",
  "tl50", "Modifiable Protective Factors",
  "tl53_pos", "Modifiable Protective Factors",
  "tq10_pos", "Modifiable Protective Factors",
  "tq11_pos", "Modifiable Protective Factors",
  "tq14_pos", "Modifiable Protective Factors",
  "tq16_pos", "Modifiable Protective Factors",
  "uninsured", "Access to Care",
  "health_office", "Access to Care",
  "tf9", "Access to Care",
  "school_last_week", "Civic Engagement",
  # "scale(I(as.numeric(school_last_week ==  'Yes')) * tb4)", 
  "I((as.numeric(school_last_week ==  'Yes')) * tb4)", 
  "Civic Engagement",
  "tl10", "Civic Engagement",
  "tq15_pos", "Civic Engagement"
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
                "ombsrreoAmerican Indian/Pacific Islander",
                "ombsrreoAsian",
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
                "tl25_posYes",
                "tl27_posYes",
                "tl50No",
                "tl53_posYes",
                "tq10_posYes",
                "tq11_posYes",
                "tq14_posYes",
                "tq16_posYes",
                "tl10No",
                "school_last_weekYes",
                "I((as.numeric(school_last_week == \"Yes\")) * tb4)",
                # "scale(I(as.numeric(school_last_week == \"Yes\")) * tb4)",
                "tq15_posYes",
                "tmax_tract10_prior_90_days_count32_delta",
                "tmax_tract10_prior_90_days_mean_delta",
                "tmax_tract10_prior_yr_count32_delta",
                "tmax_tract10_prior_yr_mean_delta",
                "tmax_county_prior_90_days_count32_delta",
                "tmax_county_prior_90_days_mean_delta",
                "tmax_county_prior_yr_count32_delta",
                "tmax_county_prior_yr_mean_delta"
            ),
        label = term,
        label = forcats::fct_recode(
            label,
            "Intercept" = "(Intercept)",
            "Age Group: 15–17" = "age_group15-17",
            "Female" = "srsexFemale",
            "Race" = "ombsrreoWhite",
            "Race" = "ombsrreoAfrican American",
            "Race" = "ombsrreoAmerican Indian/Pacific Islander",
            "Race" = "ombsrreoAsian",
            "Race" = "ombsrreoTwo Or More Races",
            "School Type" = "sch_typPublic School",
            "School Type" = "sch_typPrivate School",
            "School Type" = "sch_typOther",
            "Parents Attended College" = "ahedtc_binaryCollege or more",
            "300% FPL And Above" = "povll_binary300% FPL And Above",
            "Speaks English at Home" = "lnghmt_binaryNon-English", # note we flip sign below
            "Lives in a Rural Zip Code" = "ur_clrt2Rural",
            "Modifiable Protective Factors" = "tl25_posYes",
            "Modifiable Protective Factors" = "tl27_posYes",
            "Modifiable Protective Factors" = "tl50No", # note we flip the sign below
            "Modifiable Protective Factors" = "tl53_posYes",
            "Modifiable Protective Factors" = "tq10_posYes",
            "Modifiable Protective Factors" = "tq11_posYes",
            "Modifiable Protective Factors" = "tq14_posYes",
            "Modifiable Protective Factors" = "tq16_posYes",
            "Access to Care" = "uninsuredYes",
            "Access to Care" = "health_officePrimary care office",
            "Access to Care" = "tf9No",
            "Civic Engagement" = "school_last_weekYes",
            # "Civic Engagement" = "scale(I(as.numeric(school_last_week == \"Yes\")) * tb4)",
            "Civic Engagement" =  "I((as.numeric(school_last_week == \"Yes\")) * tb4)",

            "Civic Engagement" = "tl10No",
            "Civic Engagement" = "tq15_posYes",
            "Census Tract Climate" = "tmax_tract10_prior_90_days_count32_delta",
            "Census Tract Climate" = "tmax_tract10_prior_90_days_mean_delta",
            "Census Tract Climate" = "tmax_tract10_prior_yr_mean_delta",
            "Census Tract Climate" = "tmax_tract10_prior_yr_count32_delta",
            "County Climate" = "tmax_county_prior_90_days_count32_delta",
            "County Climate" = "tmax_county_prior_90_days_mean_delta",
            "County Climate" = "tmax_county_prior_yr_mean_delta",
            "County Climate" = "tmax_county_prior_yr_count32_delta"
        ),
        term = forcats::fct_recode(
            term,
            " " = "(Intercept)",
            " " = "age_group15-17",
            " " = "srsexFemale",
            "African American" = "ombsrreoAfrican American",
            # "Native American" = "ombsrreoAmerican Indian",
            # "Pacific Islander" = "ombsrreoPacific Islander",
            "Native American/Pacific Islander" = "ombsrreoAmerican Indian/Pacific Islander",
            "Asian" = "ombsrreoAsian",
            "Two or More Races" = "ombsrreoTwo Or More Races",
            "White" = "ombsrreoWhite",
            "Public" = "sch_typPublic School",
            "Private" = "sch_typPrivate School",
            "Other" = "sch_typOther",
            " " = "ahedtc_binaryCollege or more",
            " " = "povll_binary300% FPL And Above",
            " " = "lnghmt_binaryNon-English",
            " " = "ur_clrt2Rural",
            "Cares Deeply About Community Issues" = "tl25_posYes",
            "Believes Can Make a Difference" = "tl27_posYes",
            "Volunteered to Solve Community Problem" = "tl50No", # note we flip the sign below
            "Can Contact Gov't to Solve Problem" = "tl53_posYes",
            "Able to Talk to Family About Feelings" = "tq10_posYes",
            "Felt Supported by Family" = "tq11_posYes",
            "Felt Supported by Friends" = "tq14_posYes",
            "Enjoyed Community Traditions" = "tq16_posYes",
            "Uninsured" = "uninsuredYes",
            "Goes to PCP for Health Care" = "health_officePrimary care office",
            "Did Not Delay Medical Care" = "tf9No",
            "Attended School Last Week" = "school_last_weekYes",
            # "Number School Days Missed for Health (normalized)" = "scale(I(as.numeric(school_last_week == \"Yes\")) * tb4)",
            "Number School Days Missed for Health" = "I((as.numeric(school_last_week == \"Yes\")) * tb4)",
            "Participated in Clubs in Last Year" = "tl10No", #note flip sign below
            "Often Felt A Sense of Belonging at School" = "tq15_posYes",
            "Change in Typical Number of Heatwaves (Prev. 90 days)" = "tmax_tract10_prior_90_days_count32_delta",
            "Change in Typical Average Temp. (Prev. 90 days)" = "tmax_tract10_prior_90_days_mean_delta",
            "Change in Typical Number of Heatwaves (Prev. Year)" = "tmax_tract10_prior_yr_count32_delta",
            "Change in Typical Average Temp. (Prev. Year)" = "tmax_tract10_prior_yr_mean_delta",
            "Change in Typical Number of Heatwaves (Prev. 90 days)" = "tmax_county_prior_90_days_count32_delta",
            "Change in Typical Average Temp. (Prev. 90 days)" = "tmax_county_prior_90_days_mean_delta",
            "Change in Typical Number of Heatwaves (Prev. Year)" = "tmax_county_prior_yr_count32_delta",
            "Change in Typical Average Temp. (Prev. Year)" = "tmax_county_prior_yr_mean_delta"
        )
    ) |>
    mutate(
        flip = label %in%
            c(
                "Volunteered to Solve Community Problem",
                "Speaks English at Home",
                "Participated in Clubs in Last Year"
            )
    ) |>
    mutate(e_est = ifelse(flip, exp(-estimate), e_est)) |>
    mutate(e_lwr = ifelse(flip, exp(-upr), e_lwr)) |>
    mutate(e_upr = ifelse(flip, exp(-lwr), e_upr)) |>
    mutate(
        bh_pvalue = {
            out <- p.value
            non_intercept <- term != "(Intercept)"
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

race_before <- which(glm_lor_table$label == "Race")[1]
glm_lor_table <- tibble::add_row(
    glm_lor_table,
    term = "Hispanic (Ref.)",
    label = "Race",
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
    glm_lor_table$label == "School Type"
)[1]
glm_lor_table <- tibble::add_row(
    glm_lor_table,
    term = "Inapplicable/Other (Ref.)",
    label = "School Type",
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
        stars
    ) |>
    saveRDS(file = here::here(doc_dir, "glm_lor_table.rds"))

glm_lor_table_gt <- glm_lor_table |>
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
            bh_pvalue,
            lwr,
            upr
        ),
        decimals = 2
    ) |>
    gt::fmt(
        columns = c(e_est, e_lwr, e_upr),
        fns = function(x) {
            x <- as.numeric(x)
            big <- !is.na(x) & abs(x) >= 1e3 # pick your cutoff
            small <- !is.na(x) & x < 0.01
            out <- formatC(x, format = "f", digits = 3)
            out[big] <- formatC(x[big], format = "e", digits = 2)
            out[small] <- formatC(x[small], format = "e", digits = 2)
            out
        }
    ) |>
    gt::fmt(columns = bh_pvalue, fns = function(x) {
        x <- as.numeric(x)
        ifelse(
            is.na(x),
            NA_character_,
            ifelse(x < 0.001, "<0.001", format(round(x, 3), nsmall = 3))
        )
    }) |>
    gt::cols_move_to_end(bh_pvalue) |>
    gt::cols_move_to_end(stars)

glm_tab <- glm_lor_table_gt |>
    gt::cols_move_to_start(c(term, e_est)) |>
    gt::cols_merge(columns = c(e_lwr, e_upr), pattern = "({1}, {2})") |>
    gt::text_transform(
        locations = gt::cells_body(columns = e_lwr),
        fn = function(x) ifelse(x == "(—, —)", "—", x)
    ) |>
    gt::cols_hide(
        columns = c(
            df,
            std.error,
            statistic,
            estimate,
            upr,
            lwr,
            flip,
            p.value
        )
    ) |>
    gt::cols_label(
        # readable table names
        term = "Variable",
        e_est = "O.R.",
        e_lwr = "Conf. Int",
        bh_pvalue = "p-value",
        stars = ""
    ) |>
    gt::sub_missing(missing_text = "—") |>
    gt::tab_source_note(
        source_note = "p-values are Benjamini-Hochberg (BH) corrected for non-intercept coefficients."
    ) |>
    tab_options(row_group.as_column = TRUE)
# print(glm_tab)

glm_tab |> saveRDS(file = here::here(doc_dir, "glm_tab_gt.rds"))

glm_tab |> gt::gtsave(filename = here::here(doc_dir, "glm_tab.html"))


glm_tab_df <- glm_tab |> 
  gt::extract_body()

boxhead <- glm_tab$`_boxhead`
vars_in_order <- names(glm_tab_df)[-1:-2]

label_df <- boxhead |>
  dplyr::filter(var %in% vars_in_order) |>
  dplyr::mutate(var = factor(var, levels = vars_in_order)) |>
  dplyr::arrange(var) |>
  dplyr::transmute(
    var,
    label = column_label,
    column_type = type
  )
colnames(glm_tab_df)[-1:-2] <- label_df$label
colnames(glm_tab_df)[c(2)] <- "  "
colnames(glm_tab_df)[c(6)] <- " "
# colnames(glm_tab_df)[1:2] <- c("", "")

glm_tab_ft <- glm_tab_df |>
  flextable::as_grouped_data(groups = "::group_id::") |>
  flextable::flextable() |>
  flextable::set_header_labels(
    "::group_id::" = " ",
    "::rowname::" = " ",
    `O.R.` = "O.R.",
    `Conf. Int` = "Conf. Int",
    `p-value` = "p-value",
    " " = " "
  )

glm_tab_ft |>
  flextable::save_as_docx(path = here::here(doc_dir, "glm_tab.docx"))

glm_tab_ft |> 
  saveRDS(file = here::here(doc_dir, "glm_tab_ft.rds"))

group_glm <- glm_tab_df %>%
  mutate(row = row_number()) %>%
  summarise(
    start = min(row),
    end   = max(row),
    n     = dplyr::n(),
    .by = `::group_id::`
  )

group_index_glm <- group_glm %>%
  filter(n > 1)

singleton_index_glm <- group_glm %>%
  filter(n == 1) %>%
  pull(`start`)

glm_tab_df |> 
  add_count(`::group_id::`, name = "n_group") %>%
  mutate(
    `  ` = if_else(n_group == 1, `::group_id::`, `  `)
  ) %>%
  mutate(`  `  = if_else(
    n_group == 1,
    kableExtra::cell_spec(`  `, format = "latex", bold = TRUE),
    `  `
  )) |>
  select(-n_group) |>
  subset(select = -1) %>%
  kableExtra::kbl(format = "latex", booktabs = TRUE, 
                  escape = FALSE,
                  align = c(rep("l",2), rep("r",2),"c")) |>
  purrr::reduce(
    seq_len(nrow(group_index_glm)),
    .init = _,
    .f = function(tbl, i) {
      kableExtra::group_rows(
        tbl,
        group_label = group_index_glm$`::group_id::`[i],
        start_row   = group_index_glm$start[i],
        end_row     = group_index_glm$end[i]
      )
    }
  ) |>
  kableExtra::row_spec(0, align = "c") |>
  kableExtra::row_spec(seq(1, nrow(glm_tab_df), 2), background = "#f5f5f5") |>
  kableExtra::kable_styling(latex_options = c("hold_position", "scale_down"),
                            font_size = 9) |>
  cat(file = here::here(doc_dir, "glm_tab.tex"))


#### Generate Forest Plots ####

# forest plot
forest_sig <- glm_lor_table |>
    filter(bh_pvalue < 0.05 & !is.na(bh_pvalue)) |>
    mutate(label = if_else(term != " ", term, label)) |>
    mutate(
        label = stringr::str_wrap(label, width = 20),
        label = factor(label, levels = rev(sort(unique(label))))
    ) |>
    ggplot(aes(x = e_est, y = (label))) +
    geom_vline(xintercept = 1, linetype = 2, color = "gray50") +
    geom_errorbarh(aes(xmin = e_lwr, xmax = e_upr), height = 0.0) +
    geom_point(size = 2) +
    scale_x_log10() +
    labs(x = "Odds Ratio (95% CI)", y = NULL) +
    theme_bw() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

pdf(here::here(fig_dir, "forest_plots.pdf"), height = 5, width = 4)
forest_sig |> print()
dev.off()

png(here::here(fig_dir, "forest_plots.png"), height = 5 * 600, width = 4 * 600, res = 600)
forest_sig |> print()
dev.off()

#### Generate Predicted probabilities ####
# get mean temp data
aux_data <- readRDS(here::here("Data", "auxiliary_data.rds"))
county_heat <- c(
    "tmax_county_prior_90_days_count32_delta", # Tmax above 32 for the 90 days prior to survey date
    "tmax_county_prior_90_days_mean_delta", # Mean Tmaxfor the 90 days prior to survey date
    "tmax_county_prior_yr_mean_delta", # Mean Tmax for the year prior to survey date
    "tmax_county_prior_yr_count32_delta" # Count of Tmax above 32 for the year prior to survey date
)
census_heat <- c(
    "tmax_tract10_prior_90_days_count32_delta", # Tmax above 32 for the 90 days prior to survey date
    "tmax_tract10_prior_90_days_mean_delta", # Mean Tmaxfor the 90 days prior to survey date
    "tmax_tract10_prior_yr_mean_delta", # Mean Tmax for the year prior to survey date
    "tmax_tract10_prior_yr_count32_delta" # Count of Tmax above 32 for the year prior to survey date
)
county_temp <- aux_data$prism$county[, county_heat] |> colMeans(na.rm = TRUE)
census_temp <- aux_data$prism$census[, census_heat] |> colMeans(na.rm = TRUE)
heat <- c(census_temp, county_temp)


# construct dummy vars
age_mode <- mode_from_table1(tabl_1_long, "Age")
age_group_mode <- ifelse(age_mode %in% c("12", "13", "14"), "12-14", "15-17")

mode_lookup <- c(
    age_group = age_group_mode,
    srsex = mode_from_table1(tabl_1_long, "Sex"),
    ombsrreo = mode_from_table1(tabl_1_long, "Ethnicity"),
    sch_typ = mode_from_table1(tabl_1_long, "Type of School Attended"),
    ahedtc_binary = mode_from_table1(
        tabl_1_long,
        "Parents' Educational Attainment",
        collapse_ahedtc_binary
    ),
    povll_binary = mode_from_table1(
        tabl_1_long,
        "Poverty Level",
        collapse_povll_binary
    ),
    lnghmt_binary = mode_from_table1(
        tabl_1_long,
        "Language Spoken at Home",
        collapse_lnghmt_binary
    ),
    ur_clrt2 = mode_from_table1(
        tabl_1_long,
        "Rural/Urban (Claritas ZIP, 2-level)"
    )
)

model_vars <- glm_coef |>
    dplyr::pull(X) |>
    sapply(extract_model_var) |>
    unique() |>
    na.omit() |>
    as.character()

dummy.data <- setNames(
    as.data.frame(as.list(rep(NA, length(model_vars)))),
    model_vars
)

for (var_name in names(mode_lookup)) {
    if (var_name %in% names(dummy.data)) {
        dummy.data[[var_name]] <- mode_lookup[[var_name]]
    }
}

if ("age_group" %in% names(dummy.data)) {
    dummy.data$age_group <- factor(
        dummy.data$age_group,
        levels = c("12-14", "15-17")
    )
}
if ("srsex" %in% names(dummy.data)) {
    dummy.data$srsex <- factor(dummy.data$srsex, levels = c("Male", "Female"))
}
if ("ombsrreo" %in% names(dummy.data)) {
    dummy.data$ombsrreo <- factor(
        dummy.data$ombsrreo,
        levels = c(
            "Hispanic",
            "White",
            "African American",
            "American Indian/Pacific ISlander",
            "Asian",
            "Two Or More Races"
        )
    )
}
if ("sch_typ" %in% names(dummy.data)) {
    dummy.data$sch_typ <- factor(
        dummy.data$sch_typ,
        levels = c("Inapplicable/Other", "Public School", "Private School")
    )
}
if ("ahedtc_binary" %in% names(dummy.data)) {
    dummy.data$ahedtc_binary <- factor(
        dummy.data$ahedtc_binary,
        levels = c("No college", "College or more")
    )
}
if ("povll_binary" %in% names(dummy.data)) {
    dummy.data$povll_binary <- factor(
        dummy.data$povll_binary,
        levels = c("Less than 300% FPL", "300% FPL And Above")
    )
}
if ("lnghmt_binary" %in% names(dummy.data)) {
    dummy.data$lnghmt_binary <- factor(
        dummy.data$lnghmt_binary,
        levels = c("English", "Non-English")
    )
}
if ("ur_clrt2" %in% names(dummy.data)) {
    dummy.data$ur_clrt2 <- factor(
        dummy.data$ur_clrt2,
        levels = c("Urban", "Rural")
    )
}

binary_vars <- c(
    "uninsured",
    "health_office",
    "tf9",
    "tl25_pos",
    "tl27_pos",
    "tl53_pos",
    "tq10_pos",
    "tq11_pos",
    "tq14_pos",
    "tq16_pos",
    "school_last_week",
    "tq15_pos"
)
for (var_name in binary_vars) {
    if (var_name %in% names(dummy.data)) {
        dummy.data[[var_name]] <- factor(
            dummy.data[[var_name]],
            levels = c("No", "Yes")
        )
    }
}
if ("tl50" %in% names(dummy.data)) {
    dummy.data$tl50 <- factor(dummy.data$tl50, levels = c("Yes", "No"))
}
if ("tl10" %in% names(dummy.data)) {
    dummy.data$tl10 <- factor(dummy.data$tl10, levels = c("Yes", "No"))
}

baseline_lookup <- c(
    age_group = "12-14",
    srsex = "Male",
    ombsrreo = "Hispanic",
    sch_typ = "Inapplicable",
    ahedtc_binary = "No college",
    povll_binary = "Less than 300% FPL",
    lnghmt_binary = "English",
    ur_clrt2 = "Urban",
    uninsured = "No",
    health_office = "Not primary care office",
    tf9 = "Yes",
    tl25_pos = "No",
    tl27_pos = "No",
    tl50 = "Yes",
    tl53_pos = "No",
    tq10_pos = "No",
    tq11_pos = "No",
    tq14_pos = "No",
    tq16_pos = "No",
    school_last_week = "No",
    tl10 = "Yes",
    tq15_pos = "No"
)
for (var_name in names(baseline_lookup)) {
    dummy.data <- set_missing_baseline(
        dummy.data,
        var_name,
        baseline_lookup[[var_name]]
    )
}

numeric_baseline_vars <- c(
    "I((as.numeric(school_last_week == \"Yes\")) * tb4)",
    "tmax_tract10_prior_90_days_count32_delta",
    "tmax_tract10_prior_90_days_mean_delta",
    "tmax_tract10_prior_yr_mean_delta",
    "tmax_tract10_prior_yr_count32_delta",
    "tmax_county_prior_90_days_count32_delta",
    "tmax_county_prior_90_days_mean_delta",
    "tmax_county_prior_yr_mean_delta",
    "tmax_county_prior_yr_count32_delta"
)
for (var_name in intersect(numeric_baseline_vars, names(dummy.data))) {
    if (all(is.na(dummy.data[[var_name]]))) {
        if (var_name %in% names(heat)) {
            dummy.data[[var_name]] <- unname(heat[[var_name]])
        } else {
            dummy.data[[var_name]] <- 0
        }
    }
    dummy.data[[var_name]] <- as.numeric(dummy.data[[var_name]])
}

unfilled_vars <- names(dummy.data)[sapply(dummy.data, function(x) {
    all(is.na(x))
})]
if (length(unfilled_vars) > 0) {
    message(
        "Still missing after mode+baseline fill: ",
        paste(unfilled_vars, collapse = ", ")
    )
}

glm_terms <- as.character(glm_coef$X)

dummy_model_matrix <- matrix(
    NA_real_,
    nrow = 1,
    ncol = length(glm_terms),
    dimnames = list("dummy.data", glm_terms)
)
for (ii in seq_along(glm_terms)) {
    dummy_model_matrix[1, ii] <- term_to_design_value(
        glm_terms[ii],
        dummy.data[1, , drop = FALSE]
    )
}
# inspect manually print(dummy_model_matrix)
dummy_model_matrix[, "health_officePrimary care office"] <- 0


#### Create probability figures ####
beta <- glm_coef[, 2]
V <- glm_vcov |> as.matrix()

fem_d <- dummy_model_matrix  %>% 
    {
        .[, "srsexFemale"] <- 1
        .
    }

lp_fem <- mvtnorm::rmvnorm(
    n = 1000,
    mean = sum(fem_d * beta),
    sigma = fem_d %*% V %*% t(fem_d)
)
lp_mal <- mvtnorm::rmvnorm(
    n = 1000,
    mean = sum(dummy_model_matrix * beta),
    sigma = dummy_model_matrix %*% V %*% t(dummy_model_matrix)
)
male_female <- data.frame(
    value = plogis(c(lp_fem, lp_mal)),
    sex = c(rep("Female", 1000), rep("Male", 1000))
)
male_female |> saveRDS(file = here::here(doc_dir, "maleVfemale.rds"))

mf_plot <- male_female |>
    # bind_rows(data.frame(
    #     sex = "Difference",
    #     value = plogis(lp_fem) - plogis(lp_mal)
    # )) |>
    ggplot(aes(x = value, y = sex)) +
    ggdist::stat_pointinterval() +
    theme_bw() +
    xlab("Predicted Probability") +
    ylab("Sex") +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

pdf(file = here::here("Figures", "maleVfemale.pdf"), 3.5, 3.5)
mf_plot |> print()
dev.off()

png(file = here::here("Figures", "maleVfemale.png"), 3.5 * 600, 3.5 * 600, res = 600)
mf_plot |> print()
dev.off()

# plot for days school missed
days_school_missed <- do.call(
    "rbind",
    lapply(
        seq(-2, 2, 0.1),
        function(i) {
            x <- dummy_model_matrix
            # x[, 'scale(I(as.numeric(school_last_week == "Yes")) * tb4)'] <- i
            x[, 'I((as.numeric(school_last_week == "Yes")) * tb4)'] <- i
            x[, "school_last_weekYes"] <- 1
            return(x)
        }
    )
)

lp_school <- mvtnorm::rmvnorm(
    n = 1000,
    mean = (days_school_missed %*% beta),
    sigma = days_school_missed %*% V %*% t(days_school_missed)
)

school_plot_df <- data.frame(
    E = colMeans(lp_school |> plogis()),
    lwr = apply(lp_school |> plogis(), 2, quantile, prob = 0.025),
    upr = apply(lp_school |> plogis(), 2, quantile, prob = 0.975),
    missed = seq(-2, 2, 0.1)
) |>
    mutate(
        label = paste0("mean ", missed, "S.D.s")
    )

school_plot_df |> saveRDS(file = here::here(doc_dir, "school_plot_df.rds"))

school_plot <- school_plot_df |>
    ggplot(aes(y = E, x = missed, ymin = lwr, ymax = upr)) +
    geom_ribbon(alpha = 0.35, color = NA, fill = "#AFCBFF") +
    geom_line(color = "#2F6FB0") +
    scale_x_continuous(
        expand = expansion(mult = c(0.01, 0.01)),
        breaks = scales::pretty_breaks(5),
        labels = \(x) ifelse(x == 0, "Mean", paste0(x, " SD"))
    ) +
    theme_bw() +
    ylab("Predicted Probability") +
    xlab("School Days Missed (in SDs from mean)") +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(
          t = 5.5,
          r = 30,
          b = 5.5,
          l = 5.5
        )
    )

pdf(file = here::here("Figures", "missed_school.pdf"), 3.5, 3.5)
school_plot |> print()
dev.off()

png(file = here::here("Figures", "missed_school.png"), 3.5 * 600, 3.5*600, res = 600)
school_plot |> print()
dev.off()

#### Clean up maps ####
# get prism vars for map
aux_data <- readRDS(here::here("Data", "auxiliary_data.rds"))
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
    missing_years <- missing_counties |>filter(county == i) |>pull(year)
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
county_centers_year <- st_centroid(california_heatmap_year)

county_plot_year_heatwave <- ggplot(
    california_heatmap_year |>
        filter(complete.cases(year)) |>
        mutate(`Climate Anxiety` = ClimateAnxiety)
) +
    geom_sf(
        data = census_heatwave |>
            mutate(year = as.numeric(year), `Heatwave Days` = days_above32),
        aes(fill = `Heatwave Days`),
        color = NA
    ) +
    geom_sf(fill = NA, color = "black") +
    geom_sf(
        data = county_centers_year |>
            filter(complete.cases(year)) |>
            mutate(`Climate Anxiety` = ClimateAnxiety),
        aes(size = `Climate Anxiety`),
        shape = 21,
        fill = "#2f2f2f",
        color = "white",
        stroke = 0.25,
        alpha = 0.85
    ) +
    scale_size(range = c(1.2, 7), breaks = c(0, .25, .5, .75, 1)) +
    scale_fill_distiller(palette = "Spectral") +
    theme_minimal() +
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
            mutate(`Climate Anxiety` = ClimateAnxiety)
    ) +
    geom_sf(
        data = census_temp |>
            mutate(year = as.numeric(year), `Avg. Tmax` = tmax),
        aes(fill = `Avg. Tmax`),
        color = NA
    ) +
    geom_sf(fill = NA, color = "black") +
    geom_sf(
        data = county_centers_year |>
            filter(complete.cases(year)) |>
            mutate(`Climate Anxiety` = ClimateAnxiety),
        aes(size = `Climate Anxiety`),
        shape = 21,
        fill = "#2f2f2f",
        color = "white",
        stroke = 0.25,
        alpha = 0.85
    ) +
    scale_size(range = c(1.2, 7), breaks = c(0, .25, .5, .75, 1)) +
    scale_fill_distiller(palette = "Spectral") +
    theme_minimal() +
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

png(here::here("Figures", "heatwave_map2.png"), width = 6 * 600, height = 6 * 600, res = 600)
grid::grid.draw(final_heatwave_plot)
dev.off()


png(here::here("Figures", "tmax_map2.png"), width = 6 * 600, height = 6 * 600, res = 600)
grid::grid.draw(final_tmax_plot)
dev.off()

