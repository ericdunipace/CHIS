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
# if (dir.exists(here::here("Outputs"))) {
#     unlink(here::here("Outputs"), recursive = TRUE, force = TRUE)
# }
# dir.create(here::here("Outputs"))
# unzip(
#     here::here("Outputs_DAC_original", "DAC250735508_20260327_JR.zip"),
#     exdir = here::here("Outputs"),
#     junkpaths = TRUE
# )

#### load table 1 information ####
tabl_1_long <- utils::read.csv(here::here("Outputs", "table1_basic.csv"))
tab1_long <- utils::read.csv(here::here("Outputs", "table1.csv"))
tab1_display <- utils::read.csv(here::here("Outputs", "table1_display.csv"))

saveRDS(tab1_display, file = here::here("Outputs", "table1_display.rds"))

render_table1(
    tab1_display,
    doc_dir = doc_dir,
    prefix = "table_1",
    suffix = ""
)


#### raw number for table 1 in the appendix ####
tabl_1_long_raw <- utils::read.csv(here::here(
    "Outputs",
    "table1_raw_basic.csv"
))
tab1_long_raw <- utils::read.csv(here::here("Outputs", "table1_raw.csv"))
tab1_display_raw <- utils::read.csv(here::here(
    "Outputs",
    "table1_raw_display.csv"
))

saveRDS(
    tab1_display_raw,
    file = here::here("Outputs", "table1_display_raw.rds")
)

render_table1(
    tab1_display_raw,
    doc_dir = doc_dir,
    prefix = "raw_table_1",
    suffix = "_appendix"
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
    ~variable                                           , ~group             ,
    "tl25_pos"                                          , "Civic Engagement" , #"Cares Deeply About Community Issues"
    "tl27_pos"                                          , "Civic Engagement" , #"Believes Can Make a Difference"
    "tl50"                                              , "Civic Engagement" , #"Volunteered to Solve Community Problem"
    "tl53_pos"                                          , "Civic Engagement" , #"Can Contact Gov't to Solve Problem"
    "tq10_pos"                                          , "Civic Engagement" , #"Able to Talk to Family About Feelings"
    "tq11_pos"                                          , "Civic Engagement" , #"Felt Supported by Family"
    "tq14_pos"                                          , "Civic Engagement" , #"Felt Supported by Friends"
    "tq16_pos"                                          , "Civic Engagement" , #"Enjoyed Community Traditions"
    "uninsured"                                         , "Access to Care"   ,
    "health_office"                                     , "Access to Care"   ,
    "tf9"                                               , "Access to Care"   , #"Did Not Delay Medical Care"
    "school_last_week"                                  , "Overall Health"   ,
    # "scale(I(as.numeric(school_last_week ==  'Yes')) * tb4)",
    "I((as.numeric(school_last_week ==  'Yes')) * tb4)" , "Overall Health"   ,
    "tl10"                                              , "Civic Engagement" , #"Participated in Clubs in Last Year"
    "tq15_pos"                                          , "Civic Engagement" # "Often Felt A Sense of Belonging at School"
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
                "tl50No", #"Volunteered to Solve Community Problem"
                "tl53_posYes", #"Can Contact Gov't to Solve Problem"
                "tq10_posYes", #"Able to Talk to Family About Feelings"
                "tq11_posYes", #"Felt Supported by Family"
                "tq14_posYes", #"Felt Supported by Friends"
                "tq16_posYes",
                "tl10No", #"Participated in Clubs in Last Year"
                "school_last_weekYes",
                "I((as.numeric(school_last_week == \"Yes\")) * tb4)",
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
        label = term,
        label = forcats::fct_recode(
            label,
            "Intercept" = "(Intercept)",
            "Age Group: 15–17" = "age_group15-17",
            "Female" = "srsexFemale",
            "Race" = "ombsrreoWhite",
            "Race" = "ombsrreoAfrican American",
            "Race" = "ombsrreoAsian/Pacific Islander",
            # "Race" = "ombsrreoAsian",
            "Race" = "ombsrreoMultiracial and Other",
            "School Type" = "sch_typPublic School",
            "School Type" = "sch_typPrivate School",
            # "School Type" = "sch_typOther",
            "Parents Attended College" = "ahedtc_binaryCollege or more",
            "300% FPL And Above" = "povll_binary300% FPL And Above",
            "Speaks English at Home" = "lnghmt_binaryNon-English", # note we flip sign below
            "Lives in a Rural Zip Code" = "ur_clrt2Rural",
            "Civic Engagement" = "tl25_posYes", #"Cares Deeply About Community Issues"
            "Civic Engagement" = "tl27_posYes", #"Believes Can Make a Difference"
            "Civic Engagement" = "tl50No", #"Volunteered to Solve Community Problem" # note we flip the sign below
            "Civic Engagement" = "tl53_posYes", #"Can Contact Gov't to Solve Problem"
            "Civic Engagement" = "tq10_posYes", #"Able to Talk to Family About Feelings"
            "Civic Engagement" = "tq11_posYes", #"Felt Supported by Family"
            "Civic Engagement" = "tq14_posYes", #"Felt Supported by Friends"
            "Civic Engagement" = "tq16_posYes",
            "Access to Care" = "uninsuredYes",
            "Access to Care" = "health_officePrimary care office",
            "Access to Care" = "tf9No", #"Did Not Delay Medical Care"
            "Overall Health" = "school_last_weekYes",
            # "Civic Engagement" = "scale(I(as.numeric(school_last_week == \"Yes\")) * tb4)",
            "Overall Health" = "I((as.numeric(school_last_week == \"Yes\")) * tb4)",

            "Civic Engagement" = "tl10No", #"Participated in Clubs in Last Year"
            "Civic Engagement" = "tq15_posYes", # "Often Felt A Sense of Belonging at School"
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
            "Asian/Pacific Islander" = "ombsrreoAsian/Pacific Islander",
            "Multiracial and other" = "ombsrreoMultiracial and Other",
            "White" = "ombsrreoWhite",
            "Public" = "sch_typPublic School",
            "Private" = "sch_typPrivate School",
            # "Other" = "sch_typOther",
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
    term = "Other (Ref.)",
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
        end = max(row),
        n = dplyr::n(),
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
    mutate(
        `  ` = if_else(
            n_group == 1,
            kableExtra::cell_spec(`  `, format = "latex", bold = TRUE),
            `  `
        )
    ) |>
    select(-n_group) |>
    subset(select = -1) %>%
    kableExtra::kbl(
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        align = c(rep("l", 2), rep("r", 2), "c")
    ) |>
    purrr::reduce(
        seq_len(nrow(group_index_glm)),
        .init = _,
        .f = function(tbl, i) {
            kableExtra::group_rows(
                tbl,
                group_label = group_index_glm$`::group_id::`[i],
                start_row = group_index_glm$start[i],
                end_row = group_index_glm$end[i]
            )
        }
    ) |>
    kableExtra::row_spec(0, align = "c") |>
    kableExtra::row_spec(seq(1, nrow(glm_tab_df), 2), background = "#f5f5f5") |>
    kableExtra::kable_styling(
        latex_options = c("hold_position", "scale_down"),
        font_size = 9
    ) |>
    cat(file = here::here(doc_dir, "glm_tab.tex"))


#### Generate Forest Plots ####
stopifnot("No BH p-values below cutoff" = any(glm_lor_table$bh_pvalue < 0.05))
# forest plot
forest_sig <- glm_lor_table |>
    filter(bh_pvalue < 0.05 & !is.na(bh_pvalue)) |>
    mutate(label = if_else(term != " ", term, label)) |>
    mutate(
        label = stringr::str_wrap(label, width = 20),
        label = factor(label, levels = rev(sort(unique(label))))
    ) |>
    mutate(label = forcats::fct_reorder(label, e_est)) |>
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
