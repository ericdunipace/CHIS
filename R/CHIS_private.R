### Disparities research in barriers to mental health #############################################

### Import and read stuff in ######################################################################
# Load required libraries (ensure they are installed)
library(stats)
library(utils)
library(haven)       # Reading .dta files
library(survey)      # Survey data analysis
library(ggplot2)     # data visualization
library(sf)          # Spatial data handling and mapping
library(purrr)       # Functional programming
library(gtsummary)   # for summary tables
library(RColorBrewer)# map colors
library(lme4)        # Linear mixed-effects models
library(forcats)     # Factor manipulation
library(glue)
library(here)
library(lemon)
library(forcats)     # Factor manipulation
library(dplyr)       # data manipulation

# confirm file location
here::i_am("R/CHIS_private.R")  # adjust this to your actual file location

# skip expensive calls on testthat
is_testthat <- Sys.getenv("TESTTHAT") == "true"

#### Setup output directory ####
if (!dir.exists(output_dir <- here::here("Outputs"))) {
  dir.create(output_dir)
}

#### load Functions ####
source(here::here("R","Functions.R"))

#### load data ####
# load chis data 2021-2023
d_chis_2023 <- haven::read_dta(here::here('data', 'dummyfile_2023_teen_stata', 'TEEN_with_format.dta')) %>%
  rename_all(tolower) %>%
  mutate(year = 2023) %>% 
  haven::as_factor()

d_chis_2022 <- haven::read_dta(here::here('data', 'teen 2022 dummy STATA', 'TEEN_with_format.dta')) %>%
  rename_all(tolower) %>%
  mutate(year = 2022) %>% 
  haven::as_factor()

d_chis_2021 <- haven::read_dta(here::here('data', 'dummyfile_2021_teen_stata', 'TEEN_with_format.dta')) %>%
  rename_all(tolower) %>%
  mutate(year = 2021) %>% 
  haven::as_factor()

chis_list <- list(
  d_chis_2021,
  d_chis_2022,
  d_chis_2023
)

# supporting data files
# can uncomment on personal machine
# county_shapefile <- readRDS(here::here("data","ca_county.rds"))
# census_shapefile <- readRDS(here::here("data","ca_tract_2010.rds"))
aux_data <- readRDS(here::here("data","auxiliary_data.rds"))

#### Clean and combine CHIS data ####
# chis <- readRDS(here::here("data', 'chis_combined.Rds") )

chis <- chis_clean(chis_list) %>% 
  mutate(county = fips_cnt) %>% 
  left_join(y = aux_data$prism$census %>% select(-year),
            by = c("tract10", "survey_years","survey_months")) %>% 
  left_join(y = aux_data$prism$county %>% select(-year),
            by = c("county", "survey_years","survey_months"))


for(j in colnames(d_chis_2023)) {
  attr(chis[[j]], "label") <- attr(d_chis_2023[[j]], "label")
}

# get prism vars for map
census_shapefile <- aux_data$census_ca
county_shapefile <- aux_data$county_ca

census_temp <- aux_data$map_census_temp %>% 
  select(year, tmax, tract10) %>% 
  left_join(y = census_shapefile, by = "tract10") %>% 
  sf::st_as_sf()
census_heatwave <- aux_data$map_census_heat %>% 
  select(year, days_above32, tract10) %>% 
  left_join(y = census_shapefile, by = "tract10") %>% 
  sf::st_as_sf()

# check for missing values in climate vars
missing_obs <- which(is.na(chis$tmax_tract10_prior_yr_mean))

if ( length(missing_obs) > 0 ) {
  cn <- colnames(aux_data$prism$census %>% select(-starts_with("survey"),-tract10,-year))
  cn_c <- gsub("tract10_", "county_", cn)
  
  for(j in seq_along(cn)) {
    if (is.null(chis[missing_obs, cn[j]])) next
    chis[missing_obs, cn[j]] <- chis[missing_obs, cn_c[j]]
  }
}

#### Set up survey design object to account for weights ####
# Set up survey design for analysis
chis_design <- survey::svrepdesign(
  data = chis,
  weights = ~ fnwgt0,
  repweights = "fnwgt[1-9]",
  type = "other",
  scale = 1,
  rscales = 1,
  mse = TRUE
)

#### Define analysis variables ####

baseline_demographics <- c(
  "age_group",      # SELF-REPORTED AGE
  "srsex",        # SELF-REPORTED GENDER
  "ombsrtn_p1",   # OMB/CURRENT DOF RACE - ETHNICITY
  "schtyp_p1",    # TYPE OF SCHOOL ATTENDED
  "ahedtc_binary",    # ADULT EDUCATIONAL ATTAINMENT
  "povll_binary",        # POVERTY LEVEL
  "lnghmt_binary",    # LANGUAGE SPOKEN AT HOME,
  "ur_clrt2"     # URBAN/RURAL CLASSIFICATION
)


modifiable_protective <- c(
  "scale(as.numeric(tl25))",         # CARES DEEPLY ABOUT ISSUES IN COMMUNITY strong agree -> strong dis
  "scale(as.numeric(tl27))",         # BELIEVES CAN MAKE A DIFFERENCE IN THE COMMUNITY strong agree -> strong dis
  "tl50",         # EVER VOLUNTEERED TO SOLVE PROBLEM IN THE COMMUNITY Yes, No
  "scale(as.numeric(tl53))",         # CONFIDENCE TO CONTACT SOMEONE IN THE GOVT WHO REPRESENTS COMMUNITY Definitely Could -> Definitely Could Not
  "scale(as.numeric(tq10))",         # HOW OFTEN FELT ABLE TO TALK TO FAMILY ABOUT FEELINGS All Of The Time -> Never
  "scale(as.numeric(tq11))",         # HOW OFTEN FELT FAMILY STOOD BY YOU DURING DIFFICULT TIMES All Of The Time -> Never
  "scale(as.numeric(tq14))",         # HOW OFTEN FELT SUPPORTED BY FRIENDS  All Of The Time -> Never
  "scale(as.numeric(tq16))"          # HOW OFTEN ENJOYED PARTICIPATING IN COMMUNITY TRADITIONS  All Of The Time -> Never
);

access_to_care <- c(
  "uninsured",      # INSURANCE TYPE, Yes, No
  "health_office",      # KIND OF PLACE MOST OFTEN GO FOR HEALTH CARE
  "tf9"          # DELAYED/DID NOT GET MEDICAL CARE FELT NEEDED IN PAST 12 MOS, Yes, No
);

civic_engagement <- c(
  "school_last_week",          # ATTENDED SCHOOL LAST WEEK Yes, No
  # "ta4c_p1",         # ATTENDED SCHOOL DURING LAST SCHOOL YR, only in 2023 data
  "scale(I(as.numeric(school_last_week ==  'Yes')) * tb4)",          # Number OF DAYS OF SCHOOL MISSED FOR HEALTH PROBLEM PAST MO -1 -> 15 in raw data, -1 is 
  "tl10",         # PARTICIPATE IN CLUBS/ORGS OUTSIDE SCHOOL PAST YR, Yes, No
  "scale(as.numeric(tq15))"   # HOW OFTEN FELT SENSE OF BELONGING AT SCHOOL All Of The Time -> Never
);

climate_variables <- c(
  "scale(tmax_tract10_prior_90_days_count32_delta)", # Tmax above 32 for the 90 days prior to survey date
  "scale(tmax_tract10_prior_90_days_mean_delta)", # Mean Tmaxfor the 90 days prior to survey date
  "scale(tmax_tract10_prior_yr_mean_delta)",          # Mean Tmax for the year prior to survey date
  "scale(tmax_tract10_prior_yr_count32_delta)",  # Count of Tmax above 32 for the year prior to survey date
  "scale(tmax_county_prior_90_days_count32_delta)", # Tmax above 32 for the 90 days prior to survey date
  "scale(tmax_county_prior_90_days_mean_delta)", # Mean Tmaxfor the 90 days prior to survey date
  "scale(tmax_county_prior_yr_mean_delta)",          # Mean Tmax for the year prior to survey date
  "scale(tmax_county_prior_yr_count32_delta)"  # Count of Tmax above 32 for the year prior to survey date
)

# Aggregate all the variables
characteristics <- c(
  baseline_demographics, 
  modifiable_protective, 
  access_to_care, 
  civic_engagement,
  climate_variables
)

# formula
formula <- paste0(
  "I(tf45 == 'Yes') ~ ",
  paste0(characteristics, collapse = " + ")
)

glmer.formula <- paste0(formula, "+ (1 | county)")


# variables for table 1
table_demographics <- baseline_demographics
table_demographics[grep("age_group",baseline_demographics)] <- "cont_age"

attr(table_demographics,"label") <-list(
  "tf45"          = "Climate Anxiety", #CLIMATE CHANGE MAKES YOU NERVOUS/DEPRESSED/STRESSED
  "cont_age"     = "Age",     # SELF-REPORTED AGE
  "srsex"       = "Sex",        # SELF-REPORTED GENDER
  "ombsrtn_p1"  = "Ethnicity",   # OMB/CURRENT DOF RACE - ETHNICITY
  "schtyp_p1"   = "Type of School Attended",    # TYPE OF SCHOOL ATTENDED
  "ahedtc_p1"   = "Parents' Educational Attainment",    # ADULT EDUCATIONAL ATTAINMENT
  "povll"       = "Poverty Level",        # POVERTY LEVEL
  "lnghmt_p1"   = "Language Spoken at Home",     # LANGUAGE SPOKEN AT HOME
  "ur_clrt2"    = "Rural/Urban (Claritas ZIP, 2-level)"   # URBAN/RURAL CLASSIFICATION
)

attr(table_demographics,"type") <-list(
  "tf45"          = "categorical", #CLIMATE CHANGE MAKES YOU NERVOUS/DEPRESSED/STRESSED
  "cont_age"     = "continuous",     # SELF-REPORTED AGE
  "srsex"       = "categorical",        # SELF-REPORTED GENDER
  "ombsrtn_p1"  = "categorical",   # OMB/CURRENT DOF RACE - ETHNICITY
  "schtyp_p1"   = "categorical",    # TYPE OF SCHOOL ATTENDED
  "ahedtc_p1"   = "categorical",    # ADULT EDUCATIONAL ATTAINMENT
  "povll"       = "categorical",        # POVERTY LEVEL
  "lnghmt_p1"   = "categorical",     # LANGUAGE SPOKEN AT HOME
  "ur_clrt2"    = "categorical"   # URBAN/RURAL CLASSIFICATION
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
  statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~
                     "{N} ({p}%) "),
  digits = NULL,
  type = attr(table_demographics,"type"),
  missing = c("ifany"),
  missing_text = "Unknown",
  # missing_stat = "{N_miss}",
  include = table_demographics
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
# print(demo_table)
demo_table %>% 
  as_gt() %>% 
  gt::gtsave(filename = here::here("Outputs","demographics.tex"))

#### ANALYSIS #1- SPATIAL HEATMAP #####################################################################################
# Goal: To create a spatial heatmap of climate anxiety across California counties
# Aggregate and average anxiety scores by county
county_result_year <- chis %>% 
  group_by(year, county) %>% 
  summarize(ClimateAnxiety = weighted.mean(tf45 == "Yes",fnwgt0,na.rm = TRUE))

# join results to county shapefile
california_heatmap_year <- full_join(county_shapefile, county_result_year, by = "county")

# check for all missing counties
missing_counties <- california_heatmap_year %>% 
  group_by(county) %>%
  filter(length(year) < 3) %>% 
  dplyr::select(county,year)

for(i in unique(missing_counties$county)) {
  missing_years <- missing_counties %>% filter(county == i) %>% pull(year)
  for(j in 2021:2023) {
    if(!(j %in% missing_years)) {
      california_heatmap_year <- california_heatmap_year %>% 
        bind_rows(california_heatmap_year %>% filter(county == i) %>% 
                    mutate(year = j,
                           ClimateAnxiety = NA))
    }
  }
}

# get centroids for points
county_centers_year <- st_centroid(california_heatmap_year)

county_plot_year_heatwave <- ggplot(california_heatmap_year %>% filter(complete.cases(year)) %>% mutate(`Climate Anxiety` = ClimateAnxiety)) +
  geom_sf(data = census_heatwave %>% 
            mutate(year = as.numeric(year),
                   `Heatwave Days` = days_above32
            )
          , aes(fill = `Heatwave Days`), color = NA) +
  geom_sf(fill = NA, color = "black") +
  geom_sf(data = county_centers_year %>% 
            filter(complete.cases(year)) %>% 
            mutate(`Climate Anxiety` = ClimateAnxiety), 
          aes(size = `Climate Anxiety`), color = "gray30") +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.title = element_blank(),         # Remove axis titles
    axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
    axis.ticks = element_blank()          # Remove axis ticks
  ) + 
  facet_wrap(~year, ncol = 2) +
  theme(legend.box = "horizontal")

county_plot_year_tmax <- 
  ggplot(california_heatmap_year %>% 
           filter(complete.cases(year)) %>%
           mutate(`Climate Anxiety` = ClimateAnxiety)
  ) +
  geom_sf(data = census_temp %>% 
            mutate(year = as.numeric(year),
                   `Avg. Tmax` = tmax
            )
          , aes(fill = `Avg. Tmax`), color = NA) +
  geom_sf(fill = NA, color = "black") +
  geom_sf(data = county_centers_year %>% 
            filter(complete.cases(year)) %>% 
            mutate(`Climate Anxiety` = ClimateAnxiety), 
          aes(size = `Climate Anxiety`), color = "gray30") +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.title = element_blank(),         # Remove axis titles
    axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
    axis.ticks = element_blank()          # Remove axis ticks
  ) + 
  facet_wrap(~year, ncol = 2) +
  theme(legend.box = "horizontal")

final_tmax_plot <- lemon::reposition_legend(
  county_plot_year_tmax,
  position = "center",
  panel = "panel-2-2",
  plot = FALSE)

final_heatwave_plot <- lemon::reposition_legend(
  county_plot_year_heatwave,
  position = "center",
  panel = "panel-2-2",
  plot = FALSE)


pdf(here::here("Outputs","heatwave_map.pdf"), width = 6, height = 6)
grid::grid.draw(final_heatwave_plot)
dev.off()

pdf(here::here("Outputs","tmax_map.pdf"), width = 6, height = 6)
grid::grid.draw(final_tmax_plot)
dev.off()

utils::write.csv(county_result_year, file = here::here("Outputs","climiate_anxiety_map_data.csv"),
          row.names = FALSE)

#### ANALYSIS #2- CLIMATE CHANGE/MENTAL HEALTH #####################################################################################

#glm model
glm.model <- survey::svyglm(as.formula(formula)
                    , family = quasibinomial(),
                    design = chis_design
)

# get summary tables
glm.summ <- glm.model %>% summary() 
print(glm.summ)
glm.vcov <- vcov(glm.model)

# save basic summary outputs
utils::write.csv(glm.summ$coefficients,
          file = here::here("Outputs","glm_model_summary.csv"))
utils::write.csv(glm.vcov,
          file = here::here("Outputs","glm_vcov.csv"))

# mixed effects model
ctrl <- lme4::glmerControl(
  optimizer = "bobyqa",
  optCtrl   = list( maxfun=1E5L ) 
  )

if ( isTRUE(is_testthat) ) {
  message("Running on GitHub Actions. Limiting max function evaluations for glmer")
  ctrl$optCtrl$maxfun <- 0L     # limit total function evaluations
  glmer.formula <- "I(tf45 == 'Yes') ~ 1 + (1 | county)"
}
mixef.model <- glmer.svyrep.design(glmer.formula,
                            , family   = "binomial"
                            , design   = chis_design
                            , control  = ctrl
                            , get.coef = TRUE
                            , verbose  = TRUE
)

# get summary of mixef
mixef.summ <- mixef.model %>% summary()
print(mixef.summ)

# save summaries
utils::write.csv(mixef.summ$coefficients,
          file = here::here("Outputs","fixef_coef.csv"))

# save standard errors of model
mixef.vcov <- mixef.model$vcov$combined
utils::write.csv(mixef.vcov,
          file = here::here("Outputs","mixef_vcov.csv"))


# save replicate coefficients for later calculations
beta_me <- as.data.frame(mixef.model$param$param)
rep_beta_me <- mixef.model$param$rep_param %>%
  dplyr::bind_cols() %>% 
  as.data.frame()
rownames(rep_beta_me) <- rownames(beta_me)

utils::write.csv(beta_me,
          file = here::here("Outputs","mixef_coef.csv"))
utils::write.csv(rep_beta_me,
          file = here::here("Outputs","mixef_replicate_coef.csv"))



