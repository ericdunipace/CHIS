### Disparities research in barriers to mental health #############################################

### Import and read stuff in ######################################################################
# Load required libraries (ensure they are installed)
library(haven)       # Reading .dta files
library(survey)      # Survey data analysis
library(ggplot2)     # Data visualization
library(sf)          # Spatial data handling and mapping
library(purrr)       # Functional programming
library(gtsummary)   # for summary tables
library(RColorBrewer)# map colors
library(lme4)        # Linear mixed-effects models
library(forcats)     # Factor manipulation
library(glue)
library(here)
library(lemon)
library(dplyr)       # Data manipulation

here::i_am("R/CHIS_private.R")  # adjust this to your actual file location

#### Setup output directory ####
if (!dir.exists(output_dir <- here::here("Outputs"))) {
  dir.create(output_dir)
}

#### load Functions ####
source(here::here("R","Functions.R"))

#### load data ####
# load chis data 2021-2023
d_chis_2023 <- haven::read_dta(here::here('Data', 'dummyfile_2023_teen_stata', 'TEEN_with_format.dta')) %>%
  rename_all(tolower) %>%
  mutate(year = 2023) %>% 
  haven::as_factor()

d_chis_2022 <- haven::read_dta(here::here('Data', 'teen 2022 dummy STATA', 'TEEN_with_format.dta')) %>%
  rename_all(tolower) %>%
  mutate(year = 2022) %>% 
  haven::as_factor()

d_chis_2021 <- haven::read_dta(here::here('Data', 'dummyfile_2021_teen_stata', 'TEEN_with_format.dta')) %>%
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
# county_shapefile <- readRDS(here::here("Data","ca_county.rds"))
# census_shapefile <- readRDS(here::here("Data","ca_tract_2010.rds"))
aux_data <- readRDS(here::here("Data","auxillary_data.rds"))

#### Clean and combine CHIS data ####
# chis <- readRDS(here::here("Data', 'chis_combined.Rds") )

chis <- pooling(chis_list) %>%
  mutate(across(where(is.factor), fct_drop)) %>% 
  mutate(
    age_group = factor(ifelse(srage_p %in% c("12","13","14"),
                       "12-14", "15-17")),
    ombsrtn_p1 = forcats::fct_recode(ombsrtn_p1,
                                     "Hispanic" = "Hispanic",
                                     "White, Non-Hispanic" = "White, Non-hispanic (nh)",
                                     "Asian, Non-Hispanic" = "Asian Only, Nh",
                                     "Two Or More Races, Non-Hispanic" = "Two Or More Races, Nh"),
    povll_binary = forcats::fct_recode(povll,
                                "Less than 300% FPL" = "0-99% Fpl",
                                "Less than 300% FPL" = "100-199% Fpl",
                                "Less than 300% FPL" = "200-299% Fpl",
                                "300% FPL And Above" = "300% Fpl And Above"),
    povll = forcats::fct_recode(povll,
                                "0-99% FPL" = "0-99% Fpl",
                                "100-199% FPL" = "100-199% Fpl",
                                "200-299% FPL" = "200-299% Fpl",
                                "300% FPL And Above" = "300% Fpl And Above"),
    ahedtc_binary = forcats::fct_recode(ahedtc_p1,
                                        "No college" = "No Formal Education Or Grade 1-8",
                                        "No college" = "Grade 9-11",
                                        "No college" = "Grade 12/h.s. Diploma",
                                        "College or more" = "Some College",
                                        "College or more" = "Aa/as Degree Or Vocational School",
                                        "College or more" = "Ba Or Bs Degree/some Grad School",
                                        "College or more" = "Ma Or Ms Degree",
                                        "College or more" = "Ph.d. Or Equivalent"
    ),
    ahedtc_p1 = forcats::fct_recode(ahedtc_p1,
                                    "Grade 12/H.S. Diploma" = "Grade 12/h.s. Diploma",
                                    "AA/AS Degree Or Vocational School" = "Aa/as Degree Or Vocational School",
                                    "BA Or BS Degree/Some Grad School" = "Ba Or Bs Degree/some Grad School",
                                    "MA Or MS Degree" = "Ma Or Ms Degree",
                                    "Ph.D. Or Equivalent" = "Ph.d. Or Equivalent"
    ),
    lnghmt_binary = forcats::fct_recode(lnghmt_p1,
                                       "English" = "English & Spanish",
                                       "English" = "English And One Other Language",
                                "Non-English" = "Spanish Or Other One Language Only",
                                "Non-English" = "Other Languages (2+)"
    ),
    uninsured = factor(ifelse(as.character(instype) == "Uninsured",
                                    "Yes", "No")),
    health_office = forcats::fct_recode(tf2,
                                "Primary care office" = "Doctor's Office/kaiser/hmo",
                                "Primary care office" = "Clinic/health Center/hospital Clinic",
                                "Primary care office" = "Other Health Professional (not Md)/alternative Medicine", 
                                "Not primary care office" = "Inapplicable"   ,
                                "Not primary care office" = "Emergency Room" ,
                                "Not primary care office" = "Family Members/friends Residence/who Are Health Professionals",
                                "Not primary care office" = "Some Other Place",
                                "Not primary care office" = "No One Place"),
    school_last_week = forcats::fct_recode(ta4,
                                "No" = "On Vacation",
                                "Yes" = "Home Schooled"
    ),
  ) %>% 
  mutate(survey_dates = parse_my_to_date(tadate_mm),
         survey_years  = lubridate::year(survey_dates),
         survey_months = lubridate::month(survey_dates)) %>% 
  mutate(county = fips_cnt) %>% 
  left_join(y = aux_data$prism %>% select(-year),
            by = c("tract10", "survey_years","survey_months"))



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


#### Set up survey design object to account for weights ####
# Set up survey design for analysis
chis_design <- svrepdesign(
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
  "as.numeric(tl25)",         # CARES DEEPLY ABOUT ISSUES IN COMMUNITY
  "as.numeric(tl27)",         # BELIEVES CAN MAKE A DIFFERENCE IN THE COMMUNITY
  "tl50",         # EVER VOLUNTEERED TO SOLVE PROBLEM IN THE COMMUNITY
  "as.numeric(tl53)",         # CONFIDENCE TO CONTACT SOMEONE IN THE GOVT WHO REPRESENTS COMMUNITY
  "as.numeric(tq10)",         # HOW OFTEN FELT ABLE TO TALK TO FAMILY ABOUT FEELINGS
  "as.numeric(tq11)",         # HOW OFTEN FELT FAMILY STOOD BY YOU DURING DIFFICULT TIMES
  "as.numeric(tq14)",         # HOW OFTEN FELT SUPPORTED BY FRIENDS
  "as.numeric(tq16)"          # HOW OFTEN ENJOYED PARTICIPATING IN COMMUNITY TRADITIONS
);

access_to_care <- c(
  "uninsured",      # INSURANCE TYPE
  "health_office",      # KIND OF PLACE MOST OFTEN GO FOR HEALTH CARE
  "tf9"          # DELAYED/DID NOT GET MEDICAL CARE FELT NEEDED IN PAST 12 MOS
);

civic_engagement <- c(
  "school_last_week",          # ATTENDED SCHOOL LAST WEEK
  # "ta4c_p1",         # ATTENDED SCHOOL DURING LAST SCHOOL YR, only in 2023 data
  "tb4",          # # OF DAYS OF SCHOOL MISSED FOR HEALTH PROBLEM PAST MO
  "tl10",         # PARTICIPATE IN CLUBS/ORGS OUTSIDE SCHOOL PAST YR
  "as.numeric(tq15)"          # HOW OFTEN FELT SENSE OF BELONGING AT SCHOOL
);

# Aggregate all the variables
characteristics <- c(
  baseline_demographics, 
  modifiable_protective, 
  access_to_care, 
  civic_engagement
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
  missing_stat = "{N_miss}",
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

final_heatwave_plot 
pdf(here::here("Outputs","heatwave_map.pdf"), width = 6, height = 6)
grid.draw(final_heatwave_plot)
dev.off()

pdf(here::here("Outputs","tmax_map.pdf"), width = 6, height = 6)
grid.draw(final_tmax_plot)
dev.off()

write.csv(county_result_year, file = here::here("Outputs","climiate_anxiety_map_data.csv"),
          row.names = FALSE)

colnames(census_temp)
colnames(california_heatmap_year)
colnames(census_heatwave)

census_temp <- census_temp %>%
    mutate(county_fips = substr(tract10, 1, 5))

census_heatwave <- census_heatwave %>%
    mutate(county_fips = substr(tract10, 1, 5))


temp_by_county <- census_temp %>%
    group_by(year, county_fips) %>%
    summarize(
        `Avg Tmax` = weighted.mean(tmax, POP10, na.rm = TRUE)
    )

heatwave_by_county <- census_heatwave %>%
    group_by(year, county_fips) %>%
    summarize(
        `Heatwave Days` = weighted.mean(days_above32, POP10, na.rm = TRUE)
    )

climate_panel <- california_heatmap_year %>%
    mutate(year = as.integer(year),
           county_fips = GEOID) %>%
    left_join(
        temp_by_county %>%
            mutate(year = as.integer(year)) %>%
            st_drop_geometry(),
        by = c("county_fips", "year")
    ) %>%
    left_join(
        heatwave_by_county %>%
            mutate(year = as.integer(year)) %>%
            st_drop_geometry(),
        by = c("county_fips", "year")
    )


model_full <- lm(ClimateAnxiety ~ `Avg Tmax` + `Heatwave Days` + factor(year), data = climate_panel)
summary(model_full)
model_summary <- summary(model_full)

coefs_df <- as.data.frame(model_summary$coefficients)

# Optionally clean column names
colnames(coefs_df) <- c("Estimate", "Std_Error", "t_value", "p_value")

# Add rownames as a new column for variable names
coefs_df <- tibble::rownames_to_column(coefs_df, var = "Variable")

write.csv(coefs_df, file = here::here("Outputs", "climate_anxiety_regression_results.csv"), row.names = FALSE)


library(ggplot2)

ggplot(climate_panel, aes(x = `Avg Tmax`, y = ClimateAnxiety)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    facet_wrap(~year) +
    theme_minimal() +
    labs(title = "Climate Anxiety vs Avg Tmax", x = "Average Tmax", y = "% Climate Anxiety")


#### ANALYSIS #2- CLIMATE CHANGE/MENTAL HEALTH #####################################################################################

glm.model <- svyglm(as.formula(formula)
                    , family = "binomial",
                    design = chis_design
)

mixef.model <- glmer.svyrep.design(glmer.formula,
                            family = "binomial",
                            design = chis_design,
                            control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=100000)),
                            get.coef = TRUE
)


# get summary tables
glm.summ <- glm.model %>% summary() 
mixef.summ <- mixef.model %>% summary()

print(glm.summ)
print(mixef.summ)

# save basic summary outputs
write.csv(glm.summ$coefficients,
          file = here::here("Outputs","glm_model_summary.csv"))
write.csv(mixef.summ$coefficients,
          file = here::here("Outputs","fixef_coef.csv"))

# save coef and standard errors of glm model
mixef.vcov <- mixef.model$vcov$combined
glm.vcov <- vcov(glm.model)

write.csv(mixef.vcov,
          file = here::here("Outputs","mixef_vcov.csv"))

write.csv(glm.vcov,
          file = here::here("Outputs","glm_vcov.csv"))

# save replicate coefficients for later calculations
beta_me <- as.data.frame(mixef.model$param$param)
rep_beta_me <- mixef.model$param$rep_param %>%
  dplyr::bind_cols() %>% 
  as.data.frame()
rownames(rep_beta_me) <- rownames(beta_me)

write.csv(beta_me,
          file = here::here("Outputs","mixef_coef.csv"))
write.csv(rep_beta_me,
          file = here::here("Outputs","mixef_replicate_coef.csv"))



