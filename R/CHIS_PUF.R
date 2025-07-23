### Disparities research in barriers to mental health #############################################

### Import and read stuff in ######################################################################
# Load required libraries (ensure they are installed)
library(dplyr)       # Data manipulation
library(survey)      # Survey data analysis
library(gtsummary)   # for summary tables
library(stringr)
library(here)
library(broom)

here::i_am("R/CHIS_PUF.R")  # adjust this to your actual file location

#### Setup output directory ####
if (!dir.exists(output_dir <- here::here("Outputs"))) {
  dir.create(output_dir)
}

#### load Functions ####
source(here::here("R","Functions.R"))

#### load data ####
chis <- readRDS(here::here("Data","chis_puf_combined.Rds") )

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
demo_table %>% 
  as_gt() %>% 
  gt::gtsave(filename = here::here("Outputs","demographics.tex"))


#### analysis 1 needs the DAC data ####


#### ANALYSIS #2- CLIMATE CHANGE/MENTAL HEALTH #####################################################################################

#glm model
glm.model <- svyglm(as.formula(formula)
                    , family = quasibinomial(),
                    design = chis_design
)


tidy_model <- broom::tidy(glm.model, conf.int = TRUE, exponentiate = TRUE)
tidy_model$adj.p.value <- p.adjust(tidy_model$p.value, method = "fdr")

results_table <- tidy_model %>%
  select(term, estimate, conf.low, conf.high, adj.p.value) %>%
  mutate(adj.p.value = format.pval(adj.p.value, digits = 1, eps = 0.001)) %>% 
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  rename(
    `Variable` = term,
    `Odds Ratio` = estimate,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `P-value` = adj.p.value
  )

print(results_table)
write.csv(results_table, here::here("Outputs","vulnerable_subgroups_model_table.csv"), row.names = FALSE)




