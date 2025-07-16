### Disparities research in barriers to mental health #############################################

### Import and read stuff in ######################################################################
# Load required libraries (ensure they are installed)
library(dplyr)       # Data manipulation
library(haven)       # Reading .dta files
library(survey)      # Survey data analysis
library(ggplot2)     # Data visualization
library(sf)          # Spatial data handling and mapping
library(prism)       # Prism data visualization, for temperatures
library(purrr)       # Functional programming
library(gtsummary)   # for summary tables
library(RColorBrewer)# map colors
library(lme4)        # Linear mixed-effects models
library(forcats)     # Factor manipulation
library(rstudioapi)  # For setting working directory to script location
library(glue)
library(exactextractr)
library(raster)
library(lubridate)
library(stringr)
library(terra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Sets to script location
setwd("..")  # Moves up to the project root
getwd()

#### load Functions ####
source("R/Functions.R")

#### load data ####
chis <- readRDS("Data/chis_puf_combined.Rds") 

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

# no missing values???
# mean(complete.cases( chis_design$variables[, c("tf45", "year", "srage_p", "srsex")]))

#### TABLE 1 - DEMOGRAPHICS #########################################################################

# Define baseline demographic variables to be included in the table
baseline_demographics <- c(
  # "srage_p",      # SELF-REPORTED AGE
  "cont_age",
  "srsex",        # SELF-REPORTED GENDER
  "ombsrtn_p1",   # OMB/CURRENT DOF RACE - ETHNICITY
  "schtyp_p1",    # TYPE OF SCHOOL ATTENDED
  "ahedtc_p1",    # ADULT EDUCATIONAL ATTAINMENT
  "povll",        # POVERTY LEVEL
  "lnghmt_p1",    # LANGUAGE SPOKEN AT HOME,
  "ur_clrt2"     # URBAN/RURAL CLASSIFICATION
)

attr(baseline_demographics,"label") <-list(
  tf45          = "Climate Anxiety", #CLIMATE CHANGE MAKES YOU NERVOUS/DEPRESSED/STRESSED
  "cont_age"     = "Age",     # SELF-REPORTED AGE
  "srsex"       = "Sex",        # SELF-REPORTED GENDER
  "ombsrtn_p1"  = "Ethnicity",   # OMB/CURRENT DOF RACE - ETHNICITY
  "schtyp_p1"   = "Type of School Attended",    # TYPE OF SCHOOL ATTENDED
  "ahedtc_p1"   = "Parents' Educational Attainment",    # ADULT EDUCATIONAL ATTAINMENT
  "povll"       = "Poverty Level",        # POVERTY LEVEL
  "lnghmt_p1"   = "Language Spoken at Home",     # LANGUAGE SPOKEN AT HOME
  "ur_clrt2"    = "Rural/Urban (Claritas ZIP, 2-level)"   # URBAN/RURAL CLASSIFICATION
)

attr(baseline_demographics,"type") <-list(
  tf45          = "categorical", #CLIMATE CHANGE MAKES YOU NERVOUS/DEPRESSED/STRESSED
  "cont_age"     = "continuous",     # SELF-REPORTED AGE
  "srsex"       = "categorical",        # SELF-REPORTED GENDER
  "ombsrtn_p1"  = "categorical",   # OMB/CURRENT DOF RACE - ETHNICITY
  "schtyp_p1"   = "categorical",    # TYPE OF SCHOOL ATTENDED
  "ahedtc_p1"   = "categorical",    # ADULT EDUCATIONAL ATTAINMENT
  "povll"       = "categorical",        # POVERTY LEVEL
  "lnghmt_p1"   = "categorical",     # LANGUAGE SPOKEN AT HOME
  "ur_clrt2"    = "categorical"   # URBAN/RURAL CLASSIFICATION
)

climateanx_tot <- svytotal(~tf45, design = chis_design, na.rm = TRUE)

# demo_table_list <- lapply(2021:2023, function(yy) {
#   tbl_custom_summary(
#   data = chis_design$variables %>% filter(year == yy),
#   by = "tf45",
#   stat_fns = everything() ~ mean_svy_rep,
#   label = attr(baseline_demographics, "label"),
#   statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~
#                      "{N} ({p}%) "),
#   digits = NULL,
#   missing = c("ifany"),
#   missing_text = "Unknown",
#   missing_stat = "{N_miss}",
#   include = baseline_demographics
# ) %>% 
#   bold_labels() %>% 
#   modify_header(
#     stat_1 ~ "**Yes**, <br>N = {format(round(climateanx_tot[1], 0),big.mark = ',')}",
#     stat_2 ~ "**No**, <br>N = {format(round(climateanx_tot[2], 0),big.mark = ',')}"
#   ) %>% 
#   modify_spanning_header(all_stat_cols() ~ "**Climate Anxiety**") 
# }
# )

demo_table <- tbl_custom_summary(
  data = chis_design$variables %>% 
    mutate(cont_age = as.numeric(as.character(srage_p))),
  by = "tf45",
  stat_fns = everything() ~ mean_svy_rep,
  label = attr(baseline_demographics, "label"),
  statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~
                     "{N} ({p}%) "),
  digits = NULL,
  type = attr(baseline_demographics,"type"),
  missing = c("ifany"),
  missing_text = "Unknown",
  missing_stat = "{N_miss}",
  include = baseline_demographics
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

# demo_table <- tbl_merge(
#   tbls = demo_table_list,
#   tab_spanner = c("**Climate Anxiety, 2021**", "**2022**", "**2023**")
# ) %>% 
#   modify_footnote_header(
#     footnote = "All totals and percentages are based on values from sample weights. There are a total of {format(nrow(chis),big.mark = ',')} observations in the raw data. Percentages across all values of one variable sum to approximately 100% due to rounding",
#     columns = all_stat_cols(),
#     replace = FALSE
#   ) 

# Display the table
# print(demo_table)

demo_table %>% 
  as_gt() %>% 
  gt::gtsave(filename = "Outputs/demographics.docx")

demo_table %>% 
  as_gt() %>% 
  gt::gtsave(filename = "Outputs/demographics.tex")


#### ANALYSIS #2- CLIMATE CHANGE/MENTAL HEALTH #####################################################################################
# Goal: To assess if climate anxiety is associated with worsened mental health symptoms (eg, nervousness, distress, depressed)
baseline_demographics <- c(
  "age_group",      # SELF-REPORTED AGE
  "srsex",        # SELF-REPORTED GENDER
  "ombsrtn_p1",   # OMB/CURRENT DOF RACE - ETHNICITY
  # "racecnt_p1",   # RACE - CENSUS 2000 DEFINITION
  "schtyp_p1",    # TYPE OF SCHOOL ATTENDED
  # "ta4c_p1",      # ATTENDED SCHOOL DURING LAST SCHOOL YR
  "ahedtc_p1",    # ADULT EDUCATIONAL ATTAINMENT
  # "as.numeric(as.character(povgwd_p1))",    # FAMILY POVERTY THRESHOLD LEVEL
  "povll",        # POVERTY LEVEL
  "lnghmt_p1"     # LANGUAGE SPOKEN AT HOME
);

baseline_demographics <- c(
  "age_group",      # SELF-REPORTED AGE
  "srsex",        # SELF-REPORTED GENDER
  "ombsrtn_p1",   # OMB/CURRENT DOF RACE - ETHNICITY
  "schtyp_p1",    # TYPE OF SCHOOL ATTENDED
  "ahedtc_p1",    # ADULT EDUCATIONAL ATTAINMENT
  "povll",        # POVERTY LEVEL
  "lnghmt_p1",    # LANGUAGE SPOKEN AT HOME,
  "ur_clrt2"     # URBAN/RURAL CLASSIFICATION
)


modifiable_protective <- c(
  "tl25",         # CARES DEEPLY ABOUT ISSUES IN COMMUNITY
  "tl27",         # BELIEVES CAN MAKE A DIFFERENCE IN THE COMMUNITY
  "tl50",         # EVER VOLUNTEERED TO SOLVE PROBLEM IN THE COMMUNITY
  "tl53",         # CONFIDENCE TO CONTACT SOMEONE IN THE GOVT WHO REPRESENTS COMMUNITY
  "tq10",         # HOW OFTEN FELT ABLE TO TALK TO FAMILY ABOUT FEELINGS
  "tq11",         # HOW OFTEN FELT FAMILY STOOD BY YOU DURING DIFFICULT TIMES
  "tq14",         # HOW OFTEN FELT SUPPORTED BY FRIENDS
  "tq16"          # HOW OFTEN ENJOYED PARTICIPATING IN COMMUNITY TRADITIONS
);

access_to_care <- c(
  "instype",      # INSURANCE TYPE
  "tf2",          # KIND OF PLACE MOST OFTEN GO FOR HEALTH CARE
  "tf9"          # DELAYED/DID NOT GET MEDICAL CARE FELT NEEDED IN PAST 12 MOS
);

civic_engagement <- c(
  "ta4",          # ATTENDED SCHOOL LAST WEEK
  # "ta4c_p1",         # ATTENDED SCHOOL DURING LAST SCHOOL YR, only in 2023 data
  "tb4",          # # OF DAYS OF SCHOOL MISSED FOR HEALTH PROBLEM PAST MO
  "tl10",         # PARTICIPATE IN CLUBS/ORGS OUTSIDE SCHOOL PAST YR
  "tq15"          # HOW OFTEN FELT SENSE OF BELONGING AT SCHOOL
);

# Aggregate all the variables
characteristics <- c(
  baseline_demographics, 
  modifiable_protective, 
  access_to_care, 
  civic_engagement
)

glm.test <- svyglm(as.formula(paste0("I(tf45 == 'Yes') ~ ", 
                          paste0(characteristics, collapse = " + "), 
                          " + as.factor(year) + I(scale(as.numeric(as.character(dstrstn_p1))))")),
                   , family = "binomial",
                   design = chis_design
)

glm.test %>% summary()


