### Disparities research in barriers to mental health #############################################

# Import and read stuff in 
# Load required libraries (ensure they are installed)
library(dplyr)       # Data manipulation
library(haven)       # Reading .dta files
library(survey)      # Survey data analysis
library(ggplot2)     # Data visualization
library(table1)      # Demographic tables
library(sf)          # Spatial data handling and mapping
library(purrr)


# Step 1: 
# Set Wd

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Sets to script location
setwd("..")  # Moves up to the project root
getwd()

# Import confidential dummy datasets
d_chis_2023 <- haven::read_dta('Data/dummyfile_2023_teen_stata/TEEN.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2023)

d_chis_2022 <- haven::read_dta('Data/teen 2022 dummy STATA/TEEN.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2022)

d_chis_2021 <- haven::read_dta('Data/dummyfile_2021_teen_stata/TEEN.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2021)

# Check for labels - no labels :(
labels <- sapply(d_chis_2023, function(x) attr(x, "label"))

# Import public use variable datasets
'/Users/danielzhao/Desktop/CHIS_Youth/Data/teen_2023_stata'
chis_2023 <- haven::read_dta('Data/teen_2023_stata/TEEN.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2023)

chis_2022 <- haven::read_dta('Data/teen_stata_2022/TEEN.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2022)

chis_2021 <- haven::read_dta('Data/teen_stata_2021/TEEN.dta') %>%
  rename_all(tolower) %>%
  mutate(year = 2021)

# Compare variable labels
str(d_chis_2023)  # confidential
str(chis_2023)    # PUF

# Add dummy data to list
my_d_chis_list <- list(d_chis_2021, d_chis_2022, d_chis_2023) # work with confidential data
my_chis_list <- list(chis_2021, chis_2022, chis_2023) # work with PUF data

# DO NOT CHANGE ANYTHING INSIDE THE FUNCTION UNLESS ABSOLUTELY NECESSARY

pooling <- function(chis_list) {
  
  for(i in 1:length(chis_list)) {
    
    chis_list[[i]][ , paste0("fnwgt", 0:80)] <- chis_list[[i]][ , paste0("rakedw", 0:80)]
    
    chis_list[[i]] <- 
      chis_list[[i]] %>% 
      rename_at(vars(paste0("fnwgt", c(1:80))), ~ paste0("fnwgt", c(1:80) + 80*(i-1))) 
    
  }
  
  chis_list <- chis_list %>%
    map(. %>% mutate(across(everything(), .fns = as.character)))
  
  merged <- 
    bind_rows(chis_list) %>% 
    data.frame(., row.names = NULL)
  
  merged <-
    merged  %>% 
    mutate_all(type.convert, as.is = TRUE)
  
  merged <-
    merged  %>% 
    mutate(across(starts_with("fnwgt"), ~ ifelse(is.na(.), fnwgt0, .)))
  
  merged <- 
    merged %>% 
    mutate(across(starts_with("fnwgt"), ~ ./length(chis_list)))
  
  merged
  
}

# Store pooled data, resulting data will either be in numeric or character format (no factors at all).

combined <- pooling(my_chis_list)
d_combined <- pooling(my_d_chis_list)

# Set up survey design for analysis

chis_design <- svrepdesign(data = combined,
                           weights =  ~ fnwgt0,
                           repweights = "fnwgt[1-9]",
                           type = "other",
                           scale = 1,
                           rscales = 1,
                           mse = TRUE)

d_chis_design <- svrepdesign(data = d_combined,
                             weights =  ~ fnwgt0,
                             repweights = "fnwgt[1-9]",
                             type = "other",
                             scale = 1,
                             rscales = 1,
                             mse = TRUE)


combined <- combined %>%
  mutate(climate_distress_bin = ifelse(tf45 == 1, 1, 0))  # 1 = Yes = Distressed

chis_design <- update(chis_design, climate_distress_bin = combined$climate_distress_bin)


d_combined <- d_combined %>%
  mutate(climate_distress_bin = ifelse(tf45 == 1, 1, 0))  # 1 = Yes = Distressed

d_chis_design <- update(d_chis_design, climate_distress_bin = d_combined$climate_distress_bin)



# Preview spread of basic variables
hist(chis_design$variables$srage_p)
hist(chis_design$variables$year)
hist(chis_design$variables$climate_distress_bin)
hist(chis_design$variables$climate_distress_bin)
# Recode tf45: Convert "2" to 1 and "1" to 0 (assumed binary outcome) (2 = NO)
# data$tf45 <- ifelse(data$tf45 == "2", 0, 1) # Confusing - but now 1 is YES

# Assign the cleaned dataset for further analysis
data <- chis_design$variables
dummy_data <- d_chis_design$variables

data$ta4c_p1
### REFACTOR CHIS_DESIGN

# Goal: to identify certain subgroups most vulnerable to climate change anxiety
baseline_demographics <- c(
  "srage_p",      # SELF-REPORTED AGE
  "srsex",        # SELF-REPORTED GENDER
  "ombsrtn_p1",   # OMB/CURRENT DOF RACE - ETHNICITY
  "schtyp_p1",    # TYPE OF SCHOOL ATTENDED
  "ahedtc_p1",    # ADULT EDUCATIONAL ATTAINMENT
  "povgwd_p1",    # FAMILY POVERTY THRESHOLD LEVEL
  "lnghmt_p1",    # LANGUAGE SPOKEN AT HOME
  "ur_clrt2"      # URBANICITY
);

# Age
data$srage_p <- factor(data$srage_p, levels = c("12", "13", "14", "15", "16", "17"))
# 
# data$srage_p <- factor(
#     data$srage_p,
#     levels = c("13", "14", "15", "16", "17"),
#     labels = c("Thirteen", "Fourteen", "Fifteen", "Sixteen", "Seventeen")
#     )

label(data$srage_p) <- "Age"
data$srage_p

# Sex
data$srsex <- factor(data$srsex, levels = c("1", "2"), labels = c("Male", "Female"))
label(data$srsex) <- "Sex"

# Race/Ethnicity
data$ombsrtn_p1 <- factor(
  data$ombsrtn_p1,
  levels = c("1", "2", "3", "5", "7"),
  labels = c("Hispanic", "White, NH", "Other Race", "Asian Only, NH", "Two or More Races, NH")
);


data$ombsrtn_p1 <- relevel(data$ombsrtn_p1, ref = "White, NH")

label(data$ombsrtn_p1) <- "Race/Ethnicity"

# Type of School Attended
data$schtyp_p1 <- factor(
  data$schtyp_p1,
  levels = c("-1", "1", "2"),
  labels = c("Inapplicable", "Public School", "Private School (Includes Homeschool)")
);

label(data$schtyp_p1) <- "Type of School Attended"

# Adult Educational Attainment
data$ahedtc_p1 <- factor(
  data$ahedtc_p1,
  levels = c("1", "2", "3", "4", "5", "7", "9", "10"),
  labels = c(
    "No formal education or grade 1–8",
    "Grade 9–11",
    "Grade 12 / High School Diploma",
    "Some College",
    "AA/AS Degree or Vocational School",
    "BA or BS Degree / Some Grad School",
    "MA or MS Degree",
    "Ph.D. or Equivalent"
  )
)

label(data$ahedtc_p1) <- "Parents' Educational Attainment"

data$ta4c_p1

# Poverty Level
data$povll <- factor(
  data$povll,
  levels = c("1", "2", "3", "4"),
  labels = c("0–99% FPL", "100–199% FPL", "200–299% FPL", "300% FPL and above")
)

label(data$povll) <- "Poverty Level"

# Language Spoken at Home
data$lnghmt_p1 <- factor(
  data$lnghmt_p1,
  levels = c("1", "2", "8", "9", "13"),
  labels = c(
    "English",
    "Spanish or other (one language only)",
    "English & Spanish",
    "English and one other language",
    "Other languages (2+)"
  )
)
label(data$lnghmt_p1) <- "Language Spoken at Home"

data$lnghmt_p1 <- relevel(data$lnghmt_p1, ref = "English")

# Rural/Urban Status
data$ur_clrt2 <- factor(
  data$ur_clrt2,
  levels = c("1", "2"),
  labels = c("Urban", "Rural")
)

data$ur_clrt2 <- relevel(data$ur_clrt2, ref = "Urban")

label(data$ur_clrt2) <- "Rural/Urban (Claritas ZIP, 2-level)"


# Year
data$year <- factor(data$year)
label(data$year) <- "Year"

data$climate_distress_bin

# Outcome variable
label(data$climate_distress_bin) <- "Total"

table(data$climate_distress_bin)

# Rename 0s and 1s into Yes and Nos
data$climate_distress_bin <- factor(
  data$climate_distress_bin,
  levels = c("0", "1"),
  labels = c("No", "Yes")
)

chis_design <- update(chis_design,
                      srage_p = data$srage_p,
                      srsex = data$srsex,
                      ombsrtn_p1 = data$ombsrtn_p1,
                      schtyp_p1 = data$schtyp_p1,
                      ahedtc_p1 = data$ahedtc_p1,
                      povll = data$povll,
                      lnghmt_p1 = data$lnghmt_p1,
                      ur_clrt2 = data$ur_clrt2,
                      year = data$year,
                      climate_distress_bin = data$climate_distress_bin)

#### TABLE 1 - DEMOGRAPHICS (PUF) #########################################################################

# Define baseline demographic variables to be included in the table
baseline_demographics <- c(
  "srage_p", "srsex", "ombsrtn_p1", "racecnt_p1",
  "schtyp_p1", "ta4c_p1", "ahedtc_p1", "povgwd_p1",
  "povll", "lnghmt_p1", "ur_clrt2", "year"
)

# Create demographic table grouped by year and tf45
demo_table <- table1(
  ~ srage_p + ombsrtn_p1 + srsex + schtyp_p1 + ahedtc_p1 + povll + lnghmt_p1 + ur_clrt2 | year * climate_distress_bin,
  data = data,
  overall = "Overall",
  drop.empty.groups = FALSE
)


# Check if data exists and has observations
dim(data)
str(data)

# Check if the grouping variables exist and have valid values
table(data$year, useNA = "always")
table(data$climate_distress_bin, useNA = "always")

# Check for missing combinations
table(data$year, data$climate_distress_bin, useNA = "always")
library(table1)
library(htmltools)

# Save nicely formatted HTML file
warning(" this table  doesn't account for weights")
save_html(print(demo_table), file = "demo_table.html")

# Check for hidden factor levels
str(data$srage_p)
levels(data$srage_p)  # if it's a factor


#### ANALYSIS #1- SPATIAL HEATMAP (DAC) #####################################################################################

library(dplyr)
library(ggplot2)
library(sf)
library(stringr)

# Load shapefile and filter for California
city_shapefile <- st_read("Data/tl_2024_us_county/tl_2024_us_county.shp")
california_shapefile <- city_shapefile %>% 
  filter(STATEFP == "06") %>%
  mutate(COUNTYFP_int = as.integer(COUNTYFP))

# Ensure fips_cnt is numeric and 3-digit
dummy_data$fips_cnt <- as.integer(dummy_data$fips_cnt)

# Add county name before summarizing
dummy_data_named <- dummy_data %>%
  left_join(
    california_shapefile %>%
      transmute(fips_cnt = as.integer(COUNTYFP), county_name = NAME),
    by = "fips_cnt"
  )

# Now summarize with name preserved
anxiety_by_county_year <- dummy_data_named %>%
  group_by(fips_cnt, county_name, year) %>%
  summarize(ClimateAnxiety = mean(tf45, na.rm = TRUE), .groups = "drop")

View(anxiety_by_county_year)


# What other geographic units could we have merged by? (we could've also
# considered zipcode, but unclear if there is enough sample size per zipcode)
# We could also do county with respect to California but zipcode with respect to LA
names(chis_data_filtered)
names(california_shapefile)

# Compute average climate anxiety by county and year
anxiety_by_county_year <- dummy_data %>%
  group_by(fips_cnt, year) %>%
  summarize(ClimateAnxiety = mean(tf45, na.rm = TRUE), .groups = "drop")

View(anxiety_by_county_year)
yr <- 2021

# Join shapefile with anxiety data for each year and plot
merged_data <- anxiety_by_county_year %>%
  filter(year == yr) %>%
  left_join(california_shapefile, ., by = c("COUNTYFP_int" = "fips_cnt")) %>%
  st_as_sf()

View(merged_data)

p <- ggplot(merged_data) +
  geom_sf(aes(fill = ClimateAnxiety)) +
  scale_fill_gradientn(colors = c("green", "yellow", "orange", "red"),
                       name = "Climate\nDistress") +
  labs(title = paste("Climate Change Distress by County -", yr)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(p)
ggsave("Climate_Heatmap_2021.pdf", plot = p, width = 11, height = 8.5)

yr <- 2022

# Join shapefile with anxiety data for each year and plot
merged_data <- anxiety_by_county_year %>%
  filter(year == yr) %>%
  left_join(california_shapefile, ., by = c("COUNTYFP_int" = "fips_cnt")) %>%
  st_as_sf()

p <- ggplot(merged_data) +
  geom_sf(aes(fill = ClimateAnxiety)) +
  scale_fill_gradientn(colors = c("green", "yellow", "orange", "red"),
                       name = "Climate\nDistress") +
  labs(title = paste("Climate Change Anxiety by County -", yr)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(p)
ggsave("Climate_Heatmap_2022.pdf", plot = p, width = 11, height = 8.5)

yr <- 2023

# Join shapefile with anxiety data for each year and plot
merged_data <- anxiety_by_county_year %>%
  filter(year == yr) %>%
  left_join(california_shapefile, ., by = c("COUNTYFP_int" = "fips_cnt")) %>%
  st_as_sf()

# View(merged_data)
p <- ggplot(merged_data) +
  geom_sf(aes(fill = ClimateAnxiety)) +
  scale_fill_gradientn(colors = c("green", "yellow", "orange", "red"),
                       name = "Climate\nDistress") +
  labs(title = paste("Climate Change Anxiety by County -", yr)) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

print(p)
ggsave("Climate_Heatmap_2023.pdf", plot = p, width = 11, height = 8.5)

#### ANALYSIS #2- CLIMATE CHANGE/MENTAL HEALTH (DAC) (DISCARDED) #####################################################################################
# Goal: To assess if climate anxiety is associated with worsened mental health symptoms (eg, nervousness, distress, depressed)

#### UNIVARIATE ANALYSES
# Define mental health variables of interest
mental_health_issues <- c(
  "te68_13",  # Reasons use e-cigs: Reduce stress, anxiety, or pain
  "tf11",     # Received psychological/emotional counseling in the past 12 months
  "ti11",     # Needed help for emotional problems in the past 12 months
  "tg11",     # Felt nervous in the past 30 days
  "tg12",     # Felt hopeless in the past 30 days
  "tg14",     # Felt depressed in the past 30 days
  "dstrs30",  # Likely has psychological distress in the past month
  "dstrsyr",  # Serious psychological distress for worst month in the past year (K6 score)
  "dstrs12",  # Likely has had psychological distress in the last year
  "distress", # Serious psychological distress
  "tf31",     # Felt nervous in the worst month
  "tf32",     # Felt hopeless in the worst month
  "tf34"      # Felt depressed in the worst month
);


# Initialize vectors to store results for all levels of all variables
characteristic_names <- c()  # To store the variable name for each level
level_names <- c()           # To store the level name for each coefficient
p.values <- c()              # To store p-values for all levels
odds.ratios <- c()           # To store odds ratios for all levels
ci.lower <- c()              # To store lower bounds of confidence intervals
ci.upper <- c()              # To store upper bounds of confidence intervals
significances <- c()         # To store significance stars for all levels

characteristics <- mental_health_issues

# Factor all characteristics except continuous variables like distress and dstrsyr
for (i in 1:length(characteristics)) {
  characteristic <- characteristics[i]
  
  data[[characteristic]] <- factor(data[[characteristic]])
}

data$dstrsyr <- as.numeric(data$dstrsyr)
data$distress <- as.numeric(data$distress)

# Loop over each variable
for (i in 1:length(characteristics)) {
  
  characteristic <- characteristics[i]
  
  # Create formula for the logistic regression
  fmla <- as.formula(paste("tf45 ~ ", characteristic))
  
  # Fit the logistic regression model
  univariate_model <- glm(
    fmla,
    data = data,
    family = "binomial",
    weights = data$RAKEDW0
  );
  
  # Extract coefficients, confidence intervals, and p-values for all levels
  ORs <- exp(coef(univariate_model))  # Odds ratios
  CIs <- exp(confint.default(univariate_model, level = 0.95))  # Confidence intervals
  p_values <- summary(univariate_model)$coefficients[, 4]  # p-values
  
  # Convert p-values to significance stars
  sig <- ifelse(p_values < 0.001, "***",
                ifelse(p_values < 0.01, "**",
                       ifelse(p_values < 0.05, "*", "-")))
  
  # Append results for each level of this variable
  characteristic_names <- c(characteristic_names, rep(characteristic, length(ORs)))
  level_names <- c(level_names, names(ORs))
  odds.ratios <- c(odds.ratios, ORs)
  ci.lower <- c(ci.lower, CIs[, 1])
  ci.upper <- c(ci.upper, CIs[, 2])
  p.values <- c(p.values, p_values)
  significances <- c(significances, sig)
}

# Created the final results dataframe
model.results <- data.frame(
  characteristic_names,
  level_names,
  odds.ratios,
  ci.lower,
  ci.upper,
  odds.ratios,
  significances
);

# View(model.results)

#### MULTIVARIATE ANALYSES

# Define the covariates to be used in the multivariate analyses
covariates <- c(
  "srage",       # Age
  "srsex",       # Gender
  "ombsrreo",    # Race/Ethnicity
  "tq9",         # Family financial stress
  "instype"      # Insurance type
);

characteristics <- mental_health_issues

# Initialize vectors to store results for all levels of all variables
characteristic_names <- c()  # To store the variable name for each level
level_names <- c()           # To store the level name for each coefficient
p.values <- c()              # To store p-values for all levels
odds.ratios <- c()           # To store odds ratios for all levels
ci.lower <- c()              # To store lower bounds of confidence intervals
ci.upper <- c()              # To store upper bounds of confidence intervals
significances <- c()         # To store significance stars for all levels

# Initialize vectors
multi.p.values <- rep(NA, length(characteristics));
multi.odds.ratios <- rep(NA, length(characteristics));
multi.confidence.intervals <- rep(NA, length(characteristics));
multi.significances <- rep(NA, length(characteristic));

# as.formula(paste("tf45 ~ ", paste(xnam, collapse="+")))

# Loop over each variable while adjusting for covariates
for (i in 1:length(characteristics)) {
  
  ## Univariable: create a formula for a model with a large number of variables
  characteristic <- characteristics[i] # so this is mental health characteristic
  xnam <- c(characteristic, covariates)
  fmla <- as.formula(paste("tf45 ~ ", paste(xnam, collapse= "+")))
  
  # for each univariate model, save the unadjusted OR and p value
  univariate_model <- glm(
    fmla,
    data = data,
    family = "binomial",
    weights = data$RAKEDW0
  );
  
  # Extract odds ratio, confidence intervals, and p-value
  OR <- exp(coef(univariate_model)[2])  # Odds ratio for the variable
  CI <- exp(confint.default(univariate_model, level = 0.95)[2, ])  # 95% CI for the variable
  p_value <- summary(univariate_model)$coefficients[2, 4]  # p-value for the variable
  # Convert p-values to factors
  sig <- ifelse(p_value < 0.001, "***", 
                ifelse(p_value < 0.01, "**", 
                       ifelse(p_value < 0.05, "*", "-")))
  
  multi.p.values[i] <- p_value
  multi.confidence.intervals[i] <- CI
  multi.odds.ratios[i] <- OR
  multi.significances[i] <- sig
}

# Create dataframe with bootstrap results
model.results <- data.frame(
  characteristics,
  multi.p.values,
  multi.confidence.intervals,
  multi.odds.ratios,
  multi.significances
);

labels <- labelled::var_label(chis_2023)  # This gives variable labels
labels
# Create a named character vector
class(labels)
label_dict <- unlist(labels)
class(label_dict)
label_dict

# Lookup the labels for the variable names
friendly_names <- label_dict[characteristics]
friendly_names

##### ANALYSIS #3 - VULNERABLE SUBGROUPS (PUF) ##########################################################

# Goal: to identify certain subgroups most vulnerable to climate change anxiety

baseline_demographics <- c(
  "srage_p",      # SELF-REPORTED AGE
  "srsex",        # SELF-REPORTED GENDER
  "ombsrtn_p1",   # OMB/CURRENT DOF RACE - ETHNICITY
  "schtyp_p1",    # TYPE OF SCHOOL ATTENDED
  "ahedtc_p1",    # ADULT EDUCATIONAL ATTAINMENT
  "povgwd_p1",    # FAMILY POVERTY THRESHOLD LEVEL
  "lnghmt_p1",    # LANGUAGE SPOKEN AT HOME
  "ur_clrt2"      # URBANICITY
);


modifiable_protective <- c(
  "tq10",         # HOW OFTEN FELT ABLE TO TALK TO FAMILY ABOUT FEELINGS
  "tq11",         # HOW OFTEN FELT FAMILY STOOD BY YOU DURING DIFFICULT TIMES
  "tq14"          # HOW OFTEN FELT SUPPORTED BY FRIENDS
);

access_to_care <- c(
  "tb19",         # ER/URGENT CARE VISIT FOR ASTHMA IN PAST YEAR
  "forgo",        # HAD TO FORGO NECESSARY CARE
  "tb24",         # OF SCHOOL DAYS MISSED DUE TO ASTHMA IN PAST 12 MOS
  "instype",      # INSURANCE TYPE
  "tf2",          # KIND OF PLACE MOST OFTEN GO FOR HEALTH CARE
  "tf9",          # DELAYED/DID NOT GET MEDICAL CARE FELT NEEDED IN PAST 12 MOS
  "th57"          # EVENTUALLY RECEIVED MED CARE THAT WAS DELAYED
);

civic_engagement <- c(
  "ta4c_p1",         # ATTENDED SCHOOL DURING LAST SCHOOL YR
  "tl53",         # CONFIDENCE TO CONTACT SOMEONE IN THE GOVT WHO REPRESENTS COMMUNITY
  "tl10",         # PARTICIPATE IN CLUBS/ORGS OUTSIDE SCHOOL PAST YR
  "tq15",         # HOW OFTEN FELT SENSE OF BELONGING AT SCHOOL
  "tl25",         # CARES DEEPLY ABOUT ISSUES IN COMMUNITY
  "tl27",         # BELIEVES CAN MAKE A DIFFERENCE IN THE COMMUNITY
  "tl50",         # EVER VOLUNTEERED TO SOLVE PROBLEM IN THE COMMUNITY
  "tq16"          # HOW OFTEN ENJOYED PARTICIPATING IN COMMUNITY TRADITIONS
);

table(chis_design$variables$ta4c_p1, chis_design$variables$year)

# Define the covariates to be used in the multivariate analyses
covariates <- c(
  "srage_p",     # Age
  "srsex",       # Gender
  "ombsrtn_p1",  # Race/Ethnicity
  "povll"        # Poverty
);


# data <- chis_data_filtered
# 
# # Aggregate all the variables
# characteristics <- c(
#     baseline_demographics, 
#     modifiable_protective, 
#     access_to_care, 
#     receiving_mental_healthcare, 
#     civic_engagement
#     )
# 
# # Check to see if all the predictor variables are in dataset
# characteristics %in% colnames(data)
# 
# # If using PUF data, just subset to the variables that are avaiable 
# characteristics <- characteristics[characteristics %in% colnames(data)]
# 
# # Initialize vectors to store results for all levels of all variables
# characteristic_names <- c()  # To store the variable name for each level
# level_names <- c()           # To store the level name for each coefficient
# p.values <- c()              # To store p-values for all levels
# odds.ratios <- c()           # To store odds ratios for all levels
# ci.lower <- c()              # To store lower bounds of confidence intervals
# ci.upper <- c()              # To store upper bounds of confidence intervals
# significances <- c()         # To store significance stars for all levels
# 
# 
# # Loop over each variable
# for (i in 1:length(characteristics)) {
#     
#     characteristic <- characteristics[i]
#     
#     # Create formula for the logistic regression
#     fmla <- as.formula(paste("climate_distress_bin ~ year +", paste(xnam, collapse = "+")))
#     
#     # Fit the logistic regression model
#     univariate_model <- glm(
#         fmla,
#         data = data,
#         family = "binomial",
#         weights = data$RAKEDW0
#         );
#     
#     # Extract coefficients, confidence intervals, and p-values for all levels
#     ORs <- exp(coef(univariate_model))  # Odds ratios
#     CIs <- exp(confint.default(univariate_model, level = 0.95))  # Confidence intervals
#     p_values <- summary(univariate_model)$coefficients[, 4]  # p-values
#     
#     # Convert p-values to significance stars
#     sig <- ifelse(p_values < 0.001, "***",
#                   ifelse(p_values < 0.01, "**",
#                          ifelse(p_values < 0.05, "*", "-")))
#     
#     # Append results for each level of this variable
#     characteristic_names <- c(characteristic_names, rep(characteristic, length(ORs)))
#     level_names <- c(level_names, names(ORs))
#     odds.ratios <- c(odds.ratios, ORs)
#     ci.lower <- c(ci.lower, CIs[, 1])
#     ci.upper <- c(ci.upper, CIs[, 2])
#     p.values <- c(p.values, p_values)
#     significances <- c(significances, sig)
#     }
# 
# 
# # Create dataframe with bootstrap results
# model.results <- data.frame(
#     characteristic_names,
#     level_names,
#     odds.ratios,
#     ci.lower,
#     ci.upper,
#     odds.ratios,
#     significances
#     );
# 
# View(model.results)

#### MULTIVARIATE

library(survey)

# Create the formula
all_predictors <- c(baseline_demographics, modifiable_protective, access_to_care, civic_engagement)
all_puf_predictors <- c(baseline_demographics, modifiable_protective, civic_engagement) # access_to_care vars are missing from PUF
base_formula <- as.formula(paste("climate_distress_bin ~", paste(all_puf_predictors, collapse = " + ")))

all_puf_predictors[!all_puf_predictors %in% colnames(chis_design$variables)]


table(chis_design$variables$ta4c_p1, chis_design$variables$year)

# Multivariate logistic model
multi_logistic_model <- svyglm(base_formula, 
                               design = chis_design,
                               family = quasibinomial())



tidy_model <- broom::tidy(multi_logistic_model, conf.int = TRUE, exponentiate = TRUE)

tidy_model$adj.p.value <- p.adjust(tidy_model$p.value, method = "fdr")

results_table <- tidy_model %>%
  select(term, estimate, conf.low, conf.high, p.value, adj.p.value) %>%
  mutate(across(where(is.numeric), ~ round(., 6))) %>%
  rename(
    Characteristic = term,
    `Odds Ratio` = estimate,
    `95% CI Lower` = conf.low,
    `95% CI Upper` = conf.high,
    `P-value` = p.value
  )

print(results_table)
# View(results_table)
write.csv(results_table, "vulnerable_subgroups_model_table.csv", row.names = FALSE)

# # Step 2: Initialize vectors
# characteristic_names <- c()
# level_names <- c()
# p.values <- c()
# odds.ratios <- c()
# ci.lower <- c()
# ci.upper <- c()
# significances <- c()

# # Step 4: Loop over each characteristic
# for (i in seq_along(characteristics)) {
# 
#     characteristic <- characteristics[i]
#     print(characteristic)
#     
#     # Build formula
#     xnam <- c(characteristic)
#     fmla <- as.formula(paste("climate_distress_bin ~ year +", paste(xnam, collapse = "+")))
#     
#     # Fit survey-weighted model
#     model <- svyglm(fmla, design = chis_design, family = quasibinomial())
#     
#     # Get coefficients
#     ORs <- exp(coef(model))
#     CIs <- exp(confint(model))  # design-aware CI
#     pvals <- summary(model)$coefficients[, 4]
#     
#     # Significance stars
#     sigs <- ifelse(pvals < 0.001, "***",
#                    ifelse(pvals < 0.01, "**",
#                           ifelse(pvals < 0.05, "*", "-")))
#     
#     # Identify coefficients for the current variable
#     coef_names <- names(ORs)
#     
#     if (is.factor(data[[characteristic]])) {
#         indices <- grep(paste0("^", characteristic), coef_names)
#     } else {
#         indices <- which(coef_names == characteristic)
#     }
#     
#     # Store results
#     if (length(indices) > 0) {
#         characteristic_names <- c(characteristic_names, rep(characteristic, length(indices)))
#         level_names <- c(level_names, coef_names[indices])
#         odds.ratios <- c(odds.ratios, ORs[indices])
#         ci.lower <- c(ci.lower, CIs[indices, 1])
#         ci.upper <- c(ci.upper, CIs[indices, 2])
#         p.values <- c(p.values, pvals[indices])
#         significances <- c(significances, sigs[indices])
#     }
# }
# 
# library(broom)     # For tidy model output
# library(dplyr)     # For data manipulation
















# 
# 
# 
# data <- chis_data_filtered
# 
# # Initialize vectors to store results for ALL levels of ALL variables
# characteristic_names <- c()  # To store the variable name for each level
# level_names <- c()           # To store the level name for each coefficient
# p.values <- c()              # To store p-values for all levels
# odds.ratios <- c()           # To store odds ratios for all levels
# ci.lower <- c()              # To store lower bounds of confidence intervals
# ci.upper <- c()              # To store upper bounds of confidence intervals
# significances <- c()         # To store significance stars for all levels
# 
# # Define covariates
# covariates <- c(
#   "srage_p",     # Age
#   "srsex",       # Gender
#   "ombsrtn_p1",  # Race/Ethnicity
#   "tq9",         # Family financial stress
#   "instype"      # Insurance type
# )
# 
# # Loop over each characteristic
# for (i in 1:length(characteristics)) {
#   
#   characteristic <- characteristics[i]
#   
#   # Create formula with characteristic + covariates
#   xnam <- c(characteristic, covariates)
#   fmla <- as.formula(paste("tf45 ~ ", paste(xnam, collapse= "+")))
#   
#   # Fit the model
#   multivariate_model <- glm(
#     fmla,
#     data = data,
#     family = "binomial",
#     weights = data$RAKEDW0
#   )
#   
#   # Get ALL coefficients, CIs, and p-values
#   all_ORs <- exp(coef(multivariate_model))
#   all_CIs <- exp(confint.default(multivariate_model, level = 0.95))
#   all_p_values <- summary(multivariate_model)$coefficients[, 4]
#   
#   # Convert p-values to significance stars
#   all_sig <- ifelse(all_p_values < 0.001, "***",
#                     ifelse(all_p_values < 0.01, "**",
#                            ifelse(all_p_values < 0.05, "*", "-")))
#   
#   # Find coefficients that belong to our characteristic of interest
#   coef_names <- names(all_ORs)
#   
#   # For categorical variables, find all levels that start with the variable name
#   # For continuous variables, find exact match
#   if (is.factor(data[[characteristic]])) {
#     # Categorical: find all coefficients starting with variable name
#     char_indices <- grep(paste0("^", characteristic), coef_names)
#   } else {
#     # Continuous: find exact match
#     char_indices <- which(coef_names == characteristic)
#   }
#   
#   # Extract results for this characteristic
#   if (length(char_indices) > 0) {
#     # Store results for each level of this characteristic
#     characteristic_names <- c(characteristic_names, rep(characteristic, length(char_indices)))
#     level_names <- c(level_names, coef_names[char_indices])
#     odds.ratios <- c(odds.ratios, all_ORs[char_indices])
#     ci.lower <- c(ci.lower, all_CIs[char_indices, 1])
#     ci.upper <- c(ci.upper, all_CIs[char_indices, 2])
#     p.values <- c(p.values, all_p_values[char_indices])
#     significances <- c(significances, all_sig[char_indices])
#   }
# }
# 
# # Create final results dataframe
# model.results <- data.frame(
#   Variable = characteristic_names,
#   Level = level_names,
#   OR = round(odds.ratios, 3),
#   CI_Lower = round(ci.lower, 3),
#   CI_Upper = round(ci.upper, 3),
#   P_Value = round(p.values, 4),
#   Significance = significances,
#   stringsAsFactors = FALSE
# )
# 
# # View results
# # View(model.results)
# print(head(model.results, 10))
# 
# # Optional: Create a nicely formatted table
# library(flextable)
# 
# # Create CI column as a string
# model.results$CI_95 <- paste0("(", model.results$CI_Lower, " – ", model.results$CI_Upper, ")")
# 
# # Create display table
# display_table <- model.results[, c("Variable", "Level", "OR", "CI_95", "P_Value", "Significance")]
# 
# # Convert to flextable for nice output
# ft <- flextable(display_table) %>%
#   theme_booktabs() %>%
#   autofit() %>%
#   set_caption("Multivariate Logistic Regression Results (Adjusted for Covariates)") %>%
#   align(j = 3:6, align = "center", part = "all")
# 
# # Print the table
# print(ft)
# 
# ##### ANALYSIS #4 - TEMPORAL ANALYSIS (PUF) ##########################################################
# # 4.1 How does climate change distress change over time? 
# 
# # Create an empty data frame to store results
# climate_distress_trend <- data.frame(
#   year = c(2021, 2022, 2023),
#   Total_Respondents = NA,
#   Climate_Distress_Count = NA,
#   Climate_Distress_Percentage = NA
# )
# 
# # Loop through years and calculate prevalence of climate distress
# for (i in 1:nrow(climate_distress_trend)) {
#   year_i <- climate_distress_trend$year[i]
#   
#   # Subset data for the year
#   data_year <- data[data$year == year_i, ]
#   
#   # Calculate total respondents
#   climate_distress_trend$Total_Respondents[i] <- nrow(data_year)
#   
#   # Calculate number of respondents with climate distress (tf45 == 1)
#   climate_distress_trend$Climate_Distress_Count[i] <- sum(data_year$tf45 == 1, na.rm = TRUE)
#   
#   # Calculate percentage of respondents with climate distress
#   climate_distress_trend$Climate_Distress_Percentage[i] <- 
#     (climate_distress_trend$Climate_Distress_Count[i] / climate_distress_trend$Total_Respondents[i]) * 100
# }
# 
# # Print the summary table
# print(climate_distress_trend)
# 
# # Visualize the trend over time
# ggplot(climate_distress_trend, aes(x = year, y = Climate_Distress_Percentage)) +
#   geom_line(size = 1, color = "blue") +
#   geom_point(size = 3, color = "red") +
#   labs(
#     title = "Trend of Climate Change Distress (2021-2023)",
#     x = "Year",
#     y = "Percentage Reporting Climate Distress"
#   ) +
#   theme_minimal()
# 
# 
# # 4.2 Test whether climate distress prevalence has significantly changed over time.
# # Fit logistic regression model predicting climate distress (tf45) using year
# logistic_model <- svyglm(climate_distress_bin ~ factor(year), 
#                          design = chis_design,
#                          family = quasibinomial())
# 
# 
# # Display model summary
# summary(logistic_model)
# 
# # Extract odds ratios and confidence intervals
# odds_ratios <- exp(coef(logistic_model))
# conf_int <- exp(confint(logistic_model))
# 
# # Print results
# results_table <- data.frame(
#   Variable = names(odds_ratios),
#   OR = odds_ratios,
#   CI_Lower = conf_int[, 1],
#   CI_Upper = conf_int[, 2]
# )
# 
# # ORs suggest 2023 is statistically significant departure from 2021 in terms of climate change anxiety
# print(results_table) 
# 
# # Create the formula
# base_formula <- as.formula(paste("climate_distress_bin ~ year +", paste(covariates, collapse = " + ")))
# 
# # Multivariate logistic model
# multi_logistic_model <- svyglm(base_formula, 
#                                design = chis_design,
#                                family = quasibinomial())
# 
# covariates
# # Display model summary
# summary(multi_logistic_model)
# 
# # Extract odds ratios and confidence intervals
# odds_ratios <- exp(coef(multi_logistic_model))
# conf_int <- exp(confint(multi_logistic_model))
# 
# # Print results
# results_table <- data.frame(
#   Variable = names(odds_ratios),
#   OR = odds_ratios,
#   CI_Lower = conf_int[, 1],
#   CI_Upper = conf_int[, 2]
# )
# 
# # ORs suggest 2023 is statistically significant departure from 2021 in terms of climate change anxiety
# print(results_table) 
# 
# library(broom)     # For tidy model output
# library(dplyr)     # For data manipulation
# 
# tidy_model <- tidy(multi_logistic_model, conf.int = TRUE, exponentiate = TRUE)
# 
# tidy_model$adj.p.value <- p.adjust(tidy_model$p.value, method = "fdr")
# 
# results_table <- tidy_model %>%
#   select(term, estimate, conf.low, conf.high, p.value, adj.p.value) %>%
#   mutate(across(where(is.numeric), ~ round(., 6))) %>%
#   rename(
#     Characteristic = term,
#     `Odds Ratio` = estimate,
#     `95% CI Lower` = conf.low,
#     `95% CI Upper` = conf.high,
#     `P-value` = p.value
#   )
# 
# print(results_table)
# write.csv(results_table, "climate_distress_model_table.csv", row.names = FALSE)
# 
# 
# # Plot this in a line plot
# # Load necessary package
# library(effects)
# 
# # Compute the marginal effect of year (adjusted for covariates)
# year_effect <- effect("year", multi_logistic_model, type = "response")
# 
# # Convert to dataframe
# year_df <- as.data.frame(year_effect)
# year_df
# 
# # Filter out the 2021.5 and 2022.5
# year_df <- subset(year_df, year %in% c("2021", "2022", "2023"))
# year_df
# # Plot
# library(ggplot2)
# library(emmeans)             # Every session
# 
# p <- ggplot(year_df, aes(x = factor(year), y = fit)) +
#   geom_point(size = 3, color = "black") +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, color = "black") +
#   labs(
#     title = "Adjusted Probability of Climate Distress by Year",
#     x = "Year",
#     y = "Predicted Probability (Adjusted)"
#   ) +
#   theme_classic(base_size = 12) +
#   theme(
#     axis.line = element_line(color = "black"),
#     axis.ticks = element_line(color = "black"),
#     panel.grid = element_blank(),
#     panel.border = element_blank(),
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     axis.text = element_text(color = "black"),
#     axis.title = element_text(face = "bold")
#   )
# 
# p
# ggsave("Climate_Distress_By_Year.pdf", plot = p, width = 11, height = 8.5)
# 
# # Fit the model with interaction: year * sex
# model_sex_interaction <- svyglm(climate_distress_bin ~ year * srsex + srage_p + ombsrtn_p1 + povll, 
#                                 design = chis_design,
#                                 family = quasibinomial())
# 
# summary(model_sex_interaction)
# 
# # Compute the marginal effect of year by sex
# sex_effect <- effect("year*srsex", model_sex_interaction, type = "response")  # ensure probabilities
# sex_df <- as.data.frame(sex_effect)
# 
# # emms by sex
# emms_sex <- emmeans(model_sex_interaction, ~ year * srsex, type = "response")
# 
# contrast(emms_sex, method = "pairwise", by = "year", adjust = "tukey")
# 
# # Plot
# library(ggplot2)
# sex_df$srsex <- factor(sex_df$srsex)
# 
# table(data$climate_distress_bin, data$srsex)
# prop.table(table(data$climate_distress_bin, data$srsex), 2)  # column-wise %
# table(data$srsex, data$year)
# 
# summary(weights(chis_design))
# summary(weights(chis_design)[data$year == "2023"])
# 
# p<-ggplot(sex_df, aes(x = year, y = fit, color = srsex, group = srsex)) + 
#   geom_point(size = 3) + 
#   geom_line(size = 1) + 
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) + 
#   scale_color_manual(
#     values = c("Male" = "blue", "Female" = "deeppink"),
#     labels = c("Male" = "Male", "Female" = "Female"),
#     name = "Sex"
#   ) +
#   labs(
#     title = "Adjusted Probability of Climate Distress by Year and Sex",
#     x = "Survey Year",
#     y = "Predicted Probability (Adjusted)"
#   ) +
#   theme_classic(base_size = 12) +
#   theme(
#     axis.line = element_line(color = "black"),
#     axis.ticks = element_line(color = "black"),
#     panel.grid = element_blank(),
#     panel.border = element_blank(),
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     axis.text = element_text(color = "black"),
#     axis.title = element_text(face = "bold"),
#     legend.title = element_text(face = "bold"),
#     legend.text = element_text(color = "black")
#   )
# 
# p
# ggsave("Climate_Distress_By_Sex.pdf", plot = p, width = 11, height = 8.5)
# 
# chis_design <- update(chis_design, ombsrtn_p1 = data$ombsrtn_p1)
# chis_design$variables$ombsrtn_p1
# 
# # Run model
# # Fit the model with interaction: year * sex
# model_race_interaction <- svyglm(climate_distress_bin ~ year * ombsrtn_p1 + srage_p + srsex + povll,
#                                  design = chis_design,
#                                  family = quasibinomial())
# 
# 
# summary(model_race_interaction)
# 
# 
# race_effect <- effect("year*ombsrtn_p1", model_race_interaction, type = "response")
# race_df <- as.data.frame(race_effect)
# race_df
# 
# library(emmeans)
# 
# emms <- emmeans(model_race_interaction, ~ year * ombsrtn_p1, type = "response")
# contrast(emms, method = "pairwise", by = "year", adjust = "tukey")
# 
# p <- ggplot(race_df, aes(x = factor(year), y = fit, color = ombsrtn_p1, group = ombsrtn_p1)) +
#   geom_point(size = 3) +
#   geom_line(size = 1) +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
#   labs(
#     title = "Adjusted Probability of Climate Distress by Year and Race",
#     x = "Year",
#     y = "Predicted Probability (Adjusted)",
#     color = "Race"
#   ) +
#   theme_classic(base_size = 12) +
#   theme(
#     axis.line = element_line(color = "black"),
#     axis.ticks = element_line(color = "black"),
#     panel.grid = element_blank(),
#     panel.border = element_blank(),
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     axis.text = element_text(color = "black"),
#     axis.title = element_text(face = "bold"),
#     legend.title = element_text(face = "bold"),
#     legend.text = element_text(color = "black")
#   ) +
#   scale_color_manual(values = c("steelblue", "darkred", "forestgreen", "orange", "purple")) +  # Optional: custom colors
#   guides(color = guide_legend(title = "Race/Ethnicity"))
# p
# ggsave("Climate_Distress_By_Race.pdf", plot = p, width = 11, height = 8.5)
# 
# # Do by age (binned)
# data$srage_p
# as.numeric(as.character(data$srage_p))
# data$age_group <- cut(
#   # Convert factor to numeric (preserving the actual numeric values)
#   data$srage_p <- as.numeric(as.character(data$srage_p)),
#   breaks = c(11, 14, 17),
#   labels = c("12–14", "15–17"),
#   right = TRUE
# )
# data$age_group <- factor(data$age_group)
# 
# chis_design <- update(chis_design, age_group= data$age_group)
# chis_design$variables$age_group
# 
# # Run model
# # Fit the model with interaction: year * sex
# model_age_interaction <- svyglm(climate_distress_bin ~ year * age_group + ombsrtn_p1 + srsex + povll,
#                                 design = chis_design,
#                                 family = quasibinomial())
# 
# library(effects)
# age_effect_binned <- effect("year*age_group", model_age_interaction, type = "response")
# age_df_binned <- as.data.frame(age_effect_binned)
# 
# summary(model_age_interaction)
# 
# library(ggplot2)
# 
# p <- ggplot(age_df_binned, aes(x = year, y = fit, color = age_group, group = age_group)) +
#   geom_point(size = 3) +
#   geom_line(size = 1) +
#   geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
#   labs(
#     title = "Climate Distress by Year and Age Group",
#     x = "Survey Year",
#     y = "Predicted Probability (Adjusted)",
#     color = "Age Group"
#   ) +
#   theme_classic(base_size = 12) +
#   theme(
#     axis.line = element_line(color = "black"),
#     axis.ticks = element_line(color = "black"),
#     panel.grid = element_blank(),
#     panel.border = element_blank(),
#     plot.title = element_text(hjust = 0.5, face = "bold"),
#     axis.text = element_text(color = "black"),
#     axis.title = element_text(face = "bold"),
#     legend.title = element_text(face = "bold"),
#     legend.text = element_text(color = "black")
#   )
# 
# p
# ggsave("Climate_Distress_By_Age_Binned.pdf", plot = p, width = 11, height = 8.5)
# # 
# # # Do by age unbinned
# # data$age_unbinned <- factor(data$srage_p)
# # data <- subset(data, srage_p %in% 12:17)
# # 
# # model_age_unbinned <- glm(
# #     tf45 ~ year * age_unbinned + srsex + ombsrtn_p1 + povll,
# #     data = data,
# #     family = binomial
# # )
# # 
# # age_effect_unbinned <- effect("year*age_unbinned", model_age_unbinned, type = "response")
# # age_df_unbinned <- as.data.frame(age_effect_unbinned)
# # 
# # ggplot(age_df_unbinned, aes(x = year, y = fit, color = age_unbinned, group = age_unbinned)) +
# #     geom_point(size = 3) +
# #     geom_line(size = 1) +
# #     geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1) +
# #     labs(
# #         title = "Adjusted Probability of Climate Distress by Year and Age (12–17)",
# #         x = "Survey Year",
# #         y = "Predicted Probability (Adjusted)",
# #         color = "Age"
# #     ) +
# #     theme_minimal()
# # 
# # # now do by unbinned
# # emms_unbinned <- emmeans(model_age_unbinned, ~ year * age_unbinned, type = "response")
# # 
# # contrast(emms_unbinned, method = "pairwise", by = "year", adjust = "tukey")
# # 
# # 
# # # If already 0/1:
# # prop.table(table(data$tf45))
# # 
# # 
# # # 4.3 Identify demographic subgroups where climate distress has changed significantly over time.
# # # Define key demographic variables for subgroup analysis
# # subgroups <- c("srsex", "srage_p", "ombsrtn_p1", "povll")  # Gender, Age, Race, Poverty Level
# # 
# # # Create an empty data frame before the loop
# # all_results <- data.frame(Variable = character(), OR = numeric(), 
# #                           CI_Lower = numeric(), CI_Upper = numeric(), 
# #                           P_values = numeric(), Significances = character())
# # 
# # 
# # # Loop over each subgroup and test interaction effect
# # for (var in subgroups) {
# #     cat("\n### Analyzing interaction effect for:", var, "###\n")
# #     
# #     # Fit logistic regression model with interaction term (Year * Subgroup)
# #     model <- glm(tf45 ~ factor(year) * factor(data[[var]]), data = data, family = binomial)
# #     
# #     # Display summary
# #     print(summary(model))
# #     
# #     # Extract odds ratios and confidence intervals
# #     ORs <- exp(coef(model))
# #     CI <- exp(confint(model))
# #     P_values <- summary(model)$coefficients[, 4]
# #     # Convert p-values to significance levels
# #     significance <- ifelse(P_values < 0.001, "***", 
# #                            ifelse(P_values < 0.01, "**", 
# #                                   ifelse(P_values < 0.05, "*", "-")))
# #     
# #     # Store results
# #     results_table <- data.frame(
# #         Variable = names(ORs),
# #         OR = ORs,
# #         CI_Lower = CI[, 1],
# #         CI_Upper = CI[, 2],
# #         P_values = P_values,
# #         Significances = significance
# #         )
# #     
# #     # Append results to the final table
# #     all_results <- rbind(all_results, results_table)
# #     
# # }
# # 
# # View(all_results)
# # 
# # # Filter the significant results for Race (ombsrtn_p1) and Poverty Level (povll)
# # significant_results <- all_results[
# #     (grepl("factor\\(year\\)2023:factor\\(ombsrtn_p1\\)", all_results$Variable) | 
# #          grepl("factor\\(year\\)2023:factor\\(povll\\)", all_results$Variable)) & 
# #          all_results$P_values < 0.05, 
# # ]
# # 
# # # View which race/poverty groups were significantly impacted
# # significant_results
# 
# ##### ANALYSIS #5 - CLIMATE CHANGE EVENTS AND CLIMATE CHANGE ANXIETY (DAC) ##########################################################
# # Link external climate exposure data to examine associations between environmental conditions and climate change distress.
# # Research questions:
# # Does a year-over-year increase in temperature (Δ temperature) predict greater climate change distress?
# 
# library(terra)
# library(terra)   # raster handling
# library(dplyr)   # joins / pipes
# library(tidyr)   # pivot_longer / wider
# library(lubridate)
# library(ggplot2)
# 
# getwd()
# 
# # Housekeeping 
# grib_path      <- "Data/682ccef8d1a03d877d6ccf4f0bc3845a.grib"
# climate_rast   <- rast(grib_path)            # full ERA-5 stack
# kelvin_rasts   <- climate_rast[[grep("temperature", names(climate_rast))]]
# celcius_rasts  <- kelvin_rasts - 273.15      # convert once, keep as °C
# 
# # Your county polygons 
# # `city_shapefile` already loaded earlier
# california_shapefile <- city_shapefile |>
#   filter(STATEFP == "06") |>
#   mutate(COUNTYFP_int = as.integer(COUNTYFP))
# 
# head(california_shapefile)
# 
# california_vect <- vect(california_shapefile)   # terra SpatVector
# 
# # (1) Create clean names for each layer based on known time dimension
# layer_dates <- seq.Date(from = as.Date("2018-01-01"), by = "month", length.out = 84)
# names(celcius_rasts)
# print(paste("Original names:", head(names(celcius_rasts), 10)))
# names(celcius_rasts) <- paste0("t2m_", format(layer_dates, "%Y-%m"))
# 
# head(celcius_rasts)
# head(california_shapefile)
# 
# # (2) Extract county-aggregated means
# california_shapefile <- city_shapefile[city_shapefile$STATEFP == "06", ]
# extracted <- terra::extract(celcius_rasts, california_vect, fun = mean, na.rm = TRUE)
# 
# # (3) Add county metadata (e.g. names, GEOIDs)
# california_meta <- city_shapefile %>%
#   filter(STATEFP == "06") %>%
#   st_drop_geometry() %>%
#   mutate(ID = row_number())
# 
# temp_by_month <- left_join(extracted, california_meta, by = "ID")
# 
# # View(temp_by_month)
# 
# # (4) Pivot to long format
# temp_by_month_long <- temp_by_month %>% pivot_longer(
#   cols=starts_with("t2m"),
#   names_to = "t2m_date",
#   values_to= "mean temp"
# )
# 
# # View(temp_by_month_long)
# 
# # (5) For each county, calculate
# 
# # Absolute mean temp
# # Heatwave days - the number of days over a high-temperature threshold (e.g., >95°F or >32°C) within a certain lookback window (e.g., 30, 60, 90 days before interview).
# # Year-over-year temp delta (e.g., ∆mean annual temp)
# 
# # 1. Standardize the columns
# data <- dummy_data
# data$tadate_year <- substr(data$tadate_mm, 1, 4)
# data$tadate_month <- substr(data$tadate_mm, 5, 6)
# 
# data$COUNTYFP <- sprintf("%03d", data$fips_cnt) # convert to COUNTYFP
# data$t2m_date <- sprintf("t2m_%04d-%02d", data$tadate_mm %/% 100, data$tadate_mm  %% 100)
# 
# temp_by_month_long$t2m_year <- substr(temp_by_month_long$t2m_date, 5, 8)
# temp_by_month_long$t2m_month <-substr(temp_by_month_long$t2m_date, 10, 11)
# 
# # (5) For each participant, calculate:
# 
# # Absolute mean temp
# # Heatwave days - the number of days over a high-temperature threshold (e.g., >95°F or >32°C) within a certain lookback window (e.g., 30, 60, 90 days before interview).
# # Year-over-year temp delta (e.g., ∆mean annual temp)
# # Wildfire events
# 
# ### Absolute mean temp and year-over-year temp delta (e.g., ∆mean annual temp)
# 
# # 1. Standardize the columns
# data <- dummy_data
# data$tadate_year <- substr(data$tadate_mm, 1, 4)
# data$tadate_month <- substr(data$tadate_mm, 5, 6)
# 
# data$COUNTYFP <- sprintf("%03d", data$fips_cnt) # convert to COUNTYFP
# data$t2m_date <- sprintf("t2m_%04d-%02d", data$tadate_mm %/% 100, data$tadate_mm  %% 100)
# 
# temp_by_month_long$t2m_year <- substr(temp_by_month_long$t2m_date, 5, 8)
# temp_by_month_long$t2m_month <-substr(temp_by_month_long$t2m_date, 10, 11)
# 
# 
# # 2. Pull the ith participant
# 
# # Preallocate empty vectors
# ids <- c()
# counties <- c()
# interview_dates <- c()
# mean_temps <- c()
# rolling_means <- c()
# deltas <- c()
# distress <- c()
# 
# for (i in seq(1:nrow(data))) {
#   
#   print(i)
#   ith <- data[i,]
#   ith_t2m_month <- ith$tadate_month
#   ith_t2m_year <- ith$tadate_year
#   
#   ith_date <- ith$t2m_date
#   ith_county <- ith$COUNTYFP
#   ith_distress <- ith$tf45
#   
#   # 3. Get mean temperature for this participant's interview specific date and county
#   mean_temp <- temp_by_month_long[temp_by_month_long$COUNTYFP %in% ith_county & temp_by_month_long$t2m_date %in% ith_date,'mean temp']
#   mean_temp <- mean_temp[[1]]
#   
#   # 4. Get historical mean temperature
#   # 4a. Get all rows from same county and same calendar month (e.g., all November or "11" months in Calaveras)
#   
#   historical_mask <- temp_by_month_long$t2m_month %in% ith_t2m_month & 
#     temp_by_month_long$COUNTYFP %in% ith_county &
#     as.integer(temp_by_month_long$t2m_year) %in%
#     ((as.integer(ith_t2m_year) - 3):(as.integer(ith_t2m_year) - 1))
#   
#   temp_by_month_rolling <- temp_by_month_long[historical_mask,] # Filter by date that have the right month and county
#   
#   # 4b. Calculate the rolling average
#   rolling_mean_temp <- mean(temp_by_month_rolling$`mean temp`)
#   delta_mean_temp <- mean_temp - rolling_mean_temp
#   
#   # 5. Add to vectors
#   # Append to vectors
#   ids <- c(ids, i)
#   counties <- c(counties, ith_county)
#   interview_dates <- c(interview_dates, ith_date)
#   mean_temps <- c(mean_temps, mean_temp)
#   rolling_means <- c(rolling_means, rolling_mean_temp)
#   deltas <- c(deltas, delta_mean_temp)
#   distress <- c(distress, ith_distress)
# }
# 
# 
# # Combine into final data frame once
# results <- data.frame(
#   id = ids,
#   COUNTYFP = counties,
#   interview_date = interview_dates,
#   mean_temp = mean_temps,
#   rolling_mean_temp = rolling_means,
#   delta_mean_temp = deltas,
#   distress = distress
# )
# 
# # First, make sure `results` has a matching ID to `combined`
# d_combined$computed_mean_temp <- results$mean_temp
# d_combined$computed_rolling   <- results$rolling_mean_temp
# d_combined$computed_delta     <- results$delta_mean_temp
# d_combined$distress_bin       <- ifelse(combined$tf45 == 2, 1, 0)
# 
# # Then update your survey design
# d_chis_design <- update(
#   d_chis_design,
#   mean_temp = d_combined$computed_mean_temp,
#   rolling_mean_temp = d_combined$computed_rolling,
#   delta_mean_temp = d_combined$computed_delta,
#   distress_bin = d_combined$distress_bin
# )
# 
# d_combined %>%
#   summarise(
#     n = n(),
#     missing_distress = sum(is.na(distress_bin)),
#     missing_temp     = sum(is.na(computed_mean_temp)),
#     missing_delta    = sum(is.na(computed_delta)),
#     missing_roll     = sum(is.na(computed_rolling)),
#     infinite_temp    = sum(is.infinite(computed_mean_temp)),
#     infinite_delta   = sum(is.infinite(computed_delta)),
#     infinite_roll    = sum(is.infinite(computed_rolling))
#   )
# 
# svyvar(~mean_temp + delta_mean_temp + rolling_mean_temp, d_chis_design)
# 
# model_svy <- svyglm(
#   distress_bin ~ delta_mean_temp + rolling_mean_temp,
#   design = d_chis_design,
#   family = quasibinomial()
# )
# 
# cor(d_combined[, c("computed_mean_temp", "computed_delta", "computed_rolling")])
# 
# summary(model_svy)
# exp(coef(model_svy))  # odds ratios
# 
# 
# 
# 
# 
# 
# model <- glm(distress ~ mean_temp + delta_mean_temp + rolling_mean_temp,
#              data = results, family = binomial)
# 
# summary(model)
# exp(coef(model))  # Get odds ratios
# 
# ggplot(results, aes(x = delta_mean_temp, fill = as.factor(distress))) +
#   geom_density(alpha = 0.5) +
#   labs(title = "Delta Temp Distribution by Distress", fill = "Distress")
# 
# 
# ### Heatwave days (IGNORE - INCOMPLETE!)
# # Housekeeping 
# # Define the folder where your .nc files are stored
# nc_folder <- "/Users/danielzhao/Downloads/476f67210608f231a3ffb21545cb768c"
# 
# # List all .nc files in that folder
# nc_files <- list.files(nc_folder, pattern = "\\.nc$", full.names = TRUE)
# 
# # Load each .nc file into a SpatRaster and combine
# utci_stack <- rast(nc_files)  # terra will merge them into a multilayer SpatRaster
# utci_celsius <- utci_stack - 273.15 # Change to celcius
# 
# # Check if your raster has time information
# print("Time information:")
# print(time(utci_celsius))
# print(paste("Number of time steps:", length(time(utci_celsius))))
# print(paste("Number of layers:", nlyr(utci_celsius)))
# 
# # Create a daily grouping index
# daily_index <- format(time(utci_celsius), "%Y-%m-%d")
# unique_days <- unique(daily_index)
# daily_group <- match(daily_index, unique_days)
# 
# # Aggregate to daily **maximum** UTCI (conservative for heat stress)
# utci_daily_max <- tapp(utci_celsius, index = daily_group, fun = max, na.rm = TRUE)
# 
# # Use terra extract instead - much simpler
# county_daily_utci <- terra::extract(
#   utci_daily_max, 
#   california_vect,  # SpatVector works with terra
#   fun = mean, 
#   na.rm = TRUE,
#   ID = TRUE
# )
# 
# library(dplyr)
# library(tidyr)
# library(lubridate)
# 
# colnames(county_daily_utci)[-1] <- as.character(utci_dates)
# # Do this instead:
# colnames(county_daily_utci)
# # (1) Add county metadata (e.g. names, GEOIDs)
# california_meta <- city_shapefile %>%
#   filter(STATEFP == "06") %>%
#   st_drop_geometry() %>%
#   mutate(ID = row_number())
# 
# county_daily_utci <- left_join(county_daily_utci, california_meta, by = "ID")
# 
# # 2. Get date vector and county names
# utci_dates <- as.Date(time(utci_daily_max))  # already defined
# county_ids <- california_vect$COUNTYFP       # or whichever ID you use in `california_vect`
# 
# # 2. Convert wide to long format (exclude ID column)
# df_long <- county_daily_utci %>%
#   as.data.frame() %>%
#   rename(county_id = ID) %>%
#   pivot_longer(cols = starts_with("day_"),
#                names_to = "day_index",
#                values_to = "utci") %>%
#   mutate(date = utci_dates[as.integer(gsub("day_", "", day_index))])
# 
# View(df_long)
# 
# # 3. Add year/month columns
# df_long <- df_long %>%
#   mutate(year = year(date),
#          month = month(date))
# 
# # 4. Filter where UTCI > 32 and count per county-month
# heatwave_counts <- df_long %>%
#   filter(utci > 32) %>%
#   group_by(county_id, year, month) %>%
#   summarise(heatwave_days = n(), .groups = "drop")
# 
# # 5. Optional: join with counties that had *zero* heatwave days
# # to get a full grid of (county, year, month)
# all_combos <- expand.grid(
#   county_id = unique(df_long$county_id),
#   year = unique(df_long$year),
#   month = 1:12
# )
# 
# heatwave_full <- all_combos %>%
#   left_join(heatwave_counts, by = c("county_id", "year", "month")) %>%
#   mutate(heatwave_days = replace_na(heatwave_days, 0))
# 
# # View result
# head(heatwave_full)