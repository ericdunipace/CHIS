### Disparities research in barriers to mental health #############################################

### Import and read stuff in ######################################################################
# Load required libraries (ensure they are installed)
library(dplyr)       # Data manipulation
library(haven)       # Reading .dta files
library(survey)      # Survey data analysis
library(ggplot2)     # Data visualization
library(table1)      # Demographic tables
library(sf)          # Spatial data handling and mapping
library(purrr)
library(gtsummary)
library(broom.helpers)
library(here)

# Step 1: 
# Set Wd

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Sets to script location
# setwd("..")  # Moves up to the project root
# getwd()

# Import confidential dummy datasets
d_chis_2023 <- haven::read_dta(here::here('Data','dummyfile_2023_teen_stata/TEEN.dta')) %>% ## ED: had question about whether the other datasets on google drive are same?
    rename_all(tolower) %>%
    mutate(year = 2023)

d_chis_2022 <- haven::read_dta(here::here('Data','teen 2022 dummy STATA/TEEN.dta')) %>%
    rename_all(tolower) %>%
    mutate(year = 2022)

d_chis_2021 <- haven::read_dta(here::here('Data','dummyfile_2021_teen_stata/TEEN.dta')) %>%
    rename_all(tolower) %>%
    mutate(year = 2021)

# Import public use variable datasets
# '/Users/danielzhao/Desktop/CHIS_Youth/Data/teen_2023_stata'
chis_2023 <- haven::read_dta(here::here('Data','teen_2023_stata/TEEN.dta')) %>%
    rename_all(tolower) %>%
    mutate(year = 2023)

chis_2022 <- haven::read_dta(here::here('Data','teen_stata_2022/TEEN.dta')) %>%
    rename_all(tolower) %>%
    mutate(year = 2022)

chis_2021 <- haven::read_dta(here::here('Data','teen_stata_2021/TEEN.dta')) %>%
    rename_all(tolower) %>%
    mutate(year = 2021)

# Add dummy data to list
my_chis_list <- list(d_chis_2021, d_chis_2022, d_chis_2023) # work with confidential data
# my_chis_list <- list(chis_2021, chis_2022, chis_2023) # work with PUF data

# Pooling function to standardize and merge datasets
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

# Pool the datasets together
combined <- pooling(my_chis_list)

# Set up survey design for analysis
chis_design <- svrepdesign(
    data = combined,
    weights = ~ fnwgt0,
    repweights = "fnwgt[1-9]",
    type = "other",
    scale = 1,
    rscales = 1,
    mse = TRUE
    )


# Preview spread of basic variables
hist(chis_design$variables$srage_p)
hist(chis_design$variables$year)
hist(chis_design$variables$tf45)

# Remove rows with missing values in key variables
chis_data_filtered <- chis_design$variables[complete.cases(
    chis_design$variables[, c("tf45", "year", "srage_p", "srsex")]), ]

# Recode tf45: Convert "2" to 1 and "1" to 0 (assumed binary outcome)
chis_data_filtered$tf45 <- ifelse(chis_data_filtered$tf45 == "2", 1, 0) ## ED: depends on what 2 is in your dataset

# Assign the cleaned dataset for further analysis
data <- chis_data_filtered


#### TABLE 1 - DEMOGRAPHICS #########################################################################

# Define baseline demographic variables to be included in the table
baseline_demographics <- c(
    "srage_p",      # SELF-REPORTED AGE
    "srsex",        # SELF-REPORTED GENDER
    "ombsrtn_p1",   # OMB/CURRENT DOF RACE - ETHNICITY
    "racecnt_p1",   # RACE - CENSUS 2000 DEFINITION
    "schtyp_p1",    # TYPE OF SCHOOL ATTENDED
    "ta4c_p1",      # ATTENDED SCHOOL DURING LAST SCHOOL YR
    "ahedtc_p1",    # ADULT EDUCATIONAL ATTAINMENT
    "povgwd_p1",    # FAMILY POVERTY THRESHOLD LEVEL
    "povll",        # POVERTY LEVEL
    "lnghmt_p1"     # LANGUAGE SPOKEN AT HOME
    );

# Convert key variables to factors for better summary tables
chis_data_filtered$srage_p <- factor(chis_data_filtered$srage_p, levels = c("13", "14", "15", "16", "17"))
chis_data_filtered$ombsrtn_p1 <- factor(chis_data_filtered$ombsrtn_p1)
chis_data_filtered$srsex <- factor(chis_data_filtered$srsex)
chis_data_filtered$povll <- factor(chis_data_filtered$povll)
chis_data_filtered$lnghmt_p1 <- factor(chis_data_filtered$lnghmt_p1)

# Label variables for better readability
label(chis_data_filtered$srage_p) <- "Age"
label(chis_data_filtered$ombsrtn_p1) <- "Race/Ethnicity"
label(chis_data_filtered$srsex) <- "Sex"
label(chis_data_filtered$povll) <- "Poverty Level"
label(chis_data_filtered$lnghmt_p1) <- "Language Spoken at Home"
label(chis_data_filtered$tf45) <- "Total"

# Create demographic table grouped by tf45
demo_table <- table1(
    ~ srage_p + ombsrtn_p1 + srsex + povll + lnghmt_p1 | tf45,
    data = chis_data_filtered,
    overall = "Overall" # Include overall column
    );

# Display the table
print(demo_table)

#### ANALYSIS #1- SPATIAL HEATMAP #####################################################################################

# # Load shapefile of the city
# city_shapefile <- sf::st_read("Data/tl_2024_us_county/tl_2024_us_county.shp")
# 
# # Filter by California
# california_shapefile <- city_shapefile[city_shapefile$STATEFP %in% '06',]

california_shapefile <- readRDS(here::here('Data','ca_county.rds'))
# Generate city data
# Average the anxiety scores by county
chis_data_filtered$fips_cnt

# Aggregate and average anxiety scores by county
result <- group_by(chis_data_filtered, fips_cnt)

result <- summarize(result, ClimateAnxiety = mean(tf45)) ## ED: this doesn't use weights for the means

# COUNTYFP and fips_cnt need to be standardized - both three digit ints

california_shapefile$COUNTYFP_int <- as.integer(sprintf(fmt = california_shapefile$COUNTYFP))

# Left join 
california_heatmap <- left_join(california_shapefile, result, by = c("COUNTYFP_int" = "fips_cnt"))

# Plot heatmap - labelled
ggplot(california_heatmap) +
    geom_sf(aes(fill = ClimateAnxiety)) +
    geom_sf_text(aes(label = NAME), size = 3) +
    scale_fill_gradientn(colors = c("green", "yellow", "orange", "red")) +
    theme_minimal() + 
    theme(
        panel.grid = element_blank(),         # Remove gridlines
        axis.title = element_blank(),         # Remove axis titles
        axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
        axis.ticks = element_blank()          # Remove axis ticks
    );

# Plot heatmap - unlabelled (there are too many labels so we can manually label select counties)
ggplot(california_heatmap) +
    geom_sf(aes(fill = ClimateAnxiety)) +
    geom_sf_text(aes(label = NAME), size = 0) +
    scale_fill_gradientn(colors = c("green", "yellow", "orange", "red")) +
    theme_minimal() + 
    theme(
        panel.grid = element_blank(),         # Remove gridlines
        axis.title = element_blank(),         # Remove axis titles
        axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
        axis.ticks = element_blank()          # Remove axis ticks
    );

### ANALYSIS #1B - SPATIAL HEATMAP - LOS ANGELES ####################################################################
### STILL IN PROGRESS - NO NEED TO RUN THIS CODE

# Filter California shapefile for Los Angeles County (FIPS = 037)
la_shapefile <- california_shapefile[california_shapefile$COUNTYFP == '037',]

# Filter the dataset for Los Angeles County (fips_cnt should match COUNTYFP)
chis_data_la <- chis_data_filtered[chis_data_filtered$fips_cnt == 37,]
View(chis_data_la)
View(la_shapefile)

# Standardize COUNTYFP in the shapefile to match fips_cnt
la_shapefile$COUNTYFP_int <- as.integer(sprintf("%03d", as.integer(la_shapefile$COUNTYFP)))

# Join the Los Angeles shapefile with the aggregated anxiety data by the sublevel data structure in LA***
la_heatmap <- left_join(la_shapefile, chis_data_la, by = c("COUNTYFP_int" = "fips_cnt"))


#### ANALYSIS #2- CLIMATE CHANGE/MENTAL HEALTH #####################################################################################
# Goal: To assess if climate anxiety is associated with worsened mental health symptoms (eg, nervousness, distress, depressed)

#### UNIVARIATE ANALYSES
# Define mental health variables of interest
mental_health_issues <- c( ## ED: seems like we can cut these
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
        ); ## ED: the standard errors are wrong here
    
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

View(model.results)

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
        ); ## ED: standard errors are wrong here
    
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

##### ANALYSIS #3 - VULNERABLE SUBGROUPS (CONFIDENTIAL DATA) ##########################################################

# Goal: to identify certain subgroups most vulnerable to climate change anxiety
baseline_demographics <- c(
    "srage_p",      # SELF-REPORTED AGE
    "srsex",        # SELF-REPORTED GENDER
    "ombsrtn_p1",   # OMB/CURRENT DOF RACE - ETHNICITY
    "racecnt_p1",   # RACE - CENSUS 2000 DEFINITION
    "schtyp_p1",    # TYPE OF SCHOOL ATTENDED
    "ta4c_p1",      # ATTENDED SCHOOL DURING LAST SCHOOL YR
    "ahedtc_p1",    # ADULT EDUCATIONAL ATTAINMENT
    "povgwd_p1",    # FAMILY POVERTY THRESHOLD LEVEL
    "povll",        # POVERTY LEVEL
    "lnghmt_p1"     # LANGUAGE SPOKEN AT HOME
    );


modifiable_protective <- c(
    "povgwd",       # FAMILY POVERTY THRESHOLD LEVEL
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
    "tb19",         # ER/URGENT CARE VISIT FOR ASTHMA IN PAST YEAR
    "tb28",         # ER/URGENT CARE VISIT FOR ASTHMA IN PAST
    "forgo",        # HAD TO FORGO NECESSARY CARE
    "instype",      # INSURANCE TYPE
    "tf2",          # KIND OF PLACE MOST OFTEN GO FOR HEALTH CARE
    "tf9",          # DELAYED/DID NOT GET MEDICAL CARE FELT NEEDED IN PAST 12 MOS
    "th57"          # EVENTUALLY RECEIVED MED CARE THAT WAS DELAYED
    );

receiving_mental_healthcare <- c(
    "tf11"          # RECVD PSYCHOLOGICAL/EMOTIONAL COUNSELING IN PAST 12 MOS
    );

civic_engagement <- c(
    "sch_typ",      # TYPE OF SCHOOL ATTENDED
    "ta4",          # ATTENDED SCHOOL LAST WEEK
    "ta4c",         # ATTENDED SCHOOL DURING LAST SCHOOL YR
    "tb4",          # # OF DAYS OF SCHOOL MISSED FOR HEALTH PROBLEM PAST MO
    "tb24",         # # OF SCHOOL DAYS MISSED DUE TO ASTHMA IN PAST 12 MOS
    "tl10",         # PARTICIPATE IN CLUBS/ORGS OUTSIDE SCHOOL PAST YR
    "tq15"          # HOW OFTEN FELT SENSE OF BELONGING AT SCHOOL
    );

# Define the covariates to be used in the multivariate analyses
covariates <- c(
    "srage",       # Age
    "srsex",       # Gender
    "ombsrreo",    # Race/Ethnicity
    "tq9",         # Family financial stress
    "instype"      # Insurance type
    );


data <- chis_data_filtered

# Aggregate all the variables
characteristics <- c(
    baseline_demographics, 
    modifiable_protective, 
    access_to_care, 
    receiving_mental_healthcare, 
    civic_engagement
    )

# Check to see if all the predictor variables are in dataset
characteristics %in% colnames(data)

# If using PUF data, just subset to the variables that are avaiable 
characteristics <- characteristics[characteristics %in% colnames(data)]

# Initialize vectors to store results for all levels of all variables
characteristic_names <- c()  # To store the variable name for each level
level_names <- c()           # To store the level name for each coefficient
p.values <- c()              # To store p-values for all levels
odds.ratios <- c()           # To store odds ratios for all levels
ci.lower <- c()              # To store lower bounds of confidence intervals
ci.upper <- c()              # To store upper bounds of confidence intervals
significances <- c()         # To store significance stars for all levels


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
        ); ## ED: Standard errors wrong
    
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


# Create dataframe with bootstrap results
model.results <- data.frame(
    characteristic_names,
    level_names,
    odds.ratios,
    ci.lower,
    ci.upper,
    odds.ratios,
    significances
    );

View(model.results)

#### MULTIVARIATE

data <- chis_data_filtered

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

View(model.results)


##### ANALYSIS #4 - TEMPORAL ANALYSIS ##########################################################
# 4.1 How does climate change distress change over time? 

# Create an empty data frame to store results
climate_distress_trend <- data.frame(
    year = c(2021, 2022, 2023),
    Total_Respondents = NA,
    Climate_Distress_Count = NA,
    Climate_Distress_Percentage = NA
    )

# Loop through years and calculate prevalence of climate distress
for (i in 1:nrow(climate_distress_trend)) {
    year_i <- climate_distress_trend$year[i]
    
    # Subset data for the year
    data_year <- data[data$year == year_i, ]
    
    # Calculate total respondents
    climate_distress_trend$Total_Respondents[i] <- nrow(data_year)
    
    # Calculate number of respondents with climate distress (tf45 == 1)
    climate_distress_trend$Climate_Distress_Count[i] <- sum(data_year$tf45 == 1, na.rm = TRUE) ## ED: Standard errors wrong
    
    # Calculate percentage of respondents with climate distress
    climate_distress_trend$Climate_Distress_Percentage[i] <- 
        (climate_distress_trend$Climate_Distress_Count[i] / climate_distress_trend$Total_Respondents[i]) * 100 ## ED: needs weights
}

# Print the summary table
print(climate_distress_trend)

# Visualize the trend over time
ggplot(climate_distress_trend, aes(x = year, y = Climate_Distress_Percentage)) +
    geom_line(size = 1, color = "blue") +
    geom_point(size = 3, color = "red") +
    labs(
        title = "Trend of Climate Change Distress (2021-2023)",
        x = "Year",
        y = "Percentage Reporting Climate Distress"
    ) +
    theme_minimal()


# 4.2 Test whether climate distress prevalence has significantly changed over time.
# Fit logistic regression model predicting climate distress (tf45) using year
logistic_model <- glm(tf45 ~ factor(year), data = data, family = binomial)

# Display model summary
summary(logistic_model)

# Extract odds ratios and confidence intervals
odds_ratios <- exp(coef(logistic_model))
conf_int <- exp(confint(logistic_model))

# Print results
results_table <- data.frame(
    Variable = names(odds_ratios),
    OR = odds_ratios,
    CI_Lower = conf_int[, 1],
    CI_Upper = conf_int[, 2]
    )

# ORs suggest 2023 is statistically significant departure from 2021 in terms of climate change anxiety
print(results_table) 

# 4.3 Identify demographic subgroups where climate distress has changed significantly over time.
# Define key demographic variables for subgroup analysis
subgroups <- c("srsex", "srage_p", "ombsrtn_p1", "povll")  # Gender, Age, Race, Poverty Level

# Create an empty data frame before the loop
all_results <- data.frame(Variable = character(), OR = numeric(), 
                          CI_Lower = numeric(), CI_Upper = numeric(), 
                          P_values = numeric(), Significances = character())


# Loop over each subgroup and test interaction effect
for (var in subgroups) {
    cat("\n### Analyzing interaction effect for:", var, "###\n")
    
    # Fit logistic regression model with interaction term (Year * Subgroup)
    model <- glm(tf45 ~ factor(year) * factor(data[[var]]), data = data, family = binomial) ## ED: doesn't account for weights
    
    # Display summary
    print(summary(model))
    
    # Extract odds ratios and confidence intervals
    ORs <- exp(coef(model))
    CI <- exp(confint(model))
    P_values <- summary(model)$coefficients[, 4]
    # Convert p-values to significance levels
    significance <- ifelse(P_values < 0.001, "***", 
                           ifelse(P_values < 0.01, "**", 
                                  ifelse(P_values < 0.05, "*", "-")))
    
    # Store results
    results_table <- data.frame(
        Variable = names(ORs),
        OR = ORs,
        CI_Lower = CI[, 1],
        CI_Upper = CI[, 2],
        P_values = P_values,
        Significances = significance
        )
    
    # Append results to the final table
    all_results <- rbind(all_results, results_table)
    
}

View(all_results)

# Filter the significant results for Race (ombsrtn_p1) and Poverty Level (povll)
significant_results <- all_results[
    (grepl("factor\\(year\\)2023:factor\\(ombsrtn_p1\\)", all_results$Variable) | 
         grepl("factor\\(year\\)2023:factor\\(povll\\)", all_results$Variable)) & 
         all_results$P_values < 0.05, 
]

# View which race/poverty groups were significantly impacted
significant_results



 ##### ANALYSIS #5 - CLIMATE CHANGE EVENTS AND CLIMATE CHANGE ANXIETY ##########################################################
# Need to link external climate change events dataset to associate climate change events and climate change anxiety
# Pending Dr. Obradovich's reply to email asking for external dataset of zipcode-level climate change events to link to CHIS





### MISC. CODE - DISREGARD ########################################################################

data <- chis_data_filtered

baseline_demographics <- c(
    "srage",        # SELF-REPORTED AGE
    "srsex",        # SELF-REPORTED GENDER
    "povll",        # POVERTY LEVEL
    "povll2",       # POVERTY LEVEL AS TIMES OF 100% FPL
    "langhome",     # LANGUAGE SPOKEN AT HOME
    "ombsrreo",     # OMB/CURRENT DOF RACE - ETHNICITY
    "racecen",      # RACE - CENSUS 2000 DEFINITION
    "racecn_a",     # RACE OF ADULT - CENSUS 2000 DEFINITION
    "ta4c",         # ATTENDED SCHOOL DURING LAST SCHOOL YEAR
    
    "tb20",         # HEALTH PROFESSIONAL EVER GAVE ASTHMA MANAGEMENT PLAN
    "tb33",         # CONFIDENT CAN MANAGE ASTHMA
    "covrdca",      # PRIVATE/EMP BASED COVERAGE FROM COVERED CALIFORNIA
    "cc_subs",      # SUBSIDY AND TYPE OF COVERED CALIFORNIA COVERAGE
    "elgmagi64",    # PUBLIC COVERAGE FOR INDIVIDUALS <65 YRS: ACA MAGI (3 LVLS)
    
    "tq7",          # EVER BEEN TREATED UNFAIRLY DUE TO RACE/ETHNICITY
    "tq9"           # HOW OFTEN HARD TO GET BY ON FAMILY'S INCOME
)

modifiable_protective <- c(
    "povgwd",       # FAMILY POVERTY THRESHOLD LEVEL
    "tl25",         # CARES DEEPLY ABOUT ISSUES IN COMMUNITY
    "tl27",         # BELIEVES CAN MAKE A DIFFERENCE IN THE COMMUNITY
    "tl50",         # EVER VOLUNTEERED TO SOLVE PROBLEM IN THE COMMUNITY
    "tl53",         # CONFIDENCE TO CONTACT SOMEONE IN THE GOVT WHO REPRESENTS COMMUNITY
    "tq10",         # HOW OFTEN FELT ABLE TO TALK TO FAMILY ABOUT FEELINGS
    "tq11",         # HOW OFTEN FELT FAMILY STOOD BY YOU DURING DIFFICULT TIMES
    "tq14",         # HOW OFTEN FELT SUPPORTED BY FRIENDS
    "tq16"          # HOW OFTEN ENJOYED PARTICIPATING IN COMMUNITY TRADITIONS
)


access_to_care <- c(
    "tb19",         # ER/URGENT CARE VISIT FOR ASTHMA IN PAST YEAR
    "tb28",         # ER/URGENT CARE VISIT FOR ASTHMA IN PAST
    "forgo",        # HAD TO FORGO NECESSARY CARE
    "instype",      # INSURANCE TYPE
    "tf2",          # KIND OF PLACE MOST OFTEN GO FOR HEALTH CARE
    "tf9",          # DELAYED/DID NOT GET MEDICAL CARE FELT NEEDED IN PAST 12 MOS
    "th57"          # EVENTUALLY RECEIVED MED CARE THAT WAS DELAYED
)

receiving_mental_healthcare <- c(
    "tf11"          # RECVD PSYCHOLOGICAL/EMOTIONAL COUNSELING IN PAST 12 MOS
)

civic_engagement <- c(
    "sch_typ",      # TYPE OF SCHOOL ATTENDED
    "ta4",          # ATTENDED SCHOOL LAST WEEK
    "ta4c",         # ATTENDED SCHOOL DURING LAST SCHOOL YR
    "tb4",          # # OF DAYS OF SCHOOL MISSED FOR HEALTH PROBLEM PAST MO
    "tb24",         # # OF SCHOOL DAYS MISSED DUE TO ASTHMA IN PAST 12 MOS
    "tl10",         # PARTICIPATE IN CLUBS/ORGS OUTSIDE SCHOOL PAST YR
    "tq15"          # HOW OFTEN FELT SENSE OF BELONGING AT SCHOOL
)

data$ombsrtn_p1
covariates <- c(
    "srage_p",     # Age
    "srsex",       # Gender
    "ombsrreo",    # Race/Ethnicity
    "tq9",         # Family financial stress
    "instype",     # Insurance type
    "tq10",        # Family support (talk about feelings)
    "tq11",        # Family support (stood by during difficult times)
    "tq14",        # Support from friends
    "tb20",        # Chronic condition management
    "tb33"         # Confidence in managing chronic conditions
)


# for the univariate analyses:
# initialize arrays
characteristics = c(
    baseline_demographics, 
    modifiable_protective, 
    access_to_care, 
    receiving_mental_healthcare, 
    civic_engagement
    )

datalist = vector(mode = "list", length = length(characteristics))
characteristics %in% colnames(data)
characteristics <- characteristics[characteristics %in% colnames(data)]

for (i in 1:length(characteristics)) {
    
    ## Univariable: create a formula for a model with a large number of variables
    characteristic <- characteristics[i]
    xnam <- paste(characteristic)
    fmla <- as.formula(paste("tf45 ~ ", paste(xnam, collapse= "+")))
    
    # for each univariate model, save the unadjusted OR and p value
    univariate_model <- glm(
        fmla,
        data = data,
        family = "binomial",
        weights = data$RAKEDW0
    );
    
    summary <- summary(univariate_model, ci_method="wald") # make sure estimate is OR? make sure i'm interpreting correctly
    x <- as.data.frame(exp(cbind("Odds_ratio" = coef(univariate_model), confint.default(univariate_model, level = 0.95))))
    
    x <- na.omit(x)
    # round odds ratio
    # convert p values to stars
    
    p.values <- summary(univariate_model)$coefficients[,4]
    
    # Convert p-values to factors
    p.values <- ifelse(p.values < 0.001, "***", 
                       ifelse(p.values < 0.01, "**", 
                              ifelse(p.values < 0.05, "*", "-")))
    x$p.values <- p.values
    
    datalist[[i]] = x
}

dataframe = do.call(rbind, datalist)
dataframe$Odds_ratio <- round(dataframe$Odds_ratio, 2)
dataframe$`2.5 %` <- round(dataframe$`2.5 %`, 2)
dataframe$`97.5 %` <- round(dataframe$`97.5 %`, 2)

xnam <- paste(covariates, sep="")
fmla <- as.formula(paste("tf45 ~ ", paste(xnam, collapse= "+")))

multivariate_model <- glm(
    fmla,
    data = data,
    family = "binomial",
    weights = data$RAKEDW0
); ## ED: doesn't account for weights

 summary(multivariate_model)

summary <- summary(multivariate_model, ci_method="wald") # make sure estimate is OR? make sure i'm interpreting correctly
x <- as.data.frame(exp(cbind("Odds_ratio" = coef(multivariate_model), confint.default(multivariate_model, level = 0.95))))
x <- round(x, 3)
x <- na.omit(x)

p.values <- round(summary(multivariate_model)$coefficients[,4], 3)
# Convert p-values to factors
p.values <- ifelse(p.values < 0.001, "***", 
                   ifelse(p.values < 0.01, "**", 
                          ifelse(p.values < 0.05, "*", "-")))
x$p.values <- p.values
View(x)
# x <- x[x$p.values <= 0.05,]
View(x)

# interaction_model <- glm(
#   tf45 ~ (age + ethnicity + sex + martial_status)^2,
#     data = data,
#     family = "binomial",
#     weights = data$RAKEDW0
# );
# 
# summary <- summary(interaction_model, ci_method="wald") # make sure estimate is OR? make sure i'm interpreting correctly
# x <- as.data.frame(exp(cbind("Odds_ratio" = coef(interaction_model), confint.default(interaction_model, level = 0.95))))
# x <- round(x, 3)
# x <- na.omit(x)
# p.values <- round(summary(interaction_model)$coefficients[,4], 3)
# x$p.values <- p.values
# # x <- x[x$p.values <= 0.05,]
# # View(x)
# 
# library(jtools) # Load jtools
# library(huxtable)
# export_summs(interaction_model, scale = TRUE)
# 
# # load package
# library(sjPlot)
# library(sjmisc)
# library(sjlabelled)

# tab_model(m1)
# 
# tab_model(multivariate_model)

# model_unadj <- glm(
#     climate_changeYES ~ social_impairment,
#     data = data,
#     family = "binomial",
#     weights = data$RAKEDW0
#     );
# 
# tbl_social <- tbl_regression(model_unadj)

# View(data)

# merge tables 
# tbl_merge_ex1 <-
#     tbl_merge(
#         tbls = list(tbl_social, tbl_multi),
#         tab_spanner = c("**Tumor Response**", "**Time to Death**")
#     )
# 
# tbl_merge_ex1

x <- as.data.frame(exp(cbind("Odds_ratio" = coef(model), confint.default(model, level = 0.95))))
x <- round(x, 3)
x <- na.omit(x)
p.values <- round(summary(model)$coefficients[,4], 3)
x$p.values <- p.values
x <- x[x$p.values <= 0.05,]

colnames(x)
# Make a table of the results
table1(~ Odds_ratio + p.values, data=x)






































# Calculate mean tf45 by year
# Create a table for tf45 by year
tf45_year_table <- table(chis_design$variables$tf45 == 1, chis_design$variables$year)
# Calculate proportion of "yes" (1)
tf45_year_prop <- prop.table(tf45_year_table, 2)[2, ]  # 2 for column-wise proportion
print(tf45_year_prop)

# Table for tf45 by year, age, and sex
tf45_age_sex_table <- table(chis_design$variables$tf45 == 1, 
                            chis_design$variables$year, 
                            chis_design$variables$srage_p, 
                            chis_design$variables$srsex)
# Proportion of "yes" by each demographic combination
tf45_age_sex_prop <- prop.table(tf45_age_sex_table, c(2, 3, 4))[2, , , ]  # Slicing for "yes" (1)
print(tf45_age_sex_prop)




# Table for tf45 by year, age, and sex
tf45_age_sex_table <- table(chis_design$variables$tf45 == 1, 
                            chis_design$variables$year, 
                            chis_design$variables$srage_p, 
                            chis_design$variables$srsex,
                            chis_design$variables$schtyp_p1
)

# Filter for "yes" responses (tf45 == 1) by year, age, sex, and school type
tf45_age_sex_schtyp_table <- prop.table(table(
    chis_design$variables$tf45 == 1,
    chis_design$variables$year,
    chis_design$variables$srage_p,
    chis_design$variables$srsex,
    chis_design$variables$schtyp_p1
), margin = c(2, 3, 4, 5))  # Margins allow proportions by each group

print(tf45_age_sex_schtyp_table)

# Convert table to data frame
tf45_trends_df <- as.data.frame(tf45_age_sex_schtyp_table)
colnames(tf45_trends_df) <- c("Response", "Year", "Age", "Sex", "SchoolType", "ProportionYes")

chis_design$variables$srage_p
# List of categorical variables (modify according to your dataset)
categorical_vars <- c("srsex", "racecnt_p1", "srage_p", "schtyp_p1", "tl53", "tl54", "tl27", "tl50")

# Initialize matrix to store p-values
chi_sq_results <- matrix(nrow = length(categorical_vars), ncol = 1, dimnames = list(categorical_vars, "p_value"))

# Perform chi-square tests
for (var in categorical_vars) {
    tbl <- table(chis_design$variables$tf45, chis_design$variables[[var]])
    chi_sq_results[var, "p_value"] <- chisq.test(tbl)$p.value
}

# Display chi-square matrix of p-values
chi_sq_results

# Get proportions within each variable level
# Get proportions within each variable level, preserving names
# Load data.table
# library(data.table)
# 
# for (var in categorical_vars) {
#     tbl <- table(tf45 = chis_design$variables$tf45, !!sym(var) := chis_design$variables[[var]])
#     prop_tbl <- prop.table(tbl, 2)  # Column-wise proportions to show directionality
#     print(as.data.frame(prop_tbl))
# }
