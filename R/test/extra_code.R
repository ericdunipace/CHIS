#### K6 outcome ####
confounders <- c("scale(srage)", "I(racecn_p=='African American')", 
                 "I(srsex=='Female')",
                 "I(povll == '0-99% FPL')", 
                 "I(!grepl('English', lnghmt_p1))"
                 , "as.factor(year)"
                 # , "scale(delta_last_temp)", 
                 # "scale(delta_cur_temp)"
)
vars_selected <- NULL# c("-te80-te79-te71-tc38",
# # "ti11",
# "te81:I(tc38 == 'Yes')",
# "te80:I(te79 == 'Yes')",
# # "tf30",
# "I(tf28=='Yes'):tf29v2")

independent_vars <- paste0(c(vars_selected, confounders), collapse = "+")
k6m <- svymean(~sqrt(max_K6), chis_design) %>% as.numeric()
k6s <- svyvar(~sqrt(max_K6), chis_design) %>% as.numeric() %>% sqrt()

k6m <- svymean(~depPCA, chis_design) %>% as.numeric()
k6s <- svyvar(~depPCA, chis_design) %>% as.numeric() %>% sqrt()

form <- as.formula(glue(paste0("scale(sqrt(max_K6), center = {k6m},
                          scale = {k6s}) ~ I(tf45=='Yes')", independent_vars)))
# form <- as.formula(glue(paste0("K6_coding(tg14) ~ I(tf45=='Yes')", independent_vars)))

fit_k6 <- svyglm(formula = form
                 , design = chis_design, family = "gaussian",
                 rescale = TRUE)
fit_k6 %>% summary()

fit_k6 <- svyglm(formula = form
                 , design = chis_design, family = "gaussian")
fit_k6 %>% summary()

fit_k6_c <- svyglm(formula = as.formula(paste0("K6_ge_13 ~ as.factor(year) + I(tf45=='Yes')+", paste0("as.factor(",intermed_demo,")", collapse = "+") ))
                   , design = chis_design, family = "binomial")
fit_k6_c %>% summary()

fit_wk6 <- svyglm(formula = as.formula(paste0("scale(worst_K6) ~ as.factor(year) + I(tf45=='Yes')+", paste0("as.factor(",intermed_demo,")", collapse = "+") ))
                  , design = chis_design, family = "gaussian")
fit_wk6 %>% summary()

fit_wk6_c <- svyglm(formula = as.formula(paste0("worst_K6_ge_13 ~ as.factor(year) + I(tf45=='Yes')+", paste0("as.factor(",intermed_demo,")", collapse = "+") ))
                    , design = chis_design, family = "binomial")
fit_wk6_c %>% summary()

test <- lmer.svyrep.design(formula = as.formula(scale(K6) ~  I(tf45 == "Yes") + scale(year) + as.numeric(srage_p) +  I(racecn_p =="American Indian/Alaska Native") + I(srsex=="Female") + I(povll == "0-99% FPL") + I(!grepl("English", lnghmt_p1))+ (1 |fips_cnt)),
                           design = chis_design)

svyglm(formula = as.formula(scale(K6) ~ I(tf45 == "Yes") +as.factor(year) + scale(as.numeric(as.character(srage_p))) + I(scale(as.numeric(as.character(srage_p)))^2) +  I(racecn_p =="White") + I(srsex=="Female") + I(povll == "0-99% FPL") + I(lnghmt_p1 == "English") ),
       design = chis_design, family = "gaussian") %>% summary()

svyglm(formula = as.formula(I(as.integer(tf45 == "Yes")) ~ worst_K6_ge_13 + as.factor(year) + scale(as.numeric(as.character(srage_p))) + I(scale(as.numeric(as.character(srage_p)))^2) +  I(racecn_p =="White") + I(srsex=="Female") + I(povll == "0-99% FPL") + I(lnghmt_p1 == "English")), design = chis_design, family = "binomial", rescale = FALSE) %>% summary()

lmer.form <- as.formula(glue(paste0("scale(max_K6,center = {k6m},
                          scale = {k6s}) ~ I(tf45=='Yes') + (1|tract10)", independent_vars)))
lmer.svyrep.design(formula = lmer.form,
                   design = chis_design) %>% summary()

glmer(as.formula(I(as.integer(tf45=='Yes')) ~ as.factor(year) + scale(as.numeric(as.character(srage_p))) + I(scale(as.numeric(as.character(srage_p)))^2) +  I(racecn_p =="White") + I(srsex=="Female") + I(povll == "0-99% FPL") + I(lnghmt_p1 == "English") + (1|fips_cnt)), data = chis_design$variables, family = "binomial") %>% summary()

glm(formula = as.formula(scale(K6, scale = FALSE, center = 6.8) ~  as.factor(year) + I(tf45 == "Yes") + as.factor(srage_p) +  I(racecn_p =="American Indian/Alaska Native") + I(srsex=="Female") + I(povll == "0-99% FPL") + I(!grepl("English", lnghmt_p1))  +tc38 +  td34 +  td36 +  te24a +te64 + te80 + te81 + tf11 + tf2 + tf30 + tf9 + tg22 
                         # + th57 #collinear
                         + ti11)
    , data = chis_design$variables, weights = chis_design$pweights, family = "gaussian") 

svyglm(formula = as.formula(scale(K6, scale = FALSE, center = 6.8) ~  as.factor(year) + I(tf45 == "Yes") + srage +  I(racecn_p =="American Indian/Alaska Native") + I(srsex=="Female") + I(povll == "0-99% FPL")
                            # + tc38 +  td34 +  td36
                            +  as.factor(alcohol) + tf11
                            # + te64 
                            # + te80 + te81 + tf11 + tf30 + tf9 +
                            # + th57 #collinear
                            # + ti11
)
, design = chis_design, family = "gaussian") %>% summary()

svyglm(formula = as.formula(s_depPCA ~  
                              as.factor(year) + 
                              + srage +  
                              I(racecn_p =="American Indian/Alaska Native") 
                            + I(srsex=="Female") 
                            + I(povll == "0-99% FPL")
                            + acmdnum
                            + ccpreg19 
                            + td45
                            +te22
                            +te64
                            +te69
                            +te83
                            +tf11
                            +tf2
                            + I(as.numeric(tf45 == "Yes"))
                            + tf9
                            + ti11
                            + te68_3
)
, design = chis_design, family = "gaussian") %>% summary()

lmer.svyrep.design(formula = s_depPCA ~ acmdnum
                   + td45
                   +te22
                   +te64
                   +te69
                   +te83
                   +tf11
                   +tf2
                   + I(as.numeric(tf45 == "Yes"))
                   + tf9
                   + ti11
                   + te68_3 + (1|ccpreg19),
                   design = chis_design) %>% summary()


glmer.svyrep.design(formula = as.formula(I(tf45 == "Yes")  ~ as.factor(year) + as.factor(srage_p) +  I(racecn_p =="American Indian/Alaska Native") + I(srsex=="Female") + I(povll == "0-99% FPL") + I(!grepl("English", lnghmt_p1))
                                         +  as.factor(alcohol) + te64 
                                         + ti11 + tf30
                                         + (1 |tract10)),
                    design = chis_design, family = "binomial") %>% summary()


#### Two stage least squares ####
zfit <- svyglm(formula = as.formula(I(as.integer(tf45 == "Yes")) ~ as.factor(year) + as.factor(srage_p) +  I(racecn_p =="American Indian/Alaska Native") + I(srsex=="Female") + I(povll == "0-99% FPL") + I(!grepl("English", lnghmt_p1))
                                    +  as.factor(alcohol) + te64 
                                    + ti11 + tf30), design = chis_design, family = "binomial")

zfit %>% summary()

chis_design$variables$orthog_tf <- residuals(zfit, type = "response")
chis_design$variables$p_tf <- zfit$fitted.values

svyglm(formula = as.formula(scale(K6) ~ I(as.integer(tf45 == "Yes")) + scale(orthog_tf)), design = chis_design, family = "gaussian") %>% summary()

svyglm(formula = as.formula(scale(K6) ~ scale(p_tf)), design = chis_design, family = "gaussian") %>% summary()

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
    weights = data$rakedw0
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
multi.confidence.intervals <- matrix(NA, length(characteristics),2);
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
    weights = data$rakedw0
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
  multi.confidence.intervals[i,] <- CI
  multi.odds.ratios[i] <- OR
  multi.significances[i] <- sig
}

# Create dataframe with bootstrap results
model.results <- data.frame(
  characteristics,
  multi.p.values,
  ci.lower = multi.confidence.intervals[,1],
  ci.upper = multi.confidence.intervals[,2],
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
    weights = data$rakedw0
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
    weights = data$rakedw0
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
  climate_distress_trend$Climate_Distress_Count[i] <- sum(data_year$tf45 == 1, na.rm = TRUE)
  
  # Calculate percentage of respondents with climate distress
  climate_distress_trend$Climate_Distress_Percentage[i] <- 
    (climate_distress_trend$Climate_Distress_Count[i] / climate_distress_trend$Total_Respondents[i]) * 100
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
  model <- glm(tf45 ~ factor(year) * factor(data[[var]]), data = data, family = binomial)
  
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
    weights = data$rakedw0
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
  weights = data$rakedw0
);

summary(multivariate_model)
tbl_multi <- tbl_regression(
  multivariate_model,
  exponentiate = TRUE,
  label = list(ethnicity ~ "Race/Ethnicity", sex ~ "Sex", age ~ "Age", eng_prof ~ "English Proficiency", educ ~ "Education", poverty_level ~ "Poverty Level", social_impairment ~ "Impairment of Social Life", urbanicity = "Urbanicity", martial_status = "Marital Status", trust_neighborhood = "Trust Neighborhood", property_damage = "Property Damage")
)

summary <- summary(multivariate_model, ci_method="wald") # make sure estimate is OR? make sure i'm interpreting correctly
x <- as.data.frame(exp(cbind("Odds_ratio" = coef(multivariate_model), confint.default(multivariate_model, level = 0.95))))
x <- round(x %>% as.matrix(), 3)
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

interaction_model <- glm(
  climate_changeYES ~ (age + ethnicity + sex + martial_status)^2,
  data = data,
  family = "binomial",
  weights = data$rakedw0
);

summary <- summary(interaction_model, ci_method="wald") # make sure estimate is OR? make sure i'm interpreting correctly
x <- as.data.frame(exp(cbind("Odds_ratio" = coef(interaction_model), confint.default(interaction_model, level = 0.95))))
x <- round(x, 3)
x <- na.omit(x)
p.values <- round(summary(interaction_model)$coefficients[,4], 3)
x$p.values <- p.values
# x <- x[x$p.values <= 0.05,]
View(x)

install.packages('jtools')
install.packages('huxtable')
library(jtools) # Load jtools
library(huxtable)
export_summs(interaction_model, scale = TRUE)

# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(m1)

tbl_int <- tbl_regression(
  interaction_model,
  exponentiate = TRUE,
  label = list(ethnicity ~ "Race/Ethnicity", sex ~ "Sex", age ~ "Age", eng_prof ~ "English Proficiency", educ ~ "Education", poverty_level ~ "Poverty Level", social_impairment ~ "Impairment of Social Life", urbanicity = "Urbanicity", martial_status = "Marital Status", property_damage = "Property Damage")
)

tbl_int

# tab_model(multivariate_model)

# model_unadj <- glm(
#     climate_changeYES ~ social_impairment,
#     data = data,
#     family = "binomial",
#     weights = data$rakedw0
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
library(data.table)

for (var in categorical_vars) {
  tbl <- table(tf45 = chis_design$variables$tf45, !!sym(var) := chis_design$variables[[var]])
  prop_tbl <- prop.table(tbl, 2)  # Column-wise proportions to show directionality
  print(as.data.frame(prop_tbl))
}


library(ggplot2)
ggplot(tf45_age_sex_df, aes(x = Age, y = Proportion_Yes, color = as.factor(Sex), group = interaction(Year, Sex))) +
  geom_line() +
  facet_wrap(~ Year) +
  scale_color_manual(values = c("1" = "blue", "2" = "pink"), labels = c("1" = "Male", "2" = "Female")) +
  labs(title = "Proportion of 'Yes' for tf45 by Age, Sex, and Year",
       color = "Sex") +
  theme_minimal()


# Import these other files
chis_2023_f <- haven::read_dta('/Users/danielzhao/Desktop/Psychiatry/Climate Change Youth/teen_2023_stata/TEENF.dta')

# How does climate change anxiety change over time?

# Do the predictive variables change?

# Take ae 