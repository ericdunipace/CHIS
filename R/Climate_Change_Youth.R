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
source("Code/Functions.R")

#### load data ####
chis <- readRDS("Data/chis_combined.Rds") %>% 
  mutate(s_depPCA = (depPCA - weighted.mean(depPCA, fnwgt0)) / matrixStats::weightedSd(depPCA, fnwgt0))
california_shapefile <- sf::st_read("Data/tl_2024_us_county/tl_2024_us_county.shp") %>% 
  filter(STATEFP == "06") %>%
  rename(county = NAME)

#### Outcome Variables ####
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

#### Outcome Variables ####
K6_vars        <- c("tg11",
                    "tg12",
                    "tg13",
                    "tg14",
                    "tg15",
                    "tg16")

# subset tf30 == "Yes" (roughly corresponds to Inapplicable)
worst_K6_vars  <- c("tf31",
                    "tf32",
                    "tf33",
                    "tf34",
                    "tf35",
                    "tf36")

suicide_vars   <- c("EVER THOUGHT TO COMMIT SUICIDE" = "tk1",
                    # for these other ones tk1 == "Yes", 
                    # can just screen by not "Inapplicable"
                    "EVER THOUGHT TO COMMIT SUICIDE PAST 12 M" = "tk2",
                    "THOUGHT TO COMMIT SUICIDE PAST 2 MOS"
                    = "tk3",
                    "EVER ATTEMPTED SUICIDE" = "tk4",
                    "ATTEMPTED SUICIDE PAST 12 MOS" = "tk5")

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
mean(complete.cases( chis_design$variables[, c("tf45", "year", "srage_p", "srsex")]))

#### TABLE 1 - DEMOGRAPHICS #########################################################################

# Define baseline demographic variables to be included in the table
baseline_demographics <- c(
  "srage_p",      # SELF-REPORTED AGE
  "srsex",        # SELF-REPORTED GENDER
  "ombsrtn_p1",   # OMB/CURRENT DOF RACE - ETHNICITY
  "racecn_p",      # RACE - CENSUS 2000 DEFINITION (PUF RECODE)
  "racecnt_p1",   # RACE - CENSUS 2000 DEFINITION (PUF 1 YR RECODE)
  "schtyp_p1",    # TYPE OF SCHOOL ATTENDED
  "ta4c_p1",      # ATTENDED SCHOOL DURING LAST SCHOOL YR
  "ahedtc_p1",    # ADULT EDUCATIONAL ATTAINMENT
  "povgwd_p1",    # FAMILY POVERTY THRESHOLD LEVEL
  "povll",        # POVERTY LEVEL
  "lnghmt_p1"     # LANGUAGE SPOKEN AT HOME
)

attr(baseline_demographics,"labels") <-list(
  tf45          = "Climate Anxiety", #CLIMATE CHANGE MAKES YOU NERVOUS/DEPRESSED/STRESSED
  "srage_p"     = "Age",     # SELF-REPORTED AGE
  "srsex"       = "Sex",        # SELF-REPORTED GENDER
  "ombsrtn_p1"  = "Ethnicity",   # OMB/CURRENT DOF RACE - ETHNICITY
  "racecn_p"     = "Race",  # RACE - CENSUS 2000 DEFINITION (PUF RECODE)
  "racecnt_p1"  = "Race, limited",   # RACE - CENSUS 2000 DEFINITION
  "schtyp_p1"   = "Type of School",    # TYPE OF SCHOOL ATTENDED
  "ta4c_p1"     = "Attended School in Last Year",      # ATTENDED SCHOOL DURING LAST SCHOOL YR
  "ahedtc_p1"   = "Adult Educational Attainment",    # ADULT EDUCATIONAL ATTAINMENT
  "povgwd_p1"   = "Family Poverty Threshold",    # FAMILY POVERTY THRESHOLD LEVEL
  "povll"       = "Poverty Level",        # POVERTY LEVEL
  "lnghmt_p1"   = "Language Spoken at Home"     # LANGUAGE SPOKEN AT HOME
)

intermed_demo <- c("srage_p", "racecn_p",  "srsex", "povll", "lnghmt_p1")
climateanx_tot <- svytotal(~tf45, design = chis_design, na.rm = TRUE)


demo_table <- tbl_custom_summary(
  data = chis_design$variables,
  by = "tf45",
  stat_fns = everything() ~ mean_svy_rep,
  label = attr(baseline_demographics, "labels")[intermed_demo],,
  statistic = list(all_continuous() ~ "{mean} ({sd})", all_categorical() ~
                     "{N} ({p}%) "),
  digits = NULL,
  missing = c("ifany"),
  missing_text = "Unknown",
  missing_stat = "{N_miss}",
  include = intermed_demo
) %>% 
  bold_labels() %>% 
  modify_header(
    stat_1 ~ "**Yes**, <br>N = {format(round(climateanx_tot[1], 0),big.mark = ',')}",
    stat_2 ~ "**No**, <br>N = {format(round(climateanx_tot[2], 0),big.mark = ',')}"
  ) %>% 
  modify_spanning_header(all_stat_cols() ~ "**Climate Anxiety**") %>% 
  modify_footnote_header(
    footnote = "All totals and percentages are based on values from sample weights. There are a total of {format(nrow(combined),big.mark = ',')} observations in the raw data. Percentages across all values of one variable sum to approximately 100% due to rounding",
    columns = all_stat_cols(),
    replace = FALSE
  ) 

# Display the table
print(demo_table)

demo_table %>% 
  as_gt() %>% 
  gt::gtsave(filename = "Outputs/demographics.docx")

#### ANALYSIS #1- SPATIAL HEATMAP #####################################################################################



# Aggregate and average anxiety scores by county
result_year <- chis_design$variables %>% 
  group_by(year, fips_cnt) %>% 
  summarize(ClimateAnxiety = weighted.mean(tf45 == "Yes",fnwgt0,na.rm = TRUE))

result <- chis_design$variables %>% 
  group_by(fips_cnt) %>% 
  summarize(ClimateAnxiety = weighted.mean(tf45 == "Yes",fnwgt0,na.rm = TRUE))

# Left join 
california_heatmap_year <- full_join(california_shapefile, result_year, by = c("NAME"= "fips_cnt"))
california_heatmap <- full_join(california_shapefile, result, by = c("NAME"= "fips_cnt"))

# check for all missing counties, not just mariposa
mariposa <- california_heatmap_year %>% filter(NAME == "Mariposa")
mariposa <- mariposa %>% bind_rows(mariposa, mariposa) %>% 
  mutate(year = 2021:2023)
california_heatmap_year <- california_heatmap_year %>% 
  filter(NAME != "Mariposa") %>% 
  bind_rows(mariposa) 

missing_counties <- california_heatmap_year %>% 
  group_by(NAME) %>%
  filter(length(year) < 3) %>% 
  dplyr::select(NAME,year)

for(i in unique(missing_counties$NAME)) {
  missing_years <- missing_counties %>% filter(NAME == i) %>% pull(year)
  for(j in 2021:2023) {
    if(!(j %in% missing_years)) {
      california_heatmap_year <- california_heatmap_year %>% 
        bind_rows(california_heatmap_year %>% filter(NAME == i) %>% 
                    mutate(year = j,
                           ClimateAnxiety = NA))
    }
  }
}



ggplot(california_heatmap) +
  geom_sf(aes(fill = ClimateAnxiety)) +
  geom_sf_text(aes(label = NAME), size = 3) +
  scale_fill_distiller(type = "seq", 
                       palette = "OrRd",
                       direction = 1) +
  # scale_fill_gradientn(colors = c("green", "yellow", "orange", "red")) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.title = element_blank(),         # Remove axis titles
    axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
    axis.ticks = element_blank()          # Remove axis ticks
  )

ggplot(california_heatmap) +
  geom_sf(aes(fill = ClimateAnxiety)) +
  scale_fill_distiller(type = "seq", 
                       palette = "OrRd",
                       direction = 1) +
  geom_sf_text(aes(label = NAME), size = 0) +
  # scale_fill_gradientn(colors = c("green", "yellow", "orange", "red")) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.title = element_blank(),         # Remove axis titles
    axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
    axis.ticks = element_blank()          # Remove axis ticks
  ) 

# Plot heatmap - labelled
ggplot(california_heatmap_year) +
  geom_sf(aes(fill = ClimateAnxiety)) +
  geom_sf_text(aes(label = NAME), size = 3) +
  scale_fill_distiller(type = "seq", 
                       palette = "OrRd",
                       direction = 1) +
  # scale_fill_gradientn(colors = c("green", "yellow", "orange", "red")) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.title = element_blank(),         # Remove axis titles
    axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
    axis.ticks = element_blank()          # Remove axis ticks
  ) + facet_grid(~year)

# Plot heatmap - unlabelled (there are too many labels so we can manually label select counties)
ggplot(california_heatmap_year) +
  geom_sf(aes(fill = ClimateAnxiety)) +
  scale_fill_distiller(type = "seq", 
                       palette = "OrRd",
                       direction = 1) +
  geom_sf_text(aes(label = NAME), size = 0) +
  # scale_fill_gradientn(colors = c("green", "yellow", "orange", "red")) +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.title = element_blank(),         # Remove axis titles
    axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
    axis.ticks = element_blank()          # Remove axis ticks
  ) + facet_grid(~year)

### ANALYSIS #1B - SPATIAL HEATMAP - LOS ANGELES ####################################################################
### STILL IN PROGRESS - NO NEED TO RUN THIS CODE

# Filter California shapefile for Los Angeles County (FIPS = 037)
la_shapefile <- california_shapefile[california_shapefile$COUNTYFP == '037',]

# Filter the dataset for Los Angeles County (fips_cnt should match COUNTYFP)
chis_data_la <- chis_design$variables %>% 
  filter(fips_cnt == "Los Angeles") %>% 
  group_by(fips_cnt) %>% 
  summarize(AverageAnxiety = weighted.mean(tf45 == "Yes", fnwgt0, na.rm = TRUE))

chis_data_la_year <- chis_design$variables %>% 
  filter(fips_cnt == "Los Angeles") %>% 
  group_by(fips_cnt, year) %>% 
  summarize(AverageAnxiety = weighted.mean(tf45 == "Yes", fnwgt0, na.rm = TRUE))


# Join the Los Angeles shapefile with the aggregated anxiety data by the sublevel data structure in LA***
la_heatmap <- left_join(la_shapefile, chis_data_la, by = c("NAME" = "fips_cnt"))

# Plot heatmap for Los Angeles County
ggplot(la_heatmap) +
  geom_sf(aes(fill = AverageAnxiety)) +
  geom_sf_text(aes(label = NAME), size = 5) +  # Adjust label size if necessary
  scale_fill_distiller(type = "seq", 
                       palette = "OrRd",
                       direction = 1) +
  # scale_fill_gradientn(colors = c("green", "yellow", "orange", "red")) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.title = element_blank(),         # Remove axis titles
    axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
    axis.ticks = element_blank()          # Remove axis ticks
  ) +
  labs(title = "Average Anxiety Scores in Los Angeles County", fill = "Avg Anxiety")

left_join(la_shapefile, chis_data_la_year, by = c("NAME" = "fips_cnt")) %>% 
  ggplot() +
  geom_sf(aes(fill = AverageAnxiety)) +
  geom_sf_text(aes(label = NAME), size = 5) +  # Adjust label size if necessary
  scale_fill_distiller(type = "seq", 
                       palette = "OrRd",
                       direction = 1) +
  # scale_fill_gradientn(colors = c("green", "yellow", "orange", "red")) +
  theme_minimal() +
  labs(title = "Average Anxiety Scores in Los Angeles County", fill = "Avg Anxiety") +
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.title = element_blank(),         # Remove axis titles
    axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
    axis.ticks = element_blank()          # Remove axis ticks
  ) + 
  facet_grid(~year)

#### ANALYSIS #2- CLIMATE CHANGE/MENTAL HEALTH #####################################################################################
# Goal: To assess if climate anxiety is associated with worsened mental health symptoms (eg, nervousness, distress, depressed)

# TQ1 is if lived with someone depressed or suicidal

# TF45 CLIMATE CHANGE MAKES YOU NERVOUS/DEPRESSED/STRESSED F-22

#K 6
# TG11 FEEL NERVOUS PAST 30 DAYS F-13
# TG12 FEEL HOPELESS PAST 30 DAYS F-14
# TG13 FELT RESTLESS PAST 30 DAYS F-14
# TG14 FEEL DEPRESSED PAST 30 DAYS F-15
# TG15 FEEL EVERYTHING AN EFFORT PAST 30 DAYS F-15
# TG16 FEEL WORTHLESS PAST 30 DAYS F-16

# DISTRESS should be K6 but looks wrong

# TF30 ANY MONTH PAST 12 MONTHS FELT WORSE F-19

# K6 for worst month
# TF31 FEEL NERVOUS WORST MONTH F-19
# TF32 FEEL HOPELESS WORST MONTH F-20
# TF33 FEEL RESTLESS OR FIDGETY WORST MONTH F-20
# TF34 FEEL DEPRESSED WORST MONTH F-21
# TF35 MONTH FEEL EVERYTHING IS AN EFFORT WORST MONTH F-21
# TF36 FEEL WORTHLESS WORST MONTH F-22

# DSTRSYR should be worst K6 but also looks wrong....

# suicide
# TK1 EVER THOUGHT TO COMMIT SUICIDE K-1
# TK2 EVER THOUGHT TO COMMIT SUICIDE PAST 12 M K-1
# TK3 THOUGHT TO COMMIT SUICIDE PAST 2 MOS K-2
# TK4 EVER ATTEMPTED SUICIDE K-2
# TK5 ATTEMPTED SUICIDE PAST 12 MOS K-3


# TODO 
# [ ] try K6
# [ ] try K6 worst
# [ ] try suicide
# [X] PCA (factor version) on all three together

#### PCA ####
pca_glm <- svyglm(formula = as.formula(paste0("s_depPCA ~", paste0("scale(K6_coding(",c(K6_vars,worst_K6_vars),"))", collapse = "+"),"+ tk1 + tk3 + tk5 + dstrsyr + dstrs12 + dstrs30 + distress" )), design = chis_design, family = "gaussian")

pca_glm %>% summary()

pca_outcome <- svyglm(formula = as.formula(paste0("s_depPCA ~ as.factor(year) + I(tf45=='Yes')+", paste0("as.factor(",intermed_demo,")", collapse = "+") ))
                      , design = chis_design, family = "gaussian")

pca_outcome %>% summary()

pca_outcome <- svyglm(formula = as.formula(s_depPCA ~  as.factor(year) + I(tf45 == "Yes") + as.factor(srage_p) +  I(racecn_p =="American Indian/Alaska Native") + I(srsex=="Female") + I(povll == "0-99% FPL") + I(!grepl("English", lnghmt_p1)))
                      , design = chis_design, family = "gaussian") 

pca_outcome %>% summary()

pca_outcome <- svyglm(formula = as.formula(s_depPCA ~  as.factor(year) + I(tf45 == "Yes") + as.factor(srage_p) +  I(racecn_p =="American Indian/Alaska Native") + I(srsex=="Female") + I(povll == "0-99% FPL") + I(!grepl("English", lnghmt_p1)) + tf11)
                      , design = chis_design, family = "gaussian") 

pca_outcome %>% summary()

#### Variable Selection ####
novary_cols <- grepl("ti7_11", colnames(chis))
missing_cols <- sapply(chis, function(x) x %>% is.na %>% any)
cols <- !(missing_cols | novary_cols |grepl("raked|fnwgt|region|p_tf|orthog_tf|geometry", colnames(chis)) | 
            grepl(paste(colnames(california_shapefile), collapse = "|"), colnames(chis)))

xform <- as.formula(~. -depPCA - s_depPCA - max_K6 - K6 - worst_K6 - K6_ge_13 -  worst_K6_ge_13- baseid - srcnty - tg11 - tg12 - tg13 - tg14 - tg15 - tg16 - tf31 - tf32 - tf33 - tf34 - tf35 - tf36
                    -dstrs30 - dstrsyr - dstrs12 - distress 
                    - dstrstn_p1
                    - tf30 # if using depPCA
                    - tk1 - tk2 - tk3 -tk4 - tk5 # suicide vars, if using dep PCA
                    - fips_cnt - te24a - te24 - bestzip - tsvrunit - tsvarstr - bmi_p - povgwd_p - povll- povll2_p -  povgwd_p1 - povll2_p - povll2_p1 -   povll2_p1v2 - srage - bmi_p - intv_mode2 - wghtk_p - wghtp_p - ta1yr - CNTY_ID - wghtp - survey_dates 
                    - acmdmt_p1 - tadate_mm # date and year of survey
                    # - ccpreg19 #covered cal price regions
                    - ta2 - ma7 - ma7_p
                    - ti2h_a - ti2h_b - ti2h_c - ti2h_d - ti2h_e - ti2h_f # all white vars
                    - asian10 - asian8 - asian9 - asnhp2_p - asnhp2 # all asian vars
                    - alcohol # collinear with te22 (ever had a few sips of alcohol)
                    - acmdnum #duplicate of TF16
                    - usual5tp
                    + I(as.numeric(ti3 == "United States")) - ti3
                    + I(as.factor(survey_months)) - survey_years - survey_months
                    + I(as.numeric(as.character(povgwd_p)))
                    + I(as.numeric(as.character(povll2_p)))
                    + I(as.numeric(as.character(povll2_p1v2)))
                    + I(as.numeric(as.character(povgwd_p)))
                    + I(as.numeric(tc38=='Yes')* te81) - tc38 - te81
                    # + I(as.numeric(tc38=='Yes')):te19 - te19 
                    + te19
                    # tc38 = EVER SMOKED CIGARETTES
                    # te19 = # OF DAYS SMOKED CIGARETTES IN PAST 30 DAYS
                    + I(as.numeric(te79=='Yes')* te82) - te79 - te82 - te82_p1
                    + I(as.numeric(te79=='Yes')* te80) - te80
                    + I(as.numeric(tf28=='Yes')* tf29v2) - tf29v2 - tf28
                    + I(as.numeric(te19 != 'Inapplicable' & te19 != 'None') * te20) - te20 - te19 
                    # te19 = # OF DAYS SMOKED CIGARETTES IN PAST 30 DAYS
                    # te20 = # OF CIGARETTES SMOKED PER DAY IN PAST 30 DAYS
                    + I(as.numeric(ti6 > -1) * ti6) - ti6
                    + as.factor(year) - year - tract10 
)
Xmf <- model.frame(xform, chis[,cols])
X <- model.matrix(terms(Xmf), Xmf)
# if (any(matrixStats::colSds(X) == 0)) {
#   stop("There are columns with zero variance in the model matrix.")
# }
# Y <- scale(chis$max_K6)
Y <- scale(chis$depPCA,
           scale = matrixStats::weightedSd(chis$depPCA,chis_design$pweights),
           center = weighted.mean(chis$depPCA, chis_design$pweights))

# lambs <- glmnet::glmnet(x = X, y = Y, weights = chis$fngwt0,
#                 alpha = 1, family = "gaussian", standardize = TRUE, intercept = TRUE, lambda.min.ratio = 0.001)$lambda

k <- 10
n <- nrow(chis)
p <- ncol(X)
w <- chis$fnwgt0
penalty <- "grp.lasso"
groups <- attributes(X)$assign 
group_list <- split(seq_along(groups), groups)
set.seed(1231243)
folds <- sample(rep(1:k, length.out = n))

# cont_vars <- which(apply(X,2,function(j) length(unique(j))>2))
# i_vars <- grepl("I(", names(cont_vars), fixed = TRUE)
# cont_vars <- cont_vars[!i_vars]
# 
# std_cont <- sapply(cont_vars, function(i)
#   scale(X[,i], center = weighted.mean(X[,i],w),
#         scale = matrixStats::weightedSd(X[,i],w))
# )
# X[,cont_vars] <- std_cont

all_vars <- colnames(X)
i_vars <- grepl("I(", (all_vars), fixed = TRUE)
all_vars <- all_vars[!i_vars][-1]

std_cont <- sapply(all_vars, function(i)
  scale(X[,i], center = weighted.mean(X[,i],w),
        scale = matrixStats::weightedSd(X[,i],w))
)
X[,all_vars] <- std_cont


for(i in which(grepl("I(", colnames(X), fixed = TRUE))) {
  if (length(unique(X[,i])) > 2) {
    temp <-  X[,i]
    tnonzero <- temp != 0
    temp_nonzero <- temp[tnonzero]
    X[tnonzero,i] <- (temp_nonzero - weighted.mean(temp_nonzero, w[tnonzero]))/matrixStats::weightedSd(X[,i], w)
  } else if (length(unique(X[,i])) %in% 2) {
    X[,i] <- (X[,i] - weighted.mean(X[,i], w)) / matrixStats::weightedSd(X[,i], w)
  }
}

# matrixStats::colWeightedSds(X[,all_vars],w) 
# matrixStats::colWeightedMeans(X[,all_vars],w)

X_s <- Matrix::Matrix(X, sparse = TRUE)

xtx <- crossprod(X_s, X_s * w/sum(w) )
xty <- crossprod(X_s, Y * w/sum(w) )

time1 <- proc.time()[3]
full <- oem::oem.xtx(as.matrix(xtx), as.matrix(xty), family = "gaussian",
                     penalty = penalty,
                     lambda.min.ratio = 0.001,
                     nlambda = 100,
                     tau = 0.5, alpha = 0.5,
                     groups = groups,
                     group.weights = c(0, rep(1, length(unique(groups)) - 1))
)
print(proc.time()[3] - time1)

lambdas <- full$lambda[[1]]

# 4. Cross-validation function
cv_loss <- function(lambda) {
  library(CVXR)
  library(Matrix)
  fold_mse <- numeric(k)
  
  for (i in 1:k) {
    test_idx <- which(folds == i)
    train_idx <- setdiff(1:n, test_idx)
    
    X_train <- X[train_idx, , drop = FALSE]
    y_train <- Y[train_idx]
    X_test  <- X[test_idx, , drop = FALSE]
    y_test  <- Y[test_idx]
    
    w_test  <- w[test_idx]
    w_train <- w[train_idx]
    
    # xtx <- crossprod(X_train, X_train * n*w_train)/sum(w_train)
    # xty <- crossprod(X_train, y_train * n * w_train)/sum(w_train)
    
    # Fit lasso using CVXR
    beta <- Variable(p)
    loss <- sum_entries( w_train*(X_train %*% beta - y_train)^2) *0.5/n
    group_penalty <- Reduce("+", sapply(group_list, function(g) norm2(beta[g])))
    problem <- Problem(Minimize(loss + lambda * group_penalty))
    result <- solve(problem)
    
    beta_hat <- result$getValue(beta)
    preds <- X_test %*% beta_hat
    fold_mse[i] <- mean(w_test * (y_test - preds)^2)
  }
  
  c(mean(fold_mse), var(fold_mse))
}


cv_loss_oem <- function(i, X, Y, groups, folds) {
  require(oem)
  
  test_idx <- which(folds == i)
  train_idx <- setdiff(1:n, test_idx)
  
  X_train <- X[train_idx, , drop = FALSE]
  y_train <- Y[train_idx]
  X_test  <- X[test_idx , , drop = FALSE]
  y_test  <- Y[test_idx]
  
  w_test  <- w[test_idx]
  w_train <- w[train_idx]
  xtx <- Matrix::crossprod(X_train, X_train * w_train/sum(w_train))
  xty <- Matrix::crossprod(X_train, y_train * w_train/sum(w_train))
  
  result <- oem::oem.xtx(xtx = as.matrix(xtx), 
                         xty = as.matrix(xty), 
                         family = "gaussian",
                         penalty = penalty,
                         lambda = lambdas,
                         alpha = 0.5,
                         tau = 0.5,
                         groups = groups,
                         group.weights = c(0, rep(1, length(unique(groups)) - 1))
  )
  betas  <- result$beta[[1]]
  preds  <- as.matrix( X_test %*% betas )
  fold_mse <- matrixStats::colWeightedMeans( (y_test - preds)^2, w = w_test)
  
  fold_mse
}

cv.grplasso <- function(x, y, index, weights = rep(1, length(y)),
                        model = LinReg(), nfolds=5, foldid=NULL, nlambda=20,parallel = FALSE, standardize) {
  if(is.null(foldid)) {
    foldid = sample(rep(seq(nfolds), length = length(y)))
    
  }
  lambda.max <- lambdamax(x, y, index = index, 
                          weights = weights,
                          model = model,
                          standardize = standardize)
  lambda.min = 0.0001 * lambda.max 
  
  lambda=exp(seq(log(lambda.max), log(lambda.min), length.out = nlambda))
  
  ## Fit the solution path on the lambda grid
  if (parallel) {
    library(doParallel)
    cl = parallel::makeCluster(nfolds, outfile="")
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
    
    outlist = foreach(i = seq(nfolds),.combine = list,.multicombine = TRUE, .packages = c("grplasso")) %dopar% 
      {
        print(paste(i,"th fold",sep=""))
        which <- foldid == i
        x_sub <- x[!which, , drop = FALSE]
        y_sub <- y[!which]
        w_sub <- weights[!which]
        
        fit <- grplasso(x = x_sub, 
                        y = y_sub, index = index, 
                        w = w_sub/sum(w_sub),
                        lambda = lambda, model = model,
                        standardize = standardize,
                        control = grpl.control(trace = 0L))
        pred <- predict(fit, x[which,,drop=FALSE])
        w_test <- weights[which]
        w_test <- w_test/sum(w_test)
        y_test <- y[which]
        sapply(1:length(lambda), function(j) 
          model@nloglik(y_test, pred[,j], w_test))
      }
    # doParallel::stopImplicitCluster()
  } else {   
    outlist <- vector("list", nfolds)
    for (i in seq(nfolds)) {
      print(paste(i,"th fold",sep=""))
      which <- foldid == i
      x_sub <- x[!which, , drop = FALSE]
      y_sub <- y[!which]
      w_sub <- weights[!which]
      
      fit <- grplasso(x = x_sub, 
                      y = y_sub, index = index, 
                      weights = w_sub,
                      lambda = lambda, model = model,
                      standardize = standardize,
                      control = grpl.control(trace = 0L))
      pred <- predict(fit, x[which,,drop=FALSE])
      w_test <- weights[which]
      w_test <- w_test/sum(w_test)
      y_test <- y[which]
      outlist[[i]] <- sapply(1:length(lambda), function(j) 
        model@nloglik(y_test, pred[,j], w_test))
    }  
  }
  
  mse.mat <- do.call(rbind, outlist)
  mse <- data.frame(cvm = colMeans(mse.mat),
                    cvse = matrixStats::colSds(mse.mat) / sqrt(nfolds),
                    lambda = lambda)
  
  id<-which.min(mse$cvm)
  lambda.min <- mse$lambda[id]
  lambda.1se <- max(mse$lambda[mse$cvm <= mse$cvm[id] + mse$cvse[id]])
  
  test <- grplasso(x =x, y=y, index=index, 
                   weights = weights, standardize = standardize,
                   lambda=c(lambda.1se,lambda.min), model=model)
  list(lambda=lambda, cvm=mse$cvm, cvse=mse$cvse, grplasso.fit=test, lambda.min=lambda.min,lambda.1se=lambda.1se,foldid=foldid)  
}

# 5. Run CV for all lambdas
cv_results <- tibble(
  i = rep(1:k, each = length(lambdas)),
  lambda = rep(lambdas, k),
  cv_mse = map(1:k, cv_loss_oem, X = X_s, Y = Y, groups = groups, folds = folds, .progress = TRUE) %>% unlist()
)

cv_sum <- cv_results %>% group_by(lambda) %>% 
  summarize(mse = mean(cv_mse),
            var = var(cv_mse)/k)

mins <- cv_sum %>% summarize(
  min = min(mse),
  lambda.min = lambda[which.min(mse)],
  min.1se = min(mse[mse>=(min + sqrt(var[which.min(mse)])) & lambda >= lambda.min]),
  lambda.1se = lambda[which(mse == min.1se & lambda >= lambda.min)],
)

plot(x = cv_sum$lambda %>% log(), y = cv_sum$mse, pch=19, ylab="MSE", xlab="log lambda", col = "red")
arrows(x0 = cv_sum$lambda %>% log(), y0 = cv_sum$mse - 1* sqrt(cv_sum$var), 
       x1 = cv_sum$lambda %>% log(), y1 = cv_sum$mse + 1* sqrt(cv_sum$var),
       angle = 90, code = 3, length = 0.05)
abline(v = mins$lambda.min %>% log, lty = 3)
abline(v = mins$lambda.1se %>% log, lty = 3)

group_beta <- full$beta[[1]][,which(lambdas == mins$lambda.1se)]
nonzero_beta_group <- as.numeric(group_beta) != 0
group_beta[nonzero_beta_group]

1 - weighted.mean(as.numeric((Y - (X %*% group_beta))^2), chis$fnwgt0/sum(chis$fnwgt0))/1.0 #(divide by 1 b/c standardized)

group_beta_min <- full$beta[[1]][,which(lambdas == mins$lambda.min)]
nonzero_beta_min <- as.numeric(group_beta_min) != 0
group_beta_min[nonzero_beta_min]

nms <- names(group_beta[nonzero_beta_group])

whichcols <- attributes(X)$assign[nonzero_beta_group]
sapply(unique(attr(terms(Xmf), "term.labels")[whichcols]), function(i) attr(chis[[i]], "label"))

set.seed(103248108)
lasso.cv <- glmnet::cv.glmnet(x = X[,-1], y = Y, 
                              # alpha = 0.5,
                              weights = chis$fnwgt0,
                              family = "gaussian",
                              lambda.min.ratio = 0.001,
                              intercept = TRUE,
                              standardize = TRUE
)
print(lasso.cv)
plot(lasso.cv)

beta_lasso <- glmnet::coef.glmnet(lasso.cv, s = lasso.cv$lambda.1se)
nonzero_beta <- as.numeric(beta_lasso) != 0

plot( y = Y, x = X %*% beta_lasso)
1 - weighted.mean(as.numeric((Y - (X %*% beta_lasso))^2), chis$fnwgt0/sum(chis$fnwgt0))/1.0 #(divide by 1 b/c standardized)

rownames(beta_lasso)[nonzero_beta]
beta_lasso[nonzero_beta,]

nms <- rownames(beta_lasso)[nonzero_beta]

whichcols <- attributes(X)$assign[sapply(nms, function(n) grep(n, colnames(X), fixed = TRUE))]
sapply(attr(terms(Xmf), "term.labels")[whichcols], function(i) attr(chis[[i]], "label"))

set.seed(103248108)
tf45col <- grep("tf45No", colnames(X))
Y_tf45 <- 1-X[,tf45col]
lasso.cv.tf <- glmnet::cv.glmnet(x = X[,-c(1,tf45col)], y = Y_tf45, 
                                 weights = chis$fnwgt0/sum(chis$fnwgt0),
                                 family = "binomial",
                                 lambda.min.ratio = 0.001,
                                 intercept = TRUE,
                                 standardize = TRUE
)
print(lasso.cv.tf)
plot(lasso.cv.tf)

s_Y_tf45 <- scale(Y_tf45, center = weighted.mean(Y_tf45,w),
                  scale = matrixStats::weightedSd(Y_tf45, w))
xty_tf45 <- Matrix::crossprod(X_s[,-c(tf45col)], s_Y_tf45 * w)
xts_tf45 <- Matrix::crossprod(X_s[,-c(tf45col)], X_s[,-c(tf45col)] * w)
groups.gglasso <- as.integer(factor(groups[-c(tf45col)]))

full.tf <- oem::oem.xtx(as.matrix(xts_tf45), as.matrix(xty_tf45), family = "gaussian",
                        penalty = penalty,
                        lambda.min.ratio = 0.001,
                        nlambda = 100,
                        tau = 0.5, alpha = 0.5,
                        groups = groups.gglasso
)
set.seed(234232)
folds.tf <- sample(rep(1:k, length.out = nrow(X_s)))
cv_results.tf <- tibble(
  i = rep(1:k, each = length(full.tf$lambda[[1]])),
  lambda = rep(full.tf$lambda[[1]], k),
  cv_mse = map(1:k, cv_loss_oem, X = X_s[,-c(tf45col)], Y = s_Y_tf45, groups = groups.gglasso, folds.tf, .progress = TRUE) %>% unlist()
)

cv_sum.tf <- cv_results.tf %>% group_by(lambda) %>% 
  summarize(mse = mean(cv_mse),
            var = var(cv_mse)/k)

mins.tf <- cv_sum.tf %>% summarize(
  min = min(mse),
  lambda.min = lambda[which.min(mse)],
  min.1se = min(mse[mse>=(min + sqrt(var[which.min(mse)])) & lambda >= lambda.min]),
  lambda.1se = lambda[which(mse == min.1se & lambda >= lambda.min)],
)

plot(x = cv_sum.tf$lambda %>% log(), y = cv_sum.tf$mse, pch=19, ylab="MSE", xlab="log lambda", col = "red")
arrows(x0 = cv_sum.tf$lambda %>% log(), y0 = cv_sum.tf$mse - 1* sqrt(cv_sum.tf$var), 
       x1 = cv_sum.tf$lambda %>% log(), y1 = cv_sum.tf$mse + 1* sqrt(cv_sum.tf$var),
       angle = 90, code = 3, length = 0.05)
abline(v = mins.tf$lambda.min %>% log, lty = 3)
abline(v = mins.tf$lambda.1se %>% log, lty = 3)

group_beta.tf <- full.tf$beta[[1]][,which(full.tf$lambda[[1]] == mins.tf$lambda.1se)]
nonzero_group_beta.tf <- as.numeric(group_beta.tf) != 0
group_beta.tf[nonzero_group_beta.tf]

beta_lasso.tf <- glmnet::coef.glmnet(lasso.cv.tf, s = lasso.cv.tf$lambda.1se)
nonzero_beta.tf <- as.numeric(beta_lasso.tf) != 0
rownames(beta_lasso.tf)[nonzero_beta.tf]
beta_lasso.tf[nonzero_beta.tf,]

nms.tf <- rownames(beta_lasso.tf)[nonzero_beta.tf]
whichcols.tf <- attributes(X)$assign[sapply(nms.tf, function(n) grep(n, colnames(X), fixed = TRUE))]
sapply(attr(terms(Xmf), "term.labels")[whichcols.tf], function(i) attr(chis[[i]], "label"))

# expand_idx <- mapply(function(w,i) {rep(i,w)}, w = w*(1/min(w)), i = 1:nrow(X_s)) %>% unlist()

library(grplasso)
groups.grplasso <- groups.gglasso
groups.grplasso[1] <- NA
set.seed(123123)
grp_tf <- cv.grplasso(x = X[,-c(tf45col)], y = as.numeric(Xmf$tf45 == "Yes"),
                      index = groups.grplasso, weights = w/sum(w),
                      model = LogReg(), standardize = FALSE,
                      nfolds = 10, nlambda = 100, parallel = TRUE)

plot(x = grp_tf$lambda %>% log(), y = grp_tf$cvm, pch=19, ylab="MSE", xlab="log lambda", col = "red")
arrows(x0 = grp_tf$lambda %>% log(), y0 = grp_tf$cvm - 1* (grp_tf$cvse), 
       x1 = grp_tf$lambda %>% log(), y1 = grp_tf$cvm + 1* (grp_tf$cvse),
       angle = 90, code = 3, length = 0.05)
abline(v = grp_tf$lambda.min %>% log, lty = 3)
abline(v = grp_tf$lambda.1se %>% log, lty = 3)

gl_beta_1se <- grp_tf$grplasso.fit$coefficients[,1]
nonzero_gl_beta_1se <- as.numeric(gl_beta_1se) != 0
gl_beta_1se[nonzero_gl_beta_1se]

gl_beta_min <- grp_tf$grplasso.fit$coefficients[,2]
nonzero_gl_beta_min <- as.numeric(gl_beta_min) != 0
gl_beta_min[nonzero_gl_beta_min]

expand_idx <- mapply(function(w,i) {rep(i,w)}, w = ceiling(w), i = 1:nrow(X_s)) %>% unlist()

X_expand <- X[expand_idx,-1]
Y_tf45_expand <- Y_tf45[expand_idx]

grp_tf <- gglasso::cv.gglasso(x = X_expand, 
                              y = (2L * Y_tf45_expand - 1L),
                              group = groups.gglasso,
                              pred.loss = "loss", 
                              loss = "logit",
                              nlambda = 100, 
                              lambda.factor = 1e-8,
                              nfolds = 10,
                              intercept = TRUE)

plot(grp_tf)

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
