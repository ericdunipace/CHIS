### Disparities research in barriers to mental health #############################################

### Import and read stuff in ######################################################################
# Load required libraries (ensure they are installed)
library(dplyr)       # Data manipulation
library(survey)      # Survey data analysis
library(ggplot2)     # Data visualization
library(sf)          # Spatial data handling and mapping
library(prism)       # Prism data visualization, for temperatures
library(purrr)       # Functional programming
library(gtsummary)   # for summary tables
library(lme4)        # Linear mixed-effects models
library(forcats)     # Factor manipulation
library(rstudioapi)  # For setting working directory to script location
library(glue)
library(lubridate)
library(stringr)
library(terra)
library(spdep)
library(spatialreg)
library(grid)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))  # Sets to script location
setwd("..")  # Moves up to the project root
getwd()

#### load Functions ####
source("R/Functions.R")

#### load data ####
chis <- readRDS("Data/chis_combined.Rds") 

california_shapefile <- sf::st_read("Data/tl_2024_us_county/tl_2024_us_county.shp") %>% 
  filter(STATEFP == "06") %>%
  rename(county = NAME)

census_shapefile <- readRDS("Data/ca_tract_2010.rds")

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
    footnote = "All totals and percentages are based on values from sample weights. There are a total of {format(nrow(chis),big.mark = ',')} observations in the raw data. Percentages across all values of one variable sum to approximately 100% due to rounding",
    columns = all_stat_cols(),
    replace = FALSE
  ) 

# Display the table
# print(demo_table)

demo_table %>% 
  as_gt() %>% 
  gt::gtsave(filename = "Outputs/demographics.docx")

demo_table %>% 
  as_gt() %>% 
  gt::gtsave(filename = "Outputs/demographics.tex")

#### ANALYSIS #1- SPATIAL HEATMAP #####################################################################################

# Goal: To create a spatial heatmap of climate anxiety across California counties


# Aggregate and average anxiety scores by county
result_year <- chis %>% 
  group_by(year, county) %>% 
  summarize(ClimateAnxiety = weighted.mean(tf45 == "Yes",fnwgt0,na.rm = TRUE))

result <- chis %>% 
  group_by(county) %>% 
  summarize(ClimateAnxiety = weighted.mean(tf45 == "Yes",fnwgt0,na.rm = TRUE))

# get prism vars for map
census_temp <- prism_to_map("tmax", census_shapefile, 2021:2023, "census", cutoff = 32)
census_heatwave <- prism_to_map("tmax", census_shapefile, 2021:2023, "census", heatwave = TRUE, cutoff = 32)

# Left join 
california_heatmap_year <- full_join(california_shapefile, result_year, by = "county")
california_heatmap <- full_join(california_shapefile, result, by = "county")

county_centers_year <- st_centroid(california_heatmap_year)
county_centers <- st_centroid(california_heatmap)

# check for all missing counties, not just mariposa
mariposa <- california_heatmap_year %>% filter(county == "Mariposa")
mariposa <- mariposa %>% bind_rows(mariposa, mariposa) 
california_heatmap_year <- california_heatmap_year %>% 
  filter(county != "Mariposa") %>% 
  bind_rows(mariposa) 

census_temp_mean <- census_temp %>% 
  group_by(tract10) %>% 
  summarize(tmax = mean(tmax))

census_heatwave_mean <- census_heatwave %>% 
  group_by(tract10) %>% 
  summarize(days_above32 = mean(days_above32))

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


county_plot_heatwave <- ggplot(california_heatmap) +
  geom_sf(data = census_heatwave_mean %>% 
            mutate(`heatwave days` = days_above32)
            , aes(fill = `heatwave days`), color = NA) +
  geom_sf(fill = NA, color = "black") +
  geom_sf(data = county_centers %>% 
            mutate(`Climate Anxiety` = ClimateAnxiety), 
          aes(size = `Climate Anxiety`), color = "gray30") +
  scale_fill_distiller(palette = "Spectral")+
  theme_minimal() +
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.title = element_blank(),         # Remove axis titles
    axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
    axis.ticks = element_blank()          # Remove axis ticks
  )

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

county_plot_tmax <- ggplot(california_heatmap) +
  geom_sf(data = census_temp_mean %>% 
            mutate(`Avg. Tmax` = tmax)
          , aes(fill = `Avg. Tmax`), color = NA) +
  geom_sf(fill = NA, color = "black") +
  geom_sf(data = county_centers %>% 
            mutate(`Climate Anxiety` = ClimateAnxiety), 
          aes(size = `Climate Anxiety`), color = "gray30") +
  scale_fill_distiller(palette = "Spectral")+
  theme_minimal() +
  theme(
    panel.grid = element_blank(),         # Remove gridlines
    axis.title = element_blank(),         # Remove axis titles
    axis.text = element_blank(),          # Remove axis text (longitude/latitude labels)
    axis.ticks = element_blank()          # Remove axis ticks
  )

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
  panel = "panel-2-2")

final_heatwave_plot <- lemon::reposition_legend(
  county_plot_year_heatwave,
  position = "center",
  panel = "panel-2-2")


pdf("Outputs/heatwave_map.pdf", width = 6, height = 6)
grid.draw(final_heatwave_plot)
dev.off()

pdf("Outputs/tmax_map.pdf", width = 6, height = 6)
grid.draw(final_tmax_plot)
dev.off()

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
la_heatmap <- left_join(la_shapefile, chis_data_la, by = c("county" = "fips_cnt"))

# Plot heatmap for Los Angeles County
ggplot(la_heatmap) +
  geom_sf(aes(fill = AverageAnxiety)) +
  geom_sf_text(aes(label = county), size = 5) +  # Adjust label size if necessary
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

left_join(la_shapefile, chis_data_la_year, by = c("county" = "fips_cnt")) %>% 
  ggplot() +
  geom_sf(aes(fill = AverageAnxiety)) +
  geom_sf_text(aes(label = county), size = 5) +  # Adjust label size if necessary
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

glmer(I(tf45 == "Yes") ~ scale(srage) + racecn_p + srsex +
        as.factor(year) + 
        # te68_3 + tf9 + tf2 + te83 + te69 + te64 +
        # te22 + td45 +
        # tf11 +
        # ti11 +
        scale(ppt_prior_yr_mean_delta) +  
        scale(vpdmax_prior_yr_mean_delta) + 
        scale(tmax_prior_yr_mean_delta)
      + (1 | county),
      family = "binomial",
     data = chis_design$variables, weights = chis_design$pweights/sum(chis_design$pweights) * nrow(chis),
     control=glmerControl(optimizer="bobyqa",
                          optCtrl=list(maxfun=100000))
     ) %>% summary()


test <- glmer.svyrep.design(I(tf45 == "Yes") ~ scale(srage) + racecn_p + srsex +
                             as.factor(year) + 
                             # te68_3 + tf9 + tf2 + te83 + te69 + te64 +
                             # te22 + td45 +
                             # tf11 +
                             # ti11 +
                             scale(ppt_prior_yr_mean_delta) +  
                             scale(vpdmax_prior_yr_mean_delta) + 
                             scale(tmax_prior_yr_mean_delta)
                              + (1 | tract10),
                            , family = "binomial",
                   design = chis_design,
                   control=glmerControl(optimizer="bobyqa",
                                                            optCtrl=list(maxfun=100000))
)

glm.test <- svyglm(I(tf45 == "Yes") ~ scale(srage) + racecn_p + srsex +
                     as.factor(year) + 
                     scale(ppt_prior_yr_mean_delta) +  
                     scale(vpdmax_prior_yr_mean_delta) + 
                     scale(tmax_prior_yr_mean_delta) +
                     scale(tmax_prior_yr_count35_delta) 
                           # + as.factor(tract10)
                   , family = "gaussian",
                           design = chis_design
)

glm.test %>% summary()
test %>% summary


