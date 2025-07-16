

# are temperatures correlated???
chis %>% group_by(county, year) %>%
  filter(n() == 1) %>%
  ungroup()

test <- chis %>% group_by(county, year, survey_months) %>% 
  slice(1) %>% 
  ungroup() %>% 
  arrange(county)
test <- chis %>% group_by(county, year) %>% 
  slice(1) %>% 
  ungroup() %>% 
  filter(year == 2021) %>% 
  arrange(county)


centroids <- st_centroid(california_shapefile) %>% 
  filter(county %in% unique(test$county)) %>% 
  arrange(county)

# Convert to matrix of coordinates
coords <- st_coordinates(centroids) 

nb <- dnearneigh(coords, 0, 150000)

distances <- nbdists(nb, coords)

# Convert to inverse distances
inv_dists <- lapply(distances, function(x) 1 / x)

# Create spatial weights list
lw <- nb2listw(nb, glist = inv_dists, style = "W", zero.policy = TRUE)

moran.test(test$tmean_prior_yr_mean, lw, zero.policy = TRUE)
moran.test(test$tmean_prior_yr_lag_mean, lw, zero.policy = TRUE)
moran.test(test$tmean_prior_yr_lag_mean_delta, lw, zero.policy = TRUE)

moran.test(test$tmean_prior_month_mean, lw, zero.policy = TRUE)
moran.test(test$tmean_prior_month_mean_delta, lw, zero.policy = TRUE)

moran.test(test$tmax_prior_yr_mean, lw, zero.policy = TRUE)
moran.test(test$tmax_prior_yr_mean_delta, lw, zero.policy = TRUE)
moran.test(test$tmax_prior_yr_lag_mean, lw, zero.policy = TRUE)
moran.test(test$tmax_prior_yr_lag_mean_delta, lw, zero.policy = TRUE)
moran.test(test$tmax_prior_yr_count35, lw, zero.policy = TRUE)
moran.test(test$tmax_prior_yr_count35_delta, lw, zero.policy = TRUE)


moran.test(test$tmax_prior_month_mean, lw, zero.policy = TRUE)
moran.test(test$tmax_prior_month_mean_delta, lw, zero.policy = TRUE)
moran.test(test$tmax_prior_month_count35, lw, zero.policy = TRUE)
moran.test(test$tmax_prior_month_count35_delta, lw, zero.policy = TRUE)


moran.test(as.numeric(test$tf45), lw, zero.policy = TRUE)

moran.test(test$K6, lw, zero.policy = TRUE)
moran.test(test$max_K6, lw, zero.policy = TRUE)
moran.test(test$s_depPCA, lw, zero.policy = TRUE)


test2 <- chis %>%
  group_by(county, year) %>%
  summarize(K6_mean = mean(K6),
            climate = mean(tf45 == "Yes"),
            temp = mean(tmax_prior_yr_mean),
            precip = mean(ppt_prior_yr_mean), .groups = "drop") %>% 
  arrange(county) %>% 
  filter(year == 2021)

lagsarlm(K6_mean ~ temp + precip , data = test2, listw = lw, zero.policy = TRUE) %>% summary()

lm_resid <- lm(K6_mean ~ temp + precip , data = test2) %>% residuals
moran.test(lm_resid, listw = lw)


lagsarlm(climate ~ temp + precip , data = test2, listw = lw, zero.policy = TRUE) %>% summary()

glm_resid <- glm(climate ~ temp + precip , data = test2) %>% residuals
moran.test(glm_resid, listw = lw)
