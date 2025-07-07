#### Functions for analysis and variable creation ####
pooling <- function(chis_list) {
  
  for(i in 1:length(chis_list)) {
    
    chis_list[[i]][ , paste0("fnwgt", 0:80)] <- chis_list[[i]][ , paste0("rakedw", 0:80)]
    
    chis_list[[i]] <- 
      chis_list[[i]] %>% 
      rename_at(vars(paste0("fnwgt", c(1:80))), ~ paste0("fnwgt", c(1:80) + 80*(i-1))) 
    
  }
  
  # chis_list <- chis_list %>%
  #   map(. %>% mutate(across(everything(), .fns = as.character)))
  
  merged <- 
    bind_rows(chis_list) %>% 
    data.frame(., row.names = NULL)
  
  # merged <-
  #   merged  %>% 
  #   mutate_all(type.convert, as.is = TRUE)
  
  merged <-
    merged  %>% 
    mutate(across(starts_with("fnwgt"), ~ ifelse(is.na(.), fnwgt0, .)))
  
  merged <- 
    merged %>% 
    mutate(across(starts_with("fnwgt"), ~ ./length(chis_list)))
  
  capitalize_first_only <- function(x) {
    x <- tolower(x)
    sub("^(.)", "\\U\\1", x, perl = TRUE)
  }
  
  capitalize_each_word <- function(x) {
    sapply(strsplit(tolower(x), " "), function(words) {
      paste(toupper(substring(words, 1, 1)), substring(words, 2), sep = "", collapse = " ")
    })
  }
  
  fix_encoding <- function(x) {
    iconv(x, from = "latin1", to = "UTF-8", sub = "byte")  # or sub = "" to drop bad characters
  }
  
  merged <- lapply(merged, function(col) {
    if (is.factor(col)) {
      fixed_levels <- fix_encoding(levels(col))
      levels(col) <- capitalize_each_word(fixed_levels)
    }
    col
  }) %>% as.data.frame()
  
  merged
}

K6_coding <- function(x) {
  out <- case_when(
    x == "All Of The Time" ~ 4,
    x == "Most Of The Time" ~ 3,
    x == "Some Of The Time" ~ 2,
    x == "A Little Of The Time" ~ 1,
    x == "Not At All" ~ 0,
    x == "Inapplicable" ~ 0,
    TRUE ~ NA_real_)
  out
}

create_K6_score <- function(x) {
  
  x <- x %>% 
    mutate(across(everything(),~ K6_coding(.x))) %>%
    rowSums(na.rm = TRUE)
}

mean_svy_rep <- function(data, full_data, variable, by, ...) {
  
  out <- dplyr::tibble()
  
  if (nrow(data) > 0) {
    new_design <- svrepdesign(
      data = data,
      weights = ~ fnwgt0,
      repweights = "fnwgt[1-9]",
      type = "other",
      scale = 1,
      rscales = 1,
      mse = TRUE
    )
  }
  
  if( !is.factor(full_data[[variable]]) && !is.character(full_data[[variable]])) {
    
    # Calculate mean and standard deviation
    mean_value <- svymean(as.formula(paste0("~", variable)), design = new_design, na.rm = TRUE) 
    se <- sqrt(attributes(mean_value)$var)
    sd_value <- sqrt(svyvar(as.formula(paste0("~", variable)), design = new_design, na.rm = TRUE))
    
    out <- dplyr::tibble(
      mean = mean_value %>% as.numeric(),
      se = se %>% as.numeric(),
      sd = sd_value
    )
  } else if ( is.factor(full_data[[variable]]) || is.character(full_data[[variable]]) ) {
    
    if(nrow(data) == 0) {
      return(dplyr::tibble(
        N = 0,
        p = 0
      ))
    }
    
    full_design <- svrepdesign(
      data = full_data,
      weights = ~ fnwgt0,
      repweights = "fnwgt[1-9]",
      type = "other",
      scale = 1,
      rscales = 1,
      mse = TRUE
    )
    
    new_design$variables$ones <- 1
    full_design$variables$ones <- 1
    
    N <- svytotal(~ones, design = full_design, na.rm = TRUE) %>% as.numeric()
    
    n_value <- svytotal(~ones, design = new_design, na.rm = TRUE) %>% as.numeric()
    
    # Calculate proportions
    prop_value <- n_value/N
    
    
    out <- dplyr::tibble(
      N = format(n_value, big.mark = ','),
      p = prop_value * 100
    )
  } else {
    stop(sprintf("Invalid type specified: '%s'. Use 'categorical', 'dichotomous', or 'continuous'.",full_data[[variable]] %>% typeof()))
  }
  
  
  return(out)
}

lmer.svyrep.design <- function(formula, design = NULL, data = NULL,
                               ...) {
  
  data <- design$variables
  w0   <- design$pweights
  weights <- design$repweights
  
  rscales <- design$rscales
  scale  <- design$scale
  
  data$.weights <- w0/sum(w0) * nrow(data)
  fit <- lmer(formula = formula, data = data, weights = .weights,
              ...)
  fit@call$data <- substitute(design$variables)
  fit@call$weights <- substitute(design$pweights)
  
  rep_fit <- lapply(weights, function(w) {
    new_args <- list(object = fit, weights = w/sum(w) * nrow(data) )
    do.call(update, new_args)
  })
  
  # Extract the coefficients and standard errors
  fe     <- fixef(fit)
  rep_fe <- sapply(rep_fit, fixef)
  
  re     <- ranef(fit)
  rep_re <- lapply(rep_fit, ranef)
  
  # Calculate the variance-covariance matrix
  vcov_fe<- tcrossprod(sweep(rep_fe,1,fe, "-") * sqrt(rscales)) * scale
  
  # Calculate the variance-covariance matrix for random effects
  vcov_re <- vector("list", length(re))
  names(vcov_re) <- names(re)
  
  for(i in seq_along(re)) {
    for (j in seq_along(re[[i]] ) ) {
      r_mat <- as.matrix(re[[i]][[j]])
      vcov_re[[i]][[j]] <- tcrossprod(sapply(rep_re, function(rr) sweep(rr[[i]][[j]] %>% as.matrix(),1,r_mat, "-") * sqrt(rscales))) * scale
      rownames(vcov_re[[i]][[j]]) <- colnames(vcov_re[[i]][[j]]) <- rownames(re[[i]][[j]])
      attr(re[[i]], "postVar")[,j,] <- diag(vcov_re[[i]][[j]])
    }
  }
  
  gnms <- function(x) c(outer(colnames(x), rownames(x), function(x, 
                                                                 y) paste(y, x, sep = ".")))
  rnms <- lapply(re, gnms)
  re_nms <- unlist(Map(function(n, r) paste(n, r, sep = "."), 
                       names(re), rnms))
  fix_nms <- names(fe)
  all_nms <- unname(c(re_nms, fix_nms))
  
  rep_param <- lapply(rep_fit, function(rf) {
    c(unlist(ranef(rf)), fixef(rf))
  })
  param <- c(unlist(re), fe)
  combined_vcov <- tcrossprod(sapply(rep_param, "-", param) * sqrt(rscales)) * scale
  dimnames(combined_vcov) <- list(all_nms, all_nms)
  
  out <- list(fit = fit, fe = fe, re = re,
              vcov = list(fe = vcov_fe,
                          re = vcov_re,
                          combined = combined_vcov))
  
  class(out) <- "svyrep_merMod"
  return(out)
}

glmer.svyrep.design <- function(formula, design = NULL, data = NULL,
                                ...) {
  
  data <- design$variables
  w0   <- design$pweights
  weights <- design$repweights
  
  rscales <- design$rscales
  scale  <- design$scale
  
  data$.weights <- w0/sum(w0)
  fit <- glmer(formula = formula, data = data, weights = .weights,
               ...)
  fit@call$data <- substitute(design$variables)
  fit@call$weights <- substitute(design$pweights)
  
  rep_fit <- lapply(weights, function(w) {
    new_args <- list(object = fit, weights = w/sum(w))
    do.call(update, new_args)
  })
  
  # Extract the coefficients and standard errors
  fe     <- fixef(fit)
  rep_fe <- sapply(rep_fit, fixef)
  
  re     <- ranef(fit)
  rep_re <- lapply(rep_fit, ranef)
  
  # Calculate the variance-covariance matrix
  vcov_fe<- tcrossprod(sweep(rep_fe,1,fe, "-") * sqrt(rscales)) * scale
  
  # Calculate the variance-covariance matrix for random effects
  vcov_re <- vector("list", length(re))
  names(vcov_re) <- names(re)
  
  for(i in seq_along(re)) {
    for (j in seq_along(re[[i]] ) ) {
      r_mat <- as.matrix(re[[i]][[j]])
      vcov_re[[i]][[j]] <- tcrossprod(sapply(rep_re, function(rr) sweep(rr[[i]][[j]] %>% as.matrix(),1,r_mat, "-") * sqrt(rscales))) * scale
      rownames(vcov_re[[i]][[j]]) <- colnames(vcov_re[[i]][[j]]) <- rownames(re[[i]][[j]])
      attr(re[[i]], "postVar")[,j,] <- diag(vcov_re[[i]][[j]])
    }
  }
  
  out <- list(fit = fit, fe = fe, re = re,
              vcov = list(fe = vcov_fe,
                          re = vcov_re))
  
  class(out) <- "svyrep_merMod"
  return(out)
}

summary.svyrep_merMod <- function(object, ...) {
  holder <- summary(object$fit)
  
  holder$vcov@x <- c(object$vcov$fe)
  holder$coefficients[,"Std. Error"] <- object$vcov$fe %>% diag() %>% sqrt()
  holder$coefficients[,3] <- holder$coefficients[,"Estimate"] / holder$coefficients[,"Std. Error"]
  
  if(ncol(holder$coefficients) > 3) {
    holder$coefficients[,4] <- 2 * pnorm(-abs(holder$coefficients[,3]))
  }
  
  return(holder)
  
}

print.svyrep_merMod <- function(x, ...) {
  print(x$fit)
}

ranef.svyrep_merMod <- function(object, ...) {
  object$re
}

fixef.svyrep_merMod <- function(object, ...) {
  object$fe
}

vcov.svyrep_merMod <- function(object, full = FALSE, ...) {
  # sigm <- sigma(object$fit)
  if(full) {
    if (is.null(object$vcov$re)) {
      stop("No random effects in this model.")
    }
    object$vcov$combined
  } else {
    object$vcov$fe 
  }
}

extractAIC.svyrep_merMod <- function(object, k = 2, ...) {
 
  fit <- object$fit
  ll <- logLik(refitML(fit))
  y  <- object$fit@resp$y
  muhat <- object$fit@resp$mu
  V0 <- vcov(object$fit, sigma = 1, full = TRUE)
  V  <- vcov(object, sigma = 1, full = TRUE)
  
  w  <- weights(object$fit)
  w  <- w/sum(w) * nobs(object$fit)
  Nhat <- sum(w) #* nobs(object$fit)
  
  sigma2hat <- sum((y - muhat)^2 * w)/Nhat
  minus2ellhat  <- -2 * logLik(refitML(object$fit)) # This is the log-likelihood of the model
    # Nhat * log(sigma2hat) + Nhat + Nhat * log(2 * pi)
  
  Delta_mu <- solve(V0 * sigma2hat, V)
  Isigma2 <- Nhat/(2 * sigma2hat^2)
  Usigma2 <- -1/(2 * sigma2hat) + (y - muhat)^2/(2 * sigma2hat^2)
  Hsigma2 <- sum(w * Usigma2^2)
  
  Deltasigma2 <- Isigma2/Hsigma2
  deltabar <- mean(c(diag(Delta_mu), Deltasigma2))
  eff.p <- sum(diag(Delta_mu)) + Deltasigma2
  aic <- minus2ellhat + k * eff.p
  
  c(eff.p = eff.p, AIC = aic, deltabar = deltabar)
}

AIC.svyrep_merMod <- function(object, k = 2, ...) {
  aic <- extractAIC(object, k = k, ...)
  aic["AIC"]
}

count_hot_days <- function(start_date, end_date, lon, lat, threshold = 35) {
  date_range <- as.character(seq(start_date, end_date, by = "day"))
  valid_dates <- date_range[date_range %in% names(tmax_map)]
  if (length(valid_dates) == 0) return(NA)
  
  rasters <- stack(tmax_map[valid_dates])
  vals <- extract(rasters, matrix(c(lon, lat), ncol = 2))
  sum(vals >= threshold, na.rm = TRUE)
}

create_var_from_prism <- function(sf, var, years, minDate = NULL, maxDate = NULL,
                                  time = "annual") {
  stopifnot("'var' must be a single character" = (length(var) == 1))
  stopifnot("'var' must be in  “ppt”, “tmean”, “tmin”, “tmax”, “vpdmin”, “vpdmax”, “tdmean”." = var %in%  c("tmean","tmax","tmin","ppt","vpdmin","vpdmax","tdmean"))
  stopifnot("'time' must be in 'annual' or 'daily'" = (time %in% c("annual", "daily")))
  
  ny <- length(years)
  stopifnot("'years' must be an integer greater than 1980 and less than 2025" = ny >=1)
  
  past_20 <- lapply(years, function(y) (y - 20L):(y-1L))
  
  var_list <- vector("list", ny)
  
  for(i in 1:ny) {
    var_list[[i]] <- prism::prism_archive_subset(var, time, years = past_20[[i]]) %>% 
      pd_stack() %>% 
      raster::calc(mean, na.rm = TRUE)
  }
  
  files <- prism::prism_archive_subset(type = var, 
                                       time, 
                                       years = years)
  stack <- prism::pd_stack(files)
  var_vals <- exactextractr::exact_extract(stack, sf, "mean")
  
  for (i in 1:ny){
    temp_name <- paste0(var, "_", years[[i]])
    delta_name<- paste0("delta_",var,"_",years[[i]])
    sf <- sf %>% 
      mutate(!!temp_name  := var_vals[[i]]) %>% 
      mutate(!!delta_name := .data[[temp_name]] -
               exactextractr::exact_extract(var_list[[i]], sf, "mean"))
  }
  
  return(sf)
  
}

data_var_from_prism <- function(sf, var, date_var) {
  stopifnot("'var' must be a single character" = (length(var) == 1))
  stopifnot("'var' must be in  “ppt”, “tmean”, “tmin”, “tmax”, “vpdmin”, “vpdmax”, “tdmean”." = var %in%  c("tmean","tmax","tmin","ppt","vpdmin","vpdmax","tdmean"))
  
  files <- prism_archive_subset(var, "daily")
  dates <- prism_archive_dates(var, "daily")
  var_daily <- setNames(files, as.character(dates))
  
  survey_date <- lubridate::my(sf[[date_var]])
  survey_year   <- lubridate::year(date_surv)
  survey_month  <- lubridate::month(date_surv)
  sf <- sf %>% mutate(survey_year = survey_year,
                      survey_month = survey_month,
                      survey_date = as.Date(paste(survey_year, survey_month, "15", sep = "-")))
  
  unique_years <- sort(unique(years))
  ny <- length(unique_years)
  
  
  past_20 <- lapply(unique_years, function(y) (y - 20L):(y-1L))
  
  var_list <- vector("list", ny)
  
  for(i in 1:ny) {
    var_list[[i]] <- prism::prism_archive_subset(var, "annual", years = past_20[[i]]) %>% 
      pd_stack() %>% 
      raster::calc(mean, na.rm = TRUE)
  }
  
  
  
  
  files <- prism::prism_archive_subset(type = var, 
                                       time, 
                                       years = years)
  stack <- prism::pd_stack(files)
  var_vals <- exactextractr::exact_extract(stack, sf, "mean")
  
  for (i in 1:ny){
    temp_name <- paste0(var, "_", years[[i]])
    delta_name<- paste0("delta_",var,"_",years[[i]])
    sf <- sf %>% 
      mutate(!!temp_name  := var_vals[[i]]) %>% 
      mutate(!!delta_name := .data[[temp_name]] -
               exactextractr::exact_extract(var_list[[i]], sf, "mean"))
  }
  
  if(var == "tmax") {
    sf %>%
      mutate(
        hot_days_month = pmap_dbl(list(survey_date, lon, lat), ~ count_hot_days(
          start_date = ..1 %m-% months(1) + 1,
          end_date = ..1,
          lon = ..2, lat = ..3
        )),
        
        hot_days_year = pmap_dbl(list(survey_date, lon, lat), ~ count_hot_days(
          start_date = ..1 - lubridate::years(1) + 1,
          end_date = ..1,
          lon = ..2, lat = ..3
        )),
        
        hot_days_lagged = pmap_dbl(list(survey_year, lon, lat), ~ count_hot_days(
          start_date = ..1 - lubridate::years(2) + 1,
          end_date = ..1 - lubridate::years(1),
          lon = ..2, lat = ..3
        ))
      )
  }
  
  
  return(sf)
  
}

data_var_from_prism <- function(sf, var) {
  stopifnot("'var' must be a single character" = (length(var) == 1))
  stopifnot("'var' must be in  \"ppt\", \"tmean\", \"tmin\", \"tmax\", \"vpdmin\", \"vpdmax\", \"tdmean\"." =
              var %in%  c("tmean","tmax","tmin","ppt","vpdmin","vpdmax","tdmean"))
  
  # Get all daily file paths and their dates for 'var'
  files_daily <- prism::prism_archive_subset(var, "daily")
  # Within each folder, find the .bil files
  bil_paths <- unlist(lapply(file.path(prism::prism_get_dl_dir(), files_daily), function(f) {
    list.files(f, pattern = "\\.bil$", full.names = TRUE)
  }))
  dates_daily <- prism::pd_get_date(files_daily)
  # Named vector: names are "YYYY-MM-DD"
  var_daily <- setNames(files_daily, as.character(dates_daily))
  
  # Identify unique survey years
  unique_years <- sort(unique(sf$survey_years))
  ny <- length(unique_years)
  
  # Precompute 20-year climatology rasters for each unique_year
  clim_rasters <- vector("list", ny)
  names(clim_rasters) <- as.character(unique_years)
  for(i in seq_along(unique_years)) {
    y <- unique_years[i]
    years_vec <- (y - 20L):(y - 1L)
    # Download (or locate) annual files for those years
    ann_stack_r <- prism::pd_stack(prism::prism_archive_subset(var, "annual", years = years_vec))
    ann_stack_t <- terra::rast(ann_stack_r)
    # Compute mean raster over 20-year period
    clim_rasters[[i]] <- terra::app(ann_stack_t, mean, na.rm = TRUE)
  }
  
  county_avg_days35 <- NULL
  unique_county_obs <- which(!duplicated(sf$county))
  unique_county_names <- sf$county[unique_county_obs]
  county_obs_sort <- unique_county_obs[order(unique_county_names)]
  unique_cal_geom <- sf$geometry[county_obs_sort]
  sorted_county_names <- sort(unique_county_names)
  cal_county <- terra::vect(unique_cal_geom)
  
  if (var == "tmax") {
    layer_dates <- as.Date(stringr::str_extract(basename(files_daily), "[0-9]{8}"), "%Y%m%d")
    # Then inside tapp(), subset `vals` to only those cells:
    year_factors <- factor(format(layer_dates, "%Y"))
    
    years_all <- unique(year_factors)
    years_all <- years_all[!(as.numeric(as.character(years_all)) %in% unique_years)] %>% droplevels()
    annual_files <- vector("list", length(years_all))
    names(annual_files) <- years_all
    
    for (yr in years_all) {
      idxs <- which(year_factors == yr)
      year_bil <- bil_paths[idxs]
      year_stack <- terra::rast(year_bil)
      year_count <- terra::app(year_stack, fun = function(v) sum(v > 35, na.rm = TRUE))
      outfile <- paste0("above35_", yr, ".tif")
      terra::writeRaster(year_count, filename = outfile, overwrite = TRUE,
                         wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW")))
      annual_files[[as.character(yr)]] <- outfile
      rm(year_stack, year_count); gc()
    }
    
    # Build a multi-layer from annual files
    annual_counts <- terra::rast(unlist(annual_files))
    names(annual_counts) <- years_all
    
    cnt_vect <- terra::vect(unique_cal_geom %>% st_transform(crs = terra::crs(annual_counts)))
    cnt_vect$CNTY_ID <- as.integer(factor(sorted_county_names, levels = sorted_county_names))
    cnt_rast <- terra::rasterize(cnt_vect, annual_counts[[1]], field = "CNTY_ID")
    
    # Zonal sum for each annual layer
    zonal_df <- terra::zonal(annual_counts, cnt_rast, fun = "mean", na.rm = TRUE)
    names(zonal_df)[1] <- "CNTY_ID"
    tzb <- as_tibble(zonal_df)
    zonal_long <- tzb %>% 
      tidyr::pivot_longer(cols = -CNTY_ID,
                          names_to = "year",
                          values_to = "days_above35") %>%
      mutate(year = as.integer(str_remove(year, "^layer\\.")))
    county_avg_days35 <- zonal_long %>%
      group_by(CNTY_ID) %>% 
      summarize(mean_days_above_35_2021  = mean(days_above35[year > 2000 & year < 2021], na.rm = TRUE),
                mean_days_above_35_2022 = mean(days_above35[year > 2001 & year < 2022], na.rm = TRUE),
                mean_days_above_35_2023 = mean(days_above35[year > 2002 & year < 2023], na.rm = TRUE)) %>%
      ungroup() %>% 
      mutate(county =  sorted_county_names) %>% 
      tidyr::pivot_longer(cols = starts_with("mean_days_above_35_"),
                          names_to = "long_year",
                          values_to = "mean_days_above35") %>% 
      mutate(year = as.integer(stringr::str_extract(long_year, "[0-9]{4}"))) %>% 
      dplyr::select(-long_year)
    # sf <- sf %>%
    #   left_join(county_avg, by = c("county","year"))
    
  }
  
  # get unique month year combos
  my_unique <- paste(sf$survey_years, sf$survey_month, sep = "-") %>% unique()
  
  # Create dynamic results for each unique month-year combination
  results <- purrr::map_dfr(my_unique, function(d) {
    # Parse the month-year string into a Date object
    sd <-  lubridate::ym(d) + 14
    # Get the year and month from the Date object
    sy <- lubridate::year(sd)
    sm <- lubridate::month(sd)
    
    # Prior month window
    pm_date <- sd %m-% months(1)
    pm_start <- floor_date(pm_date, "month")
    pm_end   <- ceiling_date(pm_date, "month") - days(1)
    
    # 1-year prior window
    prior_start <- sd %m-% years(1)
    prior_end   <- sd %m-% days(1)
    
    # Lagged 1-year window
    lag_start   <- sd %m-% years(2)
    lag_end     <- sd %m-% years(1) %m-% days(1)
    
    # 1-month prior daily subset
    date_names <- as.Date(names(var_daily))
    sel_pm <- var_daily[which(date_names >= pm_start & date_names <= pm_end)]
    if(length(sel_pm) > 0) {
      stack_pm <- prism::prism_archive_subset(var, "daily", minDate = pm_start, maxDate = pm_end) %>%
        pd_stack() %>%
        terra::rast() %>% 
        terra::crop(cal_county) %>% 
        terra::mask(cal_county)
      
      pm_means  <- terra::app(stack_pm, mean, na.rm = TRUE)
      pm_mean  <- terra::extract(pm_means, cal_county, 
                                 fun = mean, na.rm = TRUE) %>% 
        .[,2]
      if(var == "tmax") {
        pm_count35 <- terra::app(stack_pm, function(x, ...) sum(x > 35,...), 
                                 na.rm = TRUE) %>% 
          terra::extract(cal_county, fun = mean, na.rm = TRUE) %>% 
          .[,2]
      } else {
        pm_count35 <- NA_integer_
      }
    } else {
      pm_mean <- NA_real_
      pm_count35 <- NA_integer_
    }
    
    # 1-year prior daily subset
    sel_prior <- var_daily[which(date_names >= prior_start & date_names <= prior_end)]
    if(length(sel_prior) > 0) {
      stack_prior <- prism::prism_archive_subset(var, "daily", minDate = prior_start, maxDate = prior_end) %>%
        pd_stack() %>%
        terra::rast() %>% 
        terra::crop(cal_county) %>%
        terra::mask(cal_county)
      
      yr_prior_means <- terra::app(stack_prior, fun = "mean", na.rm = TRUE)
      yr_prior_mean  <- terra::extract(yr_prior_means, 
                                       cal_county, fun = mean, 
                                       na.rm = TRUE) %>% 
        .[,2]
      if(var == "tmax") {
        yr_prior_count35 <-  terra::app(stack_prior, 
                                        function(x, ...) sum(x > 35,...), 
                                        na.rm = TRUE) %>% 
          terra::extract(cal_county, fun = mean, na.rm = TRUE) %>% 
          .[,2]
      } else {
        yr_prior_count35 <- NA_integer_
      }
    } else {
      yr_prior_mean <- NA_real_
      yr_prior_count35 <- NA_integer_
    }
    
    # Lagged 1-year daily subset
    sel_lag <- var_daily[which(date_names >= lag_start & date_names <= lag_end)]
    if(length(sel_lag) > 0) {
      stack_lag <- prism::prism_archive_subset(var, "daily", minDate = lag_start, maxDate = lag_end) %>%
        pd_stack() %>%
        terra::rast() %>% 
        terra::crop(cal_county) %>%
        terra::mask(cal_county)
      
      lag_means <- terra::app(stack_lag, fun = mean, na.rm = TRUE)
      lag_mean <- terra::extract(lag_means, cal_county, 
                                 fun = mean, na.rm = TRUE) %>% 
        .[,2]
      if(var == "tmax") {
        yr_lag_count35 <- terra::app(stack_lag, 
                                     function(x, ...) sum(x > 35,...), 
                                     na.rm = TRUE) %>% 
          terra::extract(cal_county, fun = mean, na.rm = TRUE) %>% 
          .[,2]
      } else {
        yr_lag_count35 <- NA_integer_
      }
    } else {
      lag_mean <- NA_real_
    }
    
    # Climatology raster for this survey_year
    clim_ras <- clim_rasters[[as.character(sy)]]
    clim_val <- terra::app(clim_ras %>% 
                             terra::crop(cal_county) %>% 
                             terra::mask(cal_county), 
                           fun = "mean", na.rm = TRUE) %>% 
      terra::extract(cal_county, fun = mean, na.rm = TRUE) %>% 
      .[,2]
    
    tibble(
      survey_dates = sd,
      survey_months = sm,
      survey_years = sy,
      county = sorted_county_names,
      !!paste0(var, "_prior_month_mean")    := pm_mean,
      !!paste0(var, "_prior_month_count35") := pm_count35,
      !!paste0(var, "_prior_yr_mean")             := yr_prior_mean,
      !!paste0(var, "_prior_yr_count35")          := yr_prior_count35,
      !!paste0(var, "_prior_yr_lag_mean")         := lag_mean,
      !!paste0(var, "_prior_yr_lag_count35")      := yr_lag_count35,
      !!paste0(var, "_20yr_normal")         := clim_val
    )
    
  })
  
  # Bind dynamic results back to original sf (dropping geometry column)
  sf_out <- sf %>%
    left_join(results, by = c("survey_dates", "survey_months", "survey_years", "county")) %>%
    mutate(
      across(starts_with(var) & ends_with("_mean"), 
             ~ . - !!sym(paste0(var, "_20yr_normal")),
             .names = "{.col}_delta")
    )
  
  if (var == "tmax")  {
    sf_out <-  sf_out %>%
      left_join(county_avg_days35,
                by = c("county", "year")) %>%
      mutate(
        tmax_prior_month_count35_delta = tmax_prior_month_count35 - (mean_days_above35/12),
        tmax_prior_yr_count35_delta = tmax_prior_yr_count35  - mean_days_above35,
        tmax_lag_yr_count35_delta = tmax_prior_yr_lag_count35    - mean_days_above35
      )
  }
  
  return(sf_out)
}

parse_my_to_date <- function(my_str) {
  # my_str like "November 2023" or "NOVEMBER 2023"
  dt <- lubridate::my(my_str)
  # set to 15th of that month
  as.Date(paste0(lubridate::year(dt), "-", lubridate::month(dt), "-15"))
}
