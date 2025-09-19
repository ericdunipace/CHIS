#### Functions for analysis and variable creation ####
pooling <- function(chis_list) {
  require(dplyr)
  
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
  require(dplyr)
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
  require(dplyr)
  x <- x %>% 
    mutate(across(everything(),~ K6_coding(.x))) %>%
    rowSums(na.rm = TRUE)
}

mean_svy_rep <- function(data, full_data, variable, by, ...) {
  require(dplyr)
  require(survey)
  
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
                               get.coef = FALSE,
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
                                get.coef = FALSE, verbose = FALSE,
                                ...) {
  
  data <- design$variables
  w0   <- design$pweights
  weights <- design$repweights
  n    <- nrow(data)
  
  rscales <- design$rscales
  scale  <- design$scale
  
  data$.weights <- w0/sum(w0) * n
  
  if (isTRUE(verbose)) message("Running model")
  fit <- lme4::glmer(formula = formula, data = data, weights = .weights,
               ...)
  fit@call$data <- substitute(design$variables)
  fit@call$weights <- substitute(design$pweights/sum(design$pweights) * nrow(design$variables))
  
  if (isTRUE(verbose)) message("Running replicate weights. Please be patient.")
  rep_fit <- lapply(weights, function(w) {
    new_args <- list(object = fit, weights = w/sum(w) * n )
    do.call(update, new_args)
  })
  
  if (isTRUE(verbose)) message("Calculating variance-covariance matrices.")
  
  # Extract the coefficients and standard errors
  fe     <- fixef(fit) 
  rep_fe <- sapply(rep_fit, fixef)  %>% matrix(ncol = length(rep_fit))
  
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
  
  param_list <- NULL
  
  if (isTRUE(get.coef)) {
    names(param) <- all_nms
    rep_param <- lapply(rep_param, function(rp) {
      names(rp) <- all_nms
      rp
    })
    param_list <- list(param = param,
                       rep_param= rep_param)
  }
  
  out <- list(fit = fit, fe = fe, re = re,
              vcov = list(fe = vcov_fe,
                          re = vcov_re,
                          combined = combined_vcov),
              param = param_list
  )
  
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

prism_to_map <- function(var, sf, years, admin.level, heatwave = FALSE, cutoff = 32) {
  stopifnot("'var' must be a single character" = (length(var) == 1))
  stopifnot("'var' must be in  \"ppt\", \"tmean\", \"tmin\", \"tmax\", \"vpdmin\", \"vpdmax\", \"tdmean\"." =
              var %in%  c("tmean","tmax","tmin","ppt","vpdmin","vpdmax","tdmean"))
  admin.level <- match.arg(admin.level, c("county","census")) %>% 
    switch(
      "county" = "county",
      "zip" = "bestzip",
      "census" = "tract10"
    )
  
  files_daily <- prism::prism_archive_subset(var, "daily")
  # Within each folder, find the .bil files
  bil_paths <- unlist(lapply(file.path(prism::prism_get_dl_dir(), files_daily), function(f) {
    list.files(f, pattern = "\\.bil$", full.names = TRUE)
  }))
  dates_daily <- prism::pd_get_date(files_daily)
  # Named vector: names are "YYYY-MM-DD"
  var_daily <- setNames(files_daily, as.character(dates_daily))
  
  # Identify unique survey years
  unique_years <- sort(unique(years))
  ny <- length(unique_years)
  
  admin_avg_days_cutoff <- NULL
  # unique_admin_obs <- which(!duplicated(sf[[admin.level]]))
  # unique_admin_names <- sf[[admin.level]][unique_admin_obs]
  # admin_obs_sort <- unique_admin_obs[order(unique_admin_names)]
  # unique_cal_geom <- sf$geometry[admin_obs_sort]
  # sorted_admin_names <- sort(unique_admin_names)
  # cal_admin <- terra::vect(unique_cal_geom)
  cal_admin <- terra::vect(sf$geometry)
  
  if (var == "tmax" & heatwave == TRUE) {
    layer_dates <- as.Date(stringr::str_extract(basename(files_daily), "[0-9]{8}"), "%Y%m%d")
    # Then inside tapp(), subset `vals` to only those cells:
    year_factors <- factor(format(layer_dates, "%Y"))
    
    years_all    <- unique(year_factors)
    # years_all    <- years_all[!(as.numeric(as.character(years_all)) %in% unique_years)] %>% 
    #   droplevels()
    
    years_search <- as.character(years_all[as.numeric(as.character(years_all)) %in% unique_years])
    
    annual_files <- vector("list", length(years_search))
    names(annual_files) <- years_search
    
    for (yr in years_search) {
      outfile    <- paste0(glue("{admin.level}_above{cutoff}_{yr}.tiff"))
      annual_files[[as.character(yr)]] <- outfile
      
      if (!file.exists(outfile)) {
        idxs       <- which(as.character(year_factors) == yr)
        year_bil   <- bil_paths[idxs]
        year_stack <- terra::rast(year_bil) %>% terra::project(cal_admin)
        year_crop  <- terra::mask(terra::crop(year_stack, cal_admin), cal_admin)
        year_count <- terra::app(year_crop, 
                                 fun = function(v) sum(v > cutoff, na.rm = TRUE))
        
        terra::writeRaster(year_count, filename = outfile, overwrite = TRUE,
                           wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW")))
        rm(year_stack, year_count); gc()
      }
      
    }
    
    # Build a multi-layer from annual files
    annual_counts        <- terra::rast(unlist(annual_files))
    names(annual_counts) <- unique_years
    
    # Zonal sum for each annual layer
    zonal_df <- terra::zonal(annual_counts, cal_admin,
                             fun = "mean", na.rm = TRUE)
    
    zonal_df[[admin.level]] <- rownames(zonal_df) <- sf[[admin.level]]
    
    # tzb <- as_tibble(zonal_df)
    zonal_long <- zonal_df %>% 
      tidyr::pivot_longer(cols = -all_of(admin.level),
                          names_to = "year",
                          values_to = paste0(glue("days_above{cutoff}")))
    
    file.remove(unlist(annual_files)) # remove the annual files
    
    return(zonal_long %>% 
             left_join(y = sf, by = admin.level) %>% st_as_sf())
    
  }
  
  # Precompute 20-year climatology rasters for each unique_year
  clim_rasters <- vector("list", ny)
  names(clim_rasters) <- as.character(unique_years)
  for(i in seq_along(unique_years)) {
    y <- unique_years[i]
    # Download (or locate) annual files for those years
    ann_stack_r <- prism::pd_stack(prism::prism_archive_subset(var, "annual", years = y))
    ann_stack_t <- terra::rast(ann_stack_r) %>% terra::project(cal_admin)
    ann_mask_ca <- terra::mask(terra::crop(ann_stack_t, cal_admin), cal_admin)
    clim_rasters[[i]] <- terra::app(ann_mask_ca, mean, na.rm = TRUE)
  }
  
  zonal <- dplyr::bind_rows(
    lapply(seq_along(unique_years), function(i) 
      sf %>% mutate(year = unique_years[[i]],
                    {{var}} := clim_rasters[[i]][,2]))
  )
  
  return(zonal)
  
}

data_var_from_prism <- function(data, var, sf, admin.level = "county",
                                cutoff = 32) {
  stopifnot("'var' must be a single character" = (length(var) == 1))
  stopifnot("'var' must be in  \"ppt\", \"tmean\", \"tmin\", \"tmax\", \"vpdmin\", \"vpdmax\", \"tdmean\"." =
              var %in%  c("tmean","tmax","tmin","ppt","vpdmin","vpdmax","tdmean"))
  admin.level <- match.arg(admin.level, c("county","census")) %>% 
    switch(
      "county" = "county",
      "zip" = "bestzip",
      "census" = "tract10"
    )
  
  # Get all daily file paths and their dates for 'var'
  files_daily <- prism::prism_archive_subset(var, "daily")
  # Within each folder, find the .bil files
  bil_paths <- unlist(lapply(file.path(prism::prism_get_dl_dir(), files_daily), function(f) {
    list.files(f, pattern = "\\.bil$", full.names = TRUE)
  }))
  dates_daily <- prism::pd_get_date(files_daily) %>% lubridate::date()
  # Named vector: names are "YYYY-MM-DD"
  var_daily <- setNames(files_daily, as.character(dates_daily))
  
  # Identify unique survey years
  unique_years <- sort(unique(data$survey_years))
  ny <- length(unique_years)
  
  # Get the California shapefile
  cal_admin <- terra::vect(sf$geometry)
  
  # Precompute 20-year climatology rasters for each unique_year
  clim_rasters <- vector("list", ny)
  names(clim_rasters) <- as.character(unique_years)
  for(i in seq_along(unique_years)) {
    y <- unique_years[i]
    years_vec <- (y - 16L):(y - 2L)
    # Download (or locate) annual files for those years
    ann_stack_r <- prism::pd_stack(prism::prism_archive_subset(var, "annual", years = years_vec))
    ann_stack_t <- terra::rast(ann_stack_r) %>% terra::project(cal_admin)
    ann_stack_ca<- terra::mask(terra::crop(ann_stack_t, cal_admin), cal_admin)
    # Compute mean raster over 20-year period
    clim_rasters[[i]] <- terra::app(ann_stack_ca, mean, na.rm = TRUE)
    # y <- unique_years[i]
    # years_vec <- 2004:2019
    # # Download (or locate) annual files for those years
    # ann_stack_r <- prism::pd_stack(prism::prism_archive_subset(var, "annual", years = years_vec))
    # ann_stack_t <- terra::rast(ann_stack_r)
    # # Compute mean raster over 20-year period
    # clim_rasters[[i]] <- terra::app(ann_stack_t, mean, na.rm = TRUE)
  }
  
  admin_avg_days_cutoff <- NULL
  # unique_admin_obs <- which(!duplicated(sf[[admin.level]]))
  # unique_admin_names <- sf[[admin.level]][unique_admin_obs]
  # admin_obs_sort <- unique_admin_obs[order(unique_admin_names)]
  # unique_cal_geom <- sf$geometry[admin_obs_sort]
  # sorted_admin_names <- sort(unique_admin_names)
  # cal_admin <- terra::vect(unique_cal_geom)
  
  if (var == "tmax") {
    layer_dates <- as.Date(stringr::str_extract(basename(files_daily), "[0-9]{8}"), "%Y%m%d")
    # Then inside tapp(), subset `vals` to only those cells:
    year_factors <- factor(format(layer_dates, "%Y"))
    
    years_all    <- unique(year_factors)
    # years_all    <- years_all[!(as.numeric(as.character(years_all)) %in% unique_years)] %>% 
    #   droplevels()
    annual_files <- vector("list", length(years_all))
    names(annual_files) <- years_all
    
    for (yr in years_all) {
      outfile    <- paste0(glue("{var}_{admin.level}_above{cutoff}_{yr}.tiff"))
      annual_files[[as.character(yr)]] <- outfile
      
      if (!file.exists(outfile)) {
        idxs       <- which(year_factors == yr)
        year_bil   <- bil_paths[idxs]
        year_stack <- terra::rast(year_bil) %>% terra::project(cal_admin)
        year_ca    <- terra::mask(terra::crop(year_stack, cal_admin), cal_admin)
        year_count <- terra::app(year_ca, 
                                 fun = function(v) sum(v > cutoff, na.rm = TRUE))
        
        terra::writeRaster(year_count, filename = outfile, overwrite = TRUE,
                           wopt = list(datatype = "INT2U", gdal = c("COMPRESS=LZW")))
        rm(year_stack, year_count); gc()
      }
      
    }
    
    # Build a multi-layer from annual files
    annual_counts        <- terra::rast(unlist(annual_files))
    names(annual_counts) <- years_all
    
    # project cal_admin vector to annual_counts raster
    # cal_proj <- terra::project(cal_admin, annual_counts[[1]])
    
    # Zonal sum for each annual layer
    zonal_df <- terra::zonal(annual_counts, cal_admin,
                             fun = "mean", na.rm = TRUE)
    
    zonal_df[[admin.level]] <- rownames(zonal_df) <- sf[[admin.level]]
    
    # tzb <- as_tibble(zonal_df)
    zonal_long <- zonal_df %>% 
      tidyr::pivot_longer(cols = -all_of(admin.level),
                          names_to = "year",
                          values_to = paste0(glue("days_above{cutoff}")))
    admin_avg_days_cutoff <- zonal_long %>%
      group_by(!!sym(glue("{admin.level}"))) %>% 
      summarize("2021" := mean(.data[[glue("days_above{cutoff}")]][year >= 2005 & year < 2020], na.rm = TRUE),
                "2022" := mean(.data[[glue("days_above{cutoff}")]][year >= 2006 & year < 2021], na.rm = TRUE),
                "2023" := mean(.data[[glue("days_above{cutoff}")]][year >= 2007 & year < 2022], na.rm = TRUE)) %>%
      ungroup() %>% 
      # mutate({{admin.level}} :=  sorted_admin_names) %>% 
      tidyr::pivot_longer(cols = -all_of(admin.level),
                          names_to = "year",
                          values_to = paste0(glue("mean_days_above{cutoff}") ))
  }
  
  # get unique month year combos
  my_unique <- paste(data$survey_years, 
                     data$survey_month, sep = "-") %>% unique()
  
  # Create dynamic results for each unique month-year combination
  results <- purrr::map_dfr(my_unique, function(d) {
    # Parse the month-year string into a Date object
    sd <-  lubridate::ym(d) + lubridate::days(14)
    # Get the year and month from the Date object
    sy <- lubridate::year(sd)
    sm <- lubridate::month(sd)
    
    # Prior month window
    pm_date  <- sd 
    pm_start <- pm_date %m-% lubridate::days(90)
    pm_end   <- pm_date %m-% lubridate::days(1)
    
    # 1-year prior window
    prior_start <- sd %m-% lubridate::years(1)
    prior_end   <- sd %m-% lubridate::days(1)
    
    # Lagged 1-year window
    lag_start   <- sd %m-% lubridate::years(2)
    lag_end     <- sd %m-% lubridate::years(1) %m-% lubridate::days(1)
    
    # 3-month prior daily subset
    date_names <- as.Date(names(var_daily))
    sel_pm <- bil_paths[which(date_names >= pm_start & date_names <= pm_end)]
    if(length(sel_pm) > 0) {
      stack_pm <- terra::rast(sel_pm) %>% 
        terra::project(cal_admin) %>% 
        terra::crop(cal_admin) %>% 
        terra::mask(cal_admin)
      # stack_pm <- prism::prism_archive_subset(var, "daily", minDate = pm_start, maxDate = pm_end) %>%
      #   prism::pd_stack() %>%
      #   terra::rast() %>% terra::project(cal_admin) %>%
      #   terra::crop(cal_admin) %>% 
      #   terra::mask(cal_admin)
      
      pm_means  <- terra::app(stack_pm, mean, na.rm = TRUE)
      pm_mean  <- terra::extract(pm_means, cal_admin, 
                                 fun = mean, na.rm = TRUE) %>% 
        .[,2]
      
      stack_pm_normal_list <- lapply(2:16, function(yy) {
        pmn_idx     <- which(dates_daily >= (pm_start %m-% lubridate::years(yy)) & dates_daily <= (pm_end %m-% lubridate::years(yy)))
        pmn_bil     <- bil_paths[pmn_idx]
        stack_pm <- terra::rast(pmn_bil) %>% 
          terra::project(cal_admin) %>% 
          terra::crop(cal_admin) %>% 
          terra::mask(cal_admin)
        # prism::prism_archive_subset(var, "daily", minDate = pm_start %m-% lubridate::years(yy), maxDate = pm_end %m-% lubridate::years(yy)) %>%
        #   # unlist()
        #   pd_stack() %>%
        #   terra::rast() %>% terra::project(cal_admin) %>%
        #   terra::crop(cal_admin) %>% 
        #   terra::mask(cal_admin)
      })
      names(stack_pm_normal_list) <- as.character(2:16)
      
      pm_mean_normal <- lapply(stack_pm_normal_list, function(d) {
        terra::app(d, mean, na.rm = TRUE) %>% 
          terra::extract(cal_admin, fun = mean, na.rm = TRUE) %>% 
          .[,2]
      }) %>% 
        bind_cols() %>% 
        rowMeans(na.rm = TRUE)
      
      if(var == "tmax") {
        # Count days above cutoff
        pm_count <- terra::app(stack_pm, function(x, ...) sum(x > cutoff,...), 
                               na.rm = TRUE) %>% 
          terra::extract(cal_admin, fun = mean, na.rm = TRUE) %>% 
          .[,2]
        
        pm_count_normal <- lapply(stack_pm_normal_list, function(d) {
          terra::app(d, function(x, ...) sum(x > cutoff,...), 
                     na.rm = TRUE) %>% 
            terra::extract(cal_admin, fun = mean, na.rm = TRUE) %>% 
            .[,2]
        }) %>% 
          bind_cols() %>% 
          rowMeans(na.rm = TRUE)
        
      } else {
        pm_count <- pm_count_normal <- NA_integer_
      }
    } else {
      pm_mean <- pm_mean_normal <- NA_real_
      pm_count <-  pm_count_normal <- NA_integer_
      
    }
    
    # 1-year prior daily subset
    sel_prior <- bil_paths[which(date_names >= prior_start & date_names <= prior_end)]
    if(length(sel_prior) > 0) {
      # Get the bil files for the prior year
      stack_prior <- terra::rast(sel_prior) %>%
         terra::project(cal_admin) %>%
        terra::crop(cal_admin) %>%
        terra::mask(cal_admin)
      
      yr_prior_means <- terra::app(stack_prior, fun = "mean", na.rm = TRUE)
      yr_prior_mean  <- terra::extract(yr_prior_means, 
                                       cal_admin, fun = mean, 
                                       na.rm = TRUE) %>% 
        .[,2]
      if(var == "tmax") {
        yr_prior_count <-  terra::app(stack_prior, 
                                      function(x, ...) sum(x > cutoff,...), 
                                      na.rm = TRUE) %>% 
          terra::extract(cal_admin, fun = mean, na.rm = TRUE) %>% 
          .[,2]
      } else {
        yr_prior_count <- NA_integer_
      }
    } else {
      yr_prior_mean <- NA_real_
      yr_prior_count <- NA_integer_
    }
    
    # Lagged 1-year daily subset
    sel_lag <- bil_paths[which(date_names >= lag_start & date_names <= lag_end)]
    if(length(sel_lag) > 0) {
      stack_lag <- terra::rast(sel_lag) %>% 
        terra::project(cal_admin) %>% 
        terra::crop(cal_admin) %>%
        terra::mask(cal_admin)
      
      lag_means <- terra::app(stack_lag, fun = mean, na.rm = TRUE)
      lag_mean <- terra::extract(lag_means, cal_admin, 
                                 fun = mean, na.rm = TRUE) %>% 
        .[,2]
      if(var == "tmax") {
        yr_lag_count <- terra::app(stack_lag, 
                                   function(x, ...) sum(x > cutoff,...), 
                                   na.rm = TRUE) %>% 
          terra::extract(cal_admin, fun = mean, na.rm = TRUE) %>% 
          .[,2]
      } else {
        yr_lag_count <- NA_integer_
      }
    } else {
      lag_mean <- NA_real_
    }
    
    # Climatology raster for this survey_year
    clim_ras <- clim_rasters[[as.character(sy)]]
    clim_val <- terra::app(clim_ras %>% 
                             terra::crop(cal_admin) %>% 
                             terra::mask(cal_admin), 
                           fun = "mean", na.rm = TRUE) %>% 
      terra::extract(cal_admin, fun = mean, na.rm = TRUE) %>% 
      .[,2]
    
    tibble(
      survey_dates = sd,
      survey_months = sm,
      survey_years = sy,
      !!glue({admin.level}) := sf[[admin.level]],
      !!paste0(var, "_" , admin.level, "_prior_90_days_mean") := pm_mean,
      !!paste0(var, "_" , admin.level, "_prior_90_days_mean_normal") := pm_mean_normal, 
      !!paste0(var, "_" , admin.level, glue("_prior_90_days_count{cutoff}")) := pm_count,
      !!paste0(var, "_" , admin.level, glue("_prior_90_days_count{cutoff}_normal")) := pm_count_normal,
      !!paste0(var, "_" , admin.level, "_prior_yr_mean")  := yr_prior_mean,
      !!paste0(var, "_" , admin.level, glue("_prior_yr_count{cutoff}"))          := yr_prior_count,
      !!paste0(var, "_" , admin.level, "_lag_yr_mean") := lag_mean,
      !!paste0(var, "_" , admin.level, glue("_lag_yr_count{cutoff}"))      := yr_lag_count,
      !!paste0(var, "_" , admin.level, "_15yr_normal")  := clim_val
    )
    
  })
  
  # Bind dynamic results back to original sf (dropping geometry column)
  data_out <- data %>%
    left_join(results, by = c("survey_dates", "survey_months", "survey_years", admin.level)) %>%
    mutate(
      across(starts_with(var) & ends_with("yr_mean"), 
             ~ . - !!sym(paste0(var, "_", admin.level, "_15yr_normal")),
             .names = "{.col}_delta"),
      across(starts_with(var) & ends_with("_90_days_mean"),
             ~ . - !!sym(paste0(var, "_", admin.level, "_prior_90_days_mean_normal")),
             .names = "{.col}_delta"
      )
    )
  
  if (var == "tmax")  {
    data_out <-  data_out %>%
      left_join(admin_avg_days_cutoff %>% mutate(year = as.numeric(year)),
                by = c(admin.level, "year")) %>%
      mutate(
        !!glue("tmax_{admin.level}_prior_90_days_count{cutoff}_delta") := .data[[glue("tmax_{admin.level}_prior_90_days_count{cutoff}")]] - .data[[glue("tmax_{admin.level}_prior_90_days_count{cutoff}_normal")]],
        !!glue("tmax_{admin.level}_prior_yr_count{cutoff}_delta") := .data[[glue("tmax_{admin.level}_prior_yr_count{cutoff}")]]  - .data[[glue("mean_days_above{cutoff}")]],
        !!glue("tmax_{admin.level}_lag_yr_count{cutoff}_delta") := .data[[glue("tmax_{admin.level}_lag_yr_count{cutoff}")]] - .data[[glue("mean_days_above{cutoff}")]]
      )
    file.remove(unlist(annual_files))
  }
  
  return(data_out)
}

parse_my_to_date <- function(my_str) {
  # my_str like "November 2023" or "NOVEMBER 2023"
  dt <- lubridate::my(my_str)
  # set to 15th of that month
  as.Date(paste0(lubridate::year(dt), "-", lubridate::month(dt), "-15"))
}

chis_clean_puf <- function(data) {
  pooling(data) %>%
    mutate(across(where(is.factor), fct_drop)) %>% 
    mutate(
      age_group = factor(ifelse(srage_p %in% c("12","13","14"),
                                "12-14", "15-17")),
      cont_age  = as.numeric(as.character(srage_p)),
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
      tl53 = forcats::fct_rev(tl53),
      
    ) %>% 
    mutate(survey_dates = parse_my_to_date(tadate_mm),
           survey_years  = lubridate::year(survey_dates),
           survey_months = lubridate::month(survey_dates))
}

chis_clean <- function(data) {
  pooling(data) %>%
    mutate(
      age_group = factor(ifelse(srage %in% c("12","13","14"),
                                "12-14", "15-17")),
      cont_age  = as.numeric(as.character(srage)),
      ombsrreo = forcats::fct_recode(ombsrreo,
                                       "Hispanic" = "Hispanic",
                                       "White" = "White, Non-hispanic (nh)",
                                       "Asian" = "Asian Only, Nh",
                                       "African American" = "African American Only, Not Hispanic",
                                       "American Indian" = "American Indian/alaska Native Only, Nh",
                                       "Pacific Islander" = "Native Hawaiian/pacific Islander, Nh",
                                       
                                       "Two Or More Races" = "Two Or More Races, Nh"),
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
      ahedtc_binary = forcats::fct_recode(aheduc,
                                          "Not known" = "Not Ascertained",
                                          "Not known" = "Dont Know",
                                          "Not known" = "Refused",
                                          "Not known" = "Inapplicable",
                                          "Not known" = "Proxy Skipped",
                                          "No college" = "No Formal Education",
                                          "No college" = "Grade 1-8",
                                          "No college" = "Grade 9-11",
                                          "No college" = "Grade 12/h.s. Diploma",
                                          "College or more" = "Some College",
                                          "College or more" = "Aa Or As Degree",
                                          "College or more" = "Vocational School",
                                          "College or more" = "Ba Or Bs Degree",
                                          "College or more" = "Some Grad. School",
                                          "College or more" = "Ma Or Ms Degree",
                                          "College or more" = "Ph.d. Or Equivalent"
      ),
      lnghmt_binary = forcats::fct_recode(langhome,
                                          "English" = "English & Spanish",
                                          "English" = "English & One Other Language",
                                          "English" = "English & European Language",
                                          "English" = "English & Another Asian Language",
                                          "Non-English" = "Spanish",
                                          "Non-English" = "Chinese",
                                          "Non-English" = "Korean",
                                          "Non-English" = "Vietnamese",
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
                                             "No" = "Not Ascertained",
                                             "No" = "Don't Know",
                                             "No" = "Refused",
                                             "No" = "Inapplicable",
                                             "Yes" = "Home Schooled"
      ),
      tl53 = forcats::fct_rev(tl53),
      
    ) %>% 
    mutate(survey_dates = parse_my_to_date(tadate_mm),
           survey_years  = lubridate::year(survey_dates),
           survey_months = lubridate::month(survey_dates)) %>% 
    mutate(across(where(is.factor), fct_drop))
}


# adapted from lemon, not currently in DAC packages unfortunately
fix_reposition_legend <- function(aplot,
                              position=NULL,
                              legend=NULL,
                              panel='panel',
                              x=NULL,
                              y=NULL,
                              just=NULL,
                              name='guide-box',
                              clip='on',
                              offset=c(0,0),
                              z=Inf,
                              plot=TRUE) {
  
  g_legend<-function(a.gplot){
    if (!gtable::is.gtable(a.gplot))
      a.gplot <- ggplotGrob(a.gplot)
    #gtable_filter(a.gplot, 'guide-box', fixed=TRUE)
    leg <- which(sapply(a.gplot$grobs, function(x) x$name) == "guide-box")
    a.gplot$grobs[[leg]]
  }
  
  # Work out positioning
  if (!is.null(position)) {
    if (length(offset) == 1) offset <- offset[c(1,1)]
    if (!grid::is.unit(offset)) offset <- grid::unit(offset, 'npc')
    
    position <- match.arg(position, c('bottom right','right bottom','bottom','bottom left','left bottom','left','top left','top','top right','left top','right top','right','center'))
    just <- switch(position,
                   'bottom right' = c(x=1, y=0),
                   'right bottom' = c(x=1, y=0),
                   'bottom' = c(x=0.5, y=0),
                   'bottom left' = c(x=0, y=0),
                   'left bottom' = c(x=0, y=0),
                   'left' = c(x=0, y=0.5),
                   'top left' = c(x=0, y=1),
                   'left top' = c(x=0, y=1),
                   'top' = c(x=0.5, y=1),
                   'top right' = c(x=1, y=1),
                   'right top' = c(x=1, y=1),
                   'right' = c(x=1, y=0.5),
                   'center' = c(x=0.5, y=0.5)
    )
    if (is.null(x)) x = unit(just[1], 'npc') + offset[1] *
      ifelse(grepl('right', position), -1, ifelse(grepl('left', position), 1, 0))
    if (is.null(y)) y = unit(just[2], 'npc') + offset[2] *
      ifelse(grepl('top', position), -1, ifelse(grepl('bottom', position), 1, 0))
  }
  if (!is.null(x) && !grid::is.unit(x)) x <- unit(x, 'npc')
  if (!is.null(y) && !grid::is.unit(y)) y <- unit(y, 'npc')
  
  if (is.null(x) | is.null(y) | is.null(just)) {
    stop('Please supply either `position`, or `x`, `y`, and `just` arguments.')
  }
  
  # Extract legends and gtable
  if (is.null(legend) & inherits(aplot, 'ggplot'))
    legend <- g_legend(aplot)
  
  if (is.null(legend) || inherits(legend, 'zeroGrob'))
    stop('No legend given in arguments, or could not extract legend from plot.')
  
  if (!inherits(aplot, 'gtable')) {
    aplot$theme$legend.position <- 'hidden'
    aplot <- ggplotGrob(aplot)
    #aplot <- ggplot_gtable(ggplot_build(aplot + theme(legend.position='hidden')))
  }
  
  # Update name if already found. No idea why this is necessary.
  if (any(grepl(name, aplot$layout$name))) {
    add <- gregexpr(paste0('(?!', name, '-?)([[:digit:]]+)'), aplot$layout$name, perl = TRUE)
    add <- regmatches(aplot$layout$name, add)
    add <- as.integer(unlist(add))
    if (length(add) > 0) {
      add <- max(add) + 1
    } else {
      add <- 1
    }
    name <- paste(name, add, sep='-')
  }
  
  legend$vp <- viewport(x=x, y=y, just=just,  width=sum(legend$widths), height=sum(legend$heights))
  legend$name <- name
  
  # Place legend *under* the lowest axis-line, if z is Inf
  if (is.infinite(z)) {
    axes <- grepl('axis', aplot$layout$name)
    .z <- min(aplot$layout$z[axes]) - 1
  } else {
    .z <- z
  }
  .clip <- clip
  
  if (is.character(panel)) {
    pn <- which(aplot$layout$name %in% panel)
    if (length(pn) == 0) stop('Could not find panel named `',panel,'`.')
    
    aplot <- with(aplot$layout[pn,],
                  gtable::gtable_add_grob(aplot,
                                  legend,
                                  t = min(t),
                                  l = min(l),
                                  r = max(r),
                                  b = max(b),
                                  z = .z,
                                  clip = .clip,
                                  name = legend$name
                  ))
  } else if ((is.numeric(panel) | is.integer(panel)) & length(panel) %in% c(2,4)) {
    panel <- rep(as.integer(panel), length.out=4)
    aplot <- gtable::gtable_add_grob(aplot,
                             legend,
                             t = panel[1],
                             l = panel[2],
                             b = panel[3],
                             r = panel[4],
                             z = .z,
                             clip = .clip,
                             name = legend$name
    )
  }
  
  if (plot) {
    grid::grid.newpage()
    grid::grid.draw(aplot)
  }
  
  invisible(aplot)
}

