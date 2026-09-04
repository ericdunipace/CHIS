#### Functions for analysis and variable creation ####
pooling <- function(chis_list) {
  require(dplyr)

  for (i in 1:length(chis_list)) {
    chis_list[[i]][, paste0("fnwgt", 0:80)] <- chis_list[[i]][, paste0(
      "rakedw",
      0:80
    )]

    chis_list[[i]] <-
      chis_list[[i]] %>%
      rename_at(
        vars(paste0("fnwgt", c(1:80))),
        ~ paste0("fnwgt", c(1:80) + 80 * (i - 1))
      )
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
    merged %>%
    mutate(across(starts_with("fnwgt"), ~ ifelse(is.na(.), fnwgt0, .)))

  merged <-
    merged %>%
    mutate(across(starts_with("fnwgt"), ~ . / length(chis_list)))

  capitalize_first_only <- function(x) {
    x <- tolower(x)
    sub("^(.)", "\\U\\1", x, perl = TRUE)
  }

  capitalize_each_word <- function(x) {
    sapply(strsplit(tolower(x), " "), function(words) {
      paste(
        toupper(substring(words, 1, 1)),
        substring(words, 2),
        sep = "",
        collapse = " "
      )
    })
  }

  fix_encoding <- function(x) {
    iconv(x, from = "latin1", to = "UTF-8", sub = "byte") # or sub = "" to drop bad characters
  }

  merged <- lapply(merged, function(col) {
    if (is.factor(col)) {
      fixed_levels <- fix_encoding(levels(col))
      levels(col) <- capitalize_each_word(fixed_levels)
    }
    col
  }) %>%
    as.data.frame()

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
    TRUE ~ NA_real_
  )
  out
}

create_K6_score <- function(x) {
  require(dplyr)
  x <- x %>%
    mutate(across(everything(), ~ K6_coding(.x))) %>%
    rowSums(na.rm = TRUE)
}

mean_svy_rep <- function(data, full_data, variable, by, ...) {
  require(dplyr)
  require(survey)

  out <- dplyr::tibble()

  if (nrow(data) > 0) {
    new_design <- svrepdesign(
      data = data,
      weights = ~fnwgt0,
      repweights = "fnwgt[1-9]",
      type = "other",
      scale = 1,
      rscales = 1,
      mse = TRUE
    )
  }

  if (
    !is.factor(full_data[[variable]]) && !is.character(full_data[[variable]])
  ) {
    # Calculate mean and standard deviation
    mean_value <- svymean(
      as.formula(paste0("~", variable)),
      design = new_design,
      na.rm = TRUE
    )
    se <- sqrt(attributes(mean_value)$var)
    sd_value <- sqrt(svyvar(
      as.formula(paste0("~", variable)),
      design = new_design,
      na.rm = TRUE
    ))

    out <- dplyr::tibble(
      mean = mean_value %>% as.numeric(),
      se = se %>% as.numeric(),
      sd = sd_value
    )
  } else if (
    is.factor(full_data[[variable]]) || is.character(full_data[[variable]])
  ) {
    if (nrow(data) == 0) {
      return(dplyr::tibble(
        N = 0,
        p = 0
      ))
    }

    full_design <- svrepdesign(
      data = full_data,
      weights = ~fnwgt0,
      repweights = "fnwgt[1-9]",
      type = "other",
      scale = 1,
      rscales = 1,
      mse = TRUE
    )

    new_design$variables$ones <- 1
    full_design$variables$ones <- 1

    N <- svytotal(~ones, design = full_design, na.rm = TRUE) %>% as.numeric()

    n_value <- svytotal(~ones, design = new_design, na.rm = TRUE) %>%
      as.numeric()

    # Calculate proportions
    prop_value <- n_value / N

    out <- dplyr::tibble(
      N = format(n_value, big.mark = ','),
      p = prop_value * 100
    )
  } else {
    stop(sprintf(
      "Invalid type specified: '%s'. Use 'categorical', 'dichotomous', or 'continuous'.",
      full_data[[variable]] %>% typeof()
    ))
  }

  return(out)
}

lmer.svyrep.design <- function(
  formula,
  design = NULL,
  data = NULL,
  get.coef = FALSE,
  fast_reps = TRUE, # cheaper settings for replicates
  control_base = lme4::lmerControl(
    optimizer = "nloptwrap",
    calc.derivs = FALSE,
    check.conv.singular = "ignore",
    check.conv.grad = "ignore"
  ),
  control_rep = lme4::lmerControl(
    optimizer = "nloptwrap",
    calc.derivs = FALSE,
    check.conv.singular = "ignore",
    check.conv.grad = "ignore",
    optCtrl = list(maxfun = 1e4) # trim tail work
  ),
  ...
) {
  # ---- prep ----
  dat <- design$variables
  w0 <- as.numeric(design$pweights)
  Wrep <- as.data.frame(design$repweights) # columns = replicates
  n <- nrow(dat)

  rscales <- as.numeric(design$rscales)
  scale <- as.numeric(design$scale)

  # normalize once (columns sum to n)
  Wrep <- sweep(Wrep, 2, colSums(Wrep), "/")
  dat$.weights <- w0 / sum(w0)

  # ---- baseline fit ----
  if (isTRUE(verbose)) {
    message("Running baseline model…")
  }
  fit <- lme4::lmer(
    formula = formula,
    data = dat,
    weights = .weights,
    REML = TRUE,
    control = control_base,
    ...
  )
  fit@call$data <- substitute(design$variables)
  fit@call$weights <- substitute(design$pweights)

  # warm starts
  theta0 <- lme4::getME(fit, "theta")
  beta0 <- lme4::getME(fit, "fixef")

  # ---- replicate fits (sequential, no update()) ----
  if (isTRUE(verbose)) {
    message("Running replicate weights (sequential)…")
  }
  rep_names <- names(Wrep)

  # We'll reuse the same data object and just overwrite a single column
  dat$.wrep <- dat$.weights

  rep_fit <- lapply(rep_names, function(rw) {
    dat$.wrep <- Wrep[[rw]] # swap weights in place
    lme4::lmer(
      formula = formula,
      data = dat,
      weights = .wrep, # evaluate to the column we just wrote
      REML = TRUE,
      start = list(theta = theta0, fixef = beta0), # warm start from baseline
      control = if (isTRUE(fast_reps)) control_rep else control_base,
      ...
    )
  })

  # Extract the coefficients and standard errors
  fe <- lme4::fixef(fit)
  rep_fe <- sapply(rep_fit, lme4::fixef)

  re <- lme4::ranef(fit)
  rep_re <- lapply(rep_fit, lme4::ranef)

  # Calculate the variance-covariance matrix
  vcov_fe <- tcrossprod(sweep(rep_fe, 1, fe, "-") * sqrt(rscales)) * scale

  # Calculate the variance-covariance matrix for random effects
  vcov_re <- vector("list", length(re))
  names(vcov_re) <- names(re)

  for (i in seq_along(re)) {
    for (j in seq_along(re[[i]])) {
      r_mat <- as.matrix(re[[i]][[j]])
      vcov_re[[i]][[j]] <- tcrossprod(sapply(rep_re, function(rr) {
        sweep(rr[[i]][[j]] %>% as.matrix(), 1, r_mat, "-") * sqrt(rscales)
      })) *
        scale
      rownames(vcov_re[[i]][[j]]) <- colnames(vcov_re[[i]][[
        j
      ]]) <- rownames(re[[i]][[j]])
      attr(re[[i]], "postVar")[, j, ] <- diag(vcov_re[[i]][[j]])
    }
  }

  gnms <- function(x) {
    c(outer(colnames(x), rownames(x), function(x, y) paste(y, x, sep = ".")))
  }
  rnms <- lapply(re, gnms)
  re_nms <- unlist(Map(function(n, r) paste(n, r, sep = "."), names(re), rnms))
  fix_nms <- names(fe)
  all_nms <- unname(c(re_nms, fix_nms))

  rep_param <- lapply(rep_fit, function(rf) {
    c(unlist(ranef(rf)), fixef(rf))
  })
  param <- c(unlist(re), fe)
  combined_vcov <- tcrossprod(sapply(rep_param, "-", param) * sqrt(rscales)) *
    scale
  dimnames(combined_vcov) <- list(all_nms, all_nms)

  out <- list(
    fit = fit,
    fe = fe,
    re = re,
    vcov = list(fe = vcov_fe, re = vcov_re, combined = combined_vcov)
  )

  class(out) <- "svyrep_merMod"
  return(out)
}

glmer.svyrep.design <- function(
  formula,
  design = NULL,
  data = NULL,
  get.coef = FALSE,
  verbose = FALSE,
  fast_reps = TRUE,
  control_base = lme4::glmerControl(
    optimizer = "nloptwrap",
    calc.derivs = FALSE,
    check.conv.singular = "ignore",
    check.conv.grad = "ignore"
  ),
  control_rep = lme4::glmerControl(
    optimizer = "nloptwrap",
    calc.derivs = FALSE,
    check.conv.singular = "ignore",
    check.conv.grad = "ignore",
    optCtrl = list(maxfun = 1e4)
  ),
  ...
) {
  # capture user ... once so we can reuse/merge safely
  dot_args <- list(...)

  # ---- prep ----
  dat <- design$variables
  w0 <- as.numeric(design$pweights)
  Wrep <- as.data.frame(design$repweights) # columns = replicates

  n <- nrow(dat)
  rscales <- as.numeric(design$rscales)
  scale <- as.numeric(design$scale)

  # normalize once (column-wise) to sum to n
  Wrep <- sweep(Wrep, 2, colSums(Wrep), "/") * n
  dat$.weights <- w0 / sum(w0) * n

  ## ---------- baseline fit ----------
  base_args <- c(
    list(
      formula = formula,
      data = dat,
      weights = dat$.weights,
      control = control_base
    ),
    dot_args
  )

  # Ensure a proper family object (e.g., binomial()), not "binomial"
  if (!"family" %in% names(base_args)) {
    stop("Pass a GLM family, e.g. family = binomial()")
  }
  if (is.character(base_args$family)) {
    base_args$family <- match.fun(base_args$family)()
  }

  if (isTRUE(verbose)) {
    message("Running model")
  }
  fit <- do.call(lme4::glmer, base_args)
  fit@call$data <- substitute(design$variables)
  fit@call$weights <- substitute(design$pweights)

  # extract warm starts
  theta0 <- lme4::getME(fit, "theta")
  beta0 <- lme4::getME(fit, "fixef")

  ## ---------- replicate fits (sequential) ----------
  rep_names <- names(Wrep)

  # extra args for replicates (optionally nAGQ=0 if user didn't set it)
  rep_dot_args <- dot_args
  if (isTRUE(fast_reps) && !"nAGQ" %in% names(rep_dot_args)) {
    rep_dot_args$nAGQ <- 0L
  }

  if (isTRUE(verbose)) {
    message("Running replicate weights. Please be patient.")
  }
  rep_fit <- lapply(rep_names, function(rw) {
    rep_args <- c(
      list(
        formula = formula,
        data = dat,
        weights = Wrep[[rw]],
        start = list(theta = theta0),
        control = control_rep
      ),
      rep_dot_args
    )
    do.call(lme4::glmer, rep_args)
  })

  if (isTRUE(verbose)) {
    message("Calculating variance-covariance matrices.")
  }

  # Extract the coefficients and standard errors
  fe <- lme4::fixef(fit)
  rep_fe <- sapply(rep_fit, fixef) %>% matrix(ncol = length(rep_fit))

  re <- lme4::ranef(fit)
  rep_re <- lapply(rep_fit, lme4::ranef)

  # Calculate the variance-covariance matrix
  vcov_fe <- tcrossprod(sweep(rep_fe, 1, fe, "-") * sqrt(rscales)) * scale

  # Calculate the variance-covariance matrix for random effects
  vcov_re <- vector("list", length(re))
  names(vcov_re) <- names(re)

  for (i in seq_along(re)) {
    for (j in seq_along(re[[i]])) {
      r_mat <- as.matrix(re[[i]][[j]])
      vcov_re[[i]][[j]] <- tcrossprod(sapply(rep_re, function(rr) {
        sweep(rr[[i]][[j]] %>% as.matrix(), 1, r_mat, "-") * sqrt(rscales)
      })) *
        scale
      rownames(vcov_re[[i]][[j]]) <- colnames(vcov_re[[i]][[
        j
      ]]) <- rownames(re[[i]][[j]])
      attr(re[[i]], "postVar")[, j, ] <- diag(vcov_re[[i]][[j]])
    }
  }

  gnms <- function(x) {
    c(outer(colnames(x), rownames(x), function(x, y) paste(y, x, sep = ".")))
  }
  rnms <- lapply(re, gnms)
  re_nms <- unlist(Map(function(n, r) paste(n, r, sep = "."), names(re), rnms))
  fix_nms <- names(fe)
  all_nms <- unname(c(re_nms, fix_nms))

  rep_param <- lapply(rep_fit, function(rf) {
    c(unlist(ranef(rf)), fixef(rf))
  })
  param <- c(unlist(re), fe)
  combined_vcov <- tcrossprod(sapply(rep_param, "-", param) * sqrt(rscales)) *
    scale
  dimnames(combined_vcov) <- list(all_nms, all_nms)

  param_list <- NULL

  if (isTRUE(get.coef)) {
    names(param) <- all_nms
    rep_param <- lapply(rep_param, function(rp) {
      names(rp) <- all_nms
      rp
    })
    param_list <- list(param = param, rep_param = rep_param)
  }

  out <- list(
    fit = fit,
    fe = fe,
    re = re,
    vcov = list(fe = vcov_fe, re = vcov_re, combined = combined_vcov),
    param = param_list
  )

  class(out) <- "svyrep_merMod"
  return(out)
}

summary.svyrep_merMod <- function(object, ...) {
  holder <- summary(object$fit)

  holder$vcov@x <- c(object$vcov$fe)
  holder$coefficients[, "Std. Error"] <- object$vcov$fe %>% diag() %>% sqrt()
  holder$coefficients[, 3] <- holder$coefficients[, "Estimate"] /
    holder$coefficients[, "Std. Error"]

  if (ncol(holder$coefficients) > 3) {
    holder$coefficients[, 4] <- 2 * pnorm(-abs(holder$coefficients[, 3]))
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
  if (full) {
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
  y <- object$fit@resp$y
  muhat <- object$fit@resp$mu
  V0 <- vcov(object$fit, sigma = 1, full = TRUE)
  V <- vcov(object, sigma = 1, full = TRUE)

  w <- weights(object$fit)
  w <- w / sum(w) * nobs(object$fit)
  Nhat <- sum(w) #* nobs(object$fit)

  sigma2hat <- sum((y - muhat)^2 * w) / Nhat
  minus2ellhat <- -2 * logLik(refitML(object$fit)) # This is the log-likelihood of the model
  # Nhat * log(sigma2hat) + Nhat + Nhat * log(2 * pi)

  Delta_mu <- solve(V0 * sigma2hat, V)
  Isigma2 <- Nhat / (2 * sigma2hat^2)
  Usigma2 <- -1 / (2 * sigma2hat) + (y - muhat)^2 / (2 * sigma2hat^2)
  Hsigma2 <- sum(w * Usigma2^2)

  Deltasigma2 <- Isigma2 / Hsigma2
  deltabar <- mean(c(diag(Delta_mu), Deltasigma2))
  eff.p <- sum(diag(Delta_mu)) + Deltasigma2
  aic <- minus2ellhat + k * eff.p

  c(eff.p = eff.p, AIC = aic, deltabar = deltabar)
}

AIC.svyrep_merMod <- function(object, k = 2, ...) {
  aic <- extractAIC(object, k = k, ...)
  aic["AIC"]
}

prism_to_map <- function(
  var,
  sf,
  years,
  admin.level,
  heatwave = FALSE,
  cutoff = 32
) {
  stopifnot("'var' must be a single character" = (length(var) == 1))
  stopifnot(
    "'var' must be in  \"ppt\", \"tmean\", \"tmin\", \"tmax\", \"vpdmin\", \"vpdmax\", \"tdmean\"." = var %in%
      c("tmean", "tmax", "tmin", "ppt", "vpdmin", "vpdmax", "tdmean")
  )
  admin.level <- match.arg(admin.level, c("county", "census")) %>%
    switch(
      "county" = "county",
      "zip" = "bestzip",
      "census" = "tract10"
    )

  files_daily <- prism::prism_archive_subset(var, "daily")
  # Within each folder, find the .bil files
  bil_paths <- unlist(lapply(
    file.path(prism::prism_get_dl_dir(), files_daily),
    function(f) {
      list.files(f, pattern = "\\.bil$", full.names = TRUE)
    }
  ))
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
    layer_dates <- as.Date(
      stringr::str_extract(basename(files_daily), "[0-9]{8}"),
      "%Y%m%d"
    )
    # Then inside tapp(), subset `vals` to only those cells:
    year_factors <- factor(format(layer_dates, "%Y"))

    years_all <- unique(year_factors)
    # years_all    <- years_all[!(as.numeric(as.character(years_all)) %in% unique_years)] %>%
    #   droplevels()

    years_search <- as.character(years_all[
      as.numeric(as.character(years_all)) %in% unique_years
    ])

    annual_files <- vector("list", length(years_search))
    names(annual_files) <- years_search

    for (yr in years_search) {
      outfile <- paste0(glue("{admin.level}_above{cutoff}_{yr}.tiff"))
      annual_files[[as.character(yr)]] <- outfile

      if (!file.exists(outfile)) {
        idxs <- which(as.character(year_factors) == yr)
        year_bil <- bil_paths[idxs]
        year_stack <- terra::rast(year_bil) %>% terra::project(cal_admin)
        year_crop <- terra::mask(terra::crop(year_stack, cal_admin), cal_admin)
        year_count <- terra::app(year_crop, fun = function(v) {
          sum(v > cutoff, na.rm = TRUE)
        })

        terra::writeRaster(
          year_count,
          filename = outfile,
          overwrite = TRUE,
          wopt = list(datatype = "INT1U", gdal = c("COMPRESS=LZW"))
        )
        rm(year_stack, year_count)
        gc()
      }
    }

    # Build a multi-layer from annual files
    annual_counts <- terra::rast(unlist(annual_files))
    names(annual_counts) <- unique_years

    # Zonal sum for each annual layer
    zonal_df <- terra::zonal(
      annual_counts,
      cal_admin,
      fun = "mean",
      na.rm = TRUE
    )

    zonal_df[[admin.level]] <- rownames(zonal_df) <- sf[[admin.level]]

    # tzb <- as_tibble(zonal_df)
    zonal_long <- zonal_df %>%
      tidyr::pivot_longer(
        cols = -all_of(admin.level),
        names_to = "year",
        values_to = paste0(glue("days_above{cutoff}"))
      )

    file.remove(unlist(annual_files)) # remove the annual files

    return(
      zonal_long %>%
        left_join(y = sf, by = admin.level) %>%
        st_as_sf()
    )
  }

  # Precompute 20-year climatology rasters for each unique_year
  clim_rasters <- vector("list", ny)
  names(clim_rasters) <- as.character(unique_years)
  for (i in seq_along(unique_years)) {
    y <- unique_years[i]
    # Download (or locate) annual files for those years
    ann_stack_r <- prism::pd_stack(prism::prism_archive_subset(
      var,
      "annual",
      years = y
    ))
    ann_stack_t <- terra::rast(ann_stack_r) %>% terra::project(cal_admin)
    ann_mask_ca <- terra::mask(terra::crop(ann_stack_t, cal_admin), cal_admin)
    clim_rasters[[i]] <- terra::app(ann_mask_ca, mean, na.rm = TRUE)
  }

  zonal <- dplyr::bind_rows(
    lapply(seq_along(unique_years), function(i) {
      sf %>%
        mutate(year = unique_years[[i]], {{ var }} := clim_rasters[[i]][, 2])
    })
  )

  return(zonal)
}

data_var_from_prism <- function(
  data,
  var,
  sf,
  admin.level = "county",
  cutoff = 32
) {
  stopifnot("'var' must be a single character" = (length(var) == 1))
  stopifnot(
    "'var' must be in  \"ppt\", \"tmean\", \"tmin\", \"tmax\", \"vpdmin\", \"vpdmax\", \"tdmean\"." = var %in%
      c("tmean", "tmax", "tmin", "ppt", "vpdmin", "vpdmax", "tdmean")
  )
  admin.level <- match.arg(admin.level, c("county", "census")) %>%
    switch(
      "county" = "county",
      "zip" = "bestzip",
      "census" = "tract10"
    )

  # Get all daily file paths and their dates for 'var'
  files_daily <- prism::prism_archive_subset(var, "daily")
  # Within each folder, find the .bil files
  bil_paths <- unlist(lapply(
    file.path(prism::prism_get_dl_dir(), files_daily),
    function(f) {
      list.files(f, pattern = "\\.bil$", full.names = TRUE)
    }
  ))
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
  for (i in seq_along(unique_years)) {
    y <- unique_years[i]
    years_vec <- (y - 16L):(y - 2L)
    # Download (or locate) annual files for those years
    ann_stack_r <- prism::pd_stack(prism::prism_archive_subset(
      var,
      "annual",
      years = years_vec
    ))
    ann_stack_t <- terra::rast(ann_stack_r) %>% terra::project(cal_admin)
    ann_stack_ca <- terra::mask(terra::crop(ann_stack_t, cal_admin), cal_admin)
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
    layer_dates <- as.Date(
      stringr::str_extract(basename(files_daily), "[0-9]{8}"),
      "%Y%m%d"
    )
    # Then inside tapp(), subset `vals` to only those cells:
    year_factors <- factor(format(layer_dates, "%Y"))

    years_all <- unique(year_factors)
    # years_all    <- years_all[!(as.numeric(as.character(years_all)) %in% unique_years)] %>%
    #   droplevels()
    annual_files <- vector("list", length(years_all))
    names(annual_files) <- years_all

    for (yr in years_all) {
      outfile <- paste0(glue("{var}_{admin.level}_above{cutoff}_{yr}.tiff"))
      annual_files[[as.character(yr)]] <- outfile

      if (!file.exists(outfile)) {
        idxs <- which(year_factors == yr)
        year_bil <- bil_paths[idxs]
        year_stack <- terra::rast(year_bil) %>% terra::project(cal_admin)
        year_ca <- terra::mask(terra::crop(year_stack, cal_admin), cal_admin)
        year_count <- terra::app(year_ca, fun = function(v) {
          sum(v > cutoff, na.rm = TRUE)
        })

        terra::writeRaster(
          year_count,
          filename = outfile,
          overwrite = TRUE,
          wopt = list(datatype = "INT2U", gdal = c("COMPRESS=LZW"))
        )
        rm(year_stack, year_count)
        gc()
      }
    }

    # Build a multi-layer from annual files
    annual_counts <- terra::rast(unlist(annual_files))
    names(annual_counts) <- years_all

    # project cal_admin vector to annual_counts raster
    # cal_proj <- terra::project(cal_admin, annual_counts[[1]])

    # Zonal sum for each annual layer
    zonal_df <- terra::zonal(
      annual_counts,
      cal_admin,
      fun = "mean",
      na.rm = TRUE
    )

    zonal_df[[admin.level]] <- rownames(zonal_df) <- sf[[admin.level]]

    # tzb <- as_tibble(zonal_df)
    zonal_long <- zonal_df %>%
      tidyr::pivot_longer(
        cols = -all_of(admin.level),
        names_to = "year",
        values_to = paste0(glue("days_above{cutoff}"))
      )
    admin_avg_days_cutoff <- zonal_long %>%
      group_by(!!sym(glue("{admin.level}"))) %>%
      summarize(
        "2021" := mean(
          .data[[glue("days_above{cutoff}")]][year >= 2005 & year < 2020],
          na.rm = TRUE
        ),
        "2022" := mean(
          .data[[glue("days_above{cutoff}")]][year >= 2006 & year < 2021],
          na.rm = TRUE
        ),
        "2023" := mean(
          .data[[glue("days_above{cutoff}")]][year >= 2007 & year < 2022],
          na.rm = TRUE
        )
      ) %>%
      ungroup() %>%
      # mutate({{admin.level}} :=  sorted_admin_names) %>%
      tidyr::pivot_longer(
        cols = -all_of(admin.level),
        names_to = "year",
        values_to = paste0(glue("mean_days_above{cutoff}"))
      )
  }

  # get unique month year combos
  my_unique <- paste(data$survey_years, data$survey_month, sep = "-") %>%
    unique()

  # Create dynamic results for each unique month-year combination
  results <- purrr::map_dfr(my_unique, function(d) {
    # Parse the month-year string into a Date object
    sd <- lubridate::ym(d) + lubridate::days(14)
    # Get the year and month from the Date object
    sy <- lubridate::year(sd)
    sm <- lubridate::month(sd)

    # Prior month window
    pm_date <- sd
    pm_start <- pm_date %m-% lubridate::days(90)
    pm_end <- pm_date %m-% lubridate::days(1)

    # 1-year prior window
    prior_start <- sd %m-% lubridate::years(1)
    prior_end <- sd %m-% lubridate::days(1)

    # Lagged 1-year window
    lag_start <- sd %m-% lubridate::years(2)
    lag_end <- sd %m-% lubridate::years(1) %m-% lubridate::days(1)

    # 3-month prior daily subset
    date_names <- as.Date(names(var_daily))
    sel_pm <- bil_paths[which(date_names >= pm_start & date_names <= pm_end)]
    if (length(sel_pm) > 0) {
      stack_pm <- terra::rast(sel_pm) %>%
        terra::project(cal_admin) %>%
        terra::crop(cal_admin) %>%
        terra::mask(cal_admin)
      # stack_pm <- prism::prism_archive_subset(var, "daily", minDate = pm_start, maxDate = pm_end) %>%
      #   prism::pd_stack() %>%
      #   terra::rast() %>% terra::project(cal_admin) %>%
      #   terra::crop(cal_admin) %>%
      #   terra::mask(cal_admin)

      pm_means <- terra::app(stack_pm, mean, na.rm = TRUE)
      pm_mean <- terra::extract(
        pm_means,
        cal_admin,
        fun = mean,
        na.rm = TRUE
      ) %>%
        .[, 2]

      stack_pm_normal_list <- lapply(2:16, function(yy) {
        pmn_idx <- which(
          dates_daily >= (pm_start %m-% lubridate::years(yy)) &
            dates_daily <= (pm_end %m-% lubridate::years(yy))
        )
        pmn_bil <- bil_paths[pmn_idx]
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
          .[, 2]
      }) %>%
        bind_cols() %>%
        rowMeans(na.rm = TRUE)

      if (var == "tmax") {
        # Count days above cutoff
        pm_count <- terra::app(
          stack_pm,
          function(x, ...) sum(x > cutoff, ...),
          na.rm = TRUE
        ) %>%
          terra::extract(cal_admin, fun = mean, na.rm = TRUE) %>%
          .[, 2]

        pm_count_normal <- lapply(stack_pm_normal_list, function(d) {
          terra::app(d, function(x, ...) sum(x > cutoff, ...), na.rm = TRUE) %>%
            terra::extract(cal_admin, fun = mean, na.rm = TRUE) %>%
            .[, 2]
        }) %>%
          bind_cols() %>%
          rowMeans(na.rm = TRUE)
      } else {
        pm_count <- pm_count_normal <- NA_integer_
      }
    } else {
      pm_mean <- pm_mean_normal <- NA_real_
      pm_count <- pm_count_normal <- NA_integer_
    }

    # 1-year prior daily subset
    sel_prior <- bil_paths[which(
      date_names >= prior_start & date_names <= prior_end
    )]
    if (length(sel_prior) > 0) {
      # Get the bil files for the prior year
      stack_prior <- terra::rast(sel_prior) %>%
        terra::project(cal_admin) %>%
        terra::crop(cal_admin) %>%
        terra::mask(cal_admin)

      yr_prior_means <- terra::app(stack_prior, fun = "mean", na.rm = TRUE)
      yr_prior_mean <- terra::extract(
        yr_prior_means,
        cal_admin,
        fun = mean,
        na.rm = TRUE
      ) %>%
        .[, 2]
      if (var == "tmax") {
        yr_prior_count <- terra::app(
          stack_prior,
          function(x, ...) sum(x > cutoff, ...),
          na.rm = TRUE
        ) %>%
          terra::extract(cal_admin, fun = mean, na.rm = TRUE) %>%
          .[, 2]
      } else {
        yr_prior_count <- NA_integer_
      }
    } else {
      yr_prior_mean <- NA_real_
      yr_prior_count <- NA_integer_
    }

    # Lagged 1-year daily subset
    sel_lag <- bil_paths[which(date_names >= lag_start & date_names <= lag_end)]
    if (length(sel_lag) > 0) {
      stack_lag <- terra::rast(sel_lag) %>%
        terra::project(cal_admin) %>%
        terra::crop(cal_admin) %>%
        terra::mask(cal_admin)

      lag_means <- terra::app(stack_lag, fun = mean, na.rm = TRUE)
      lag_mean <- terra::extract(
        lag_means,
        cal_admin,
        fun = mean,
        na.rm = TRUE
      ) %>%
        .[, 2]
      if (var == "tmax") {
        yr_lag_count <- terra::app(
          stack_lag,
          function(x, ...) sum(x > cutoff, ...),
          na.rm = TRUE
        ) %>%
          terra::extract(cal_admin, fun = mean, na.rm = TRUE) %>%
          .[, 2]
      } else {
        yr_lag_count <- NA_integer_
      }
    } else {
      lag_mean <- NA_real_
    }

    # Climatology raster for this survey_year
    clim_ras <- clim_rasters[[as.character(sy)]]
    clim_val <- terra::app(
      clim_ras %>%
        terra::crop(cal_admin) %>%
        terra::mask(cal_admin),
      fun = "mean",
      na.rm = TRUE
    ) %>%
      terra::extract(cal_admin, fun = mean, na.rm = TRUE) %>%
      .[, 2]

    tibble(
      survey_dates = sd,
      survey_months = sm,
      survey_years = sy,
      !!glue({
        admin.level
      }) := sf[[admin.level]],
      !!paste0(var, "_", admin.level, "_prior_90_days_mean") := pm_mean,
      !!paste0(
        var,
        "_",
        admin.level,
        "_prior_90_days_mean_normal"
      ) := pm_mean_normal,
      !!paste0(
        var,
        "_",
        admin.level,
        glue("_prior_90_days_count{cutoff}")
      ) := pm_count,
      !!paste0(
        var,
        "_",
        admin.level,
        glue("_prior_90_days_count{cutoff}_normal")
      ) := pm_count_normal,
      !!paste0(var, "_", admin.level, "_prior_yr_mean") := yr_prior_mean,
      !!paste0(
        var,
        "_",
        admin.level,
        glue("_prior_yr_count{cutoff}")
      ) := yr_prior_count,
      !!paste0(var, "_", admin.level, "_lag_yr_mean") := lag_mean,
      !!paste0(
        var,
        "_",
        admin.level,
        glue("_lag_yr_count{cutoff}")
      ) := yr_lag_count,
      !!paste0(var, "_", admin.level, "_15yr_normal") := clim_val
    )
  })

  # Bind dynamic results back to original sf (dropping geometry column)
  data_out <- data %>%
    left_join(
      results,
      by = c("survey_dates", "survey_months", "survey_years", admin.level)
    ) %>%
    mutate(
      across(
        starts_with(var) & ends_with("yr_mean"),
        ~ . - !!sym(paste0(var, "_", admin.level, "_15yr_normal")),
        .names = "{.col}_delta"
      ),
      across(
        starts_with(var) & ends_with("_90_days_mean"),
        ~ . -
          !!sym(paste0(var, "_", admin.level, "_prior_90_days_mean_normal")),
        .names = "{.col}_delta"
      )
    )

  if (var == "tmax") {
    data_out <- data_out %>%
      left_join(
        admin_avg_days_cutoff %>% mutate(year = as.numeric(year)),
        by = c(admin.level, "year")
      ) %>%
      mutate(
        !!glue(
          "tmax_{admin.level}_prior_90_days_count{cutoff}_delta"
        ) := .data[[glue("tmax_{admin.level}_prior_90_days_count{cutoff}")]] -
          .data[[glue(
            "tmax_{admin.level}_prior_90_days_count{cutoff}_normal"
          )]],
        !!glue(
          "tmax_{admin.level}_prior_yr_count{cutoff}_delta"
        ) := .data[[glue("tmax_{admin.level}_prior_yr_count{cutoff}")]] -
          .data[[glue("mean_days_above{cutoff}")]],
        !!glue("tmax_{admin.level}_lag_yr_count{cutoff}_delta") := .data[[glue(
          "tmax_{admin.level}_lag_yr_count{cutoff}"
        )]] -
          .data[[glue("mean_days_above{cutoff}")]]
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
      age_group = factor(ifelse(
        srage_p %in% c("12", "13", "14"),
        "12-14",
        "15-17"
      )),
      cont_age = as.numeric(as.character(srage_p)),
      ombsrtn_p1 = forcats::fct_recode(
        ombsrtn_p1,
        "Hispanic" = "Hispanic",
        "White, Non-Hispanic" = "White, Non-hispanic (nh)",
        "Asian, Non-Hispanic" = "Asian Only, Nh",
        "Two or More Races, Non-Hispanic" = "Two Or More Races, Nh"
      ),
      povll_binary = forcats::fct_recode(
        povll,
        "Less than 300% FPL" = "0-99% Fpl",
        "Less than 300% FPL" = "100-199% Fpl",
        "Less than 300% FPL" = "200-299% Fpl",
        "300% FPL And Above" = "300% Fpl And Above"
      ),
      povll = forcats::fct_recode(
        povll,
        "0-99% FPL" = "0-99% Fpl",
        "100-199% FPL" = "100-199% Fpl",
        "200-299% FPL" = "200-299% Fpl",
        "300% FPL And Above" = "300% Fpl And Above"
      ),
      ahedtc_binary = forcats::fct_recode(
        ahedtc_p1,
        "No college" = "No Formal Education Or Grade 1-8",
        "No college" = "Grade 9-11",
        "No college" = "Grade 12/h.s. Diploma",
        "College or more" = "Some College",
        "College or more" = "Aa/as Degree Or Vocational School",
        "College or more" = "Ba Or Bs Degree/some Grad School",
        "College or more" = "Ma Or Ms Degree",
        "College or more" = "Ph.d. Or Equivalent"
      ),
      ahedtc_p1 = forcats::fct_recode(
        ahedtc_p1,
        "Grade 12/H.S. Diploma" = "Grade 12/h.s. Diploma",
        "AA/AS Degree Or Vocational School" = "Aa/as Degree Or Vocational School",
        "BA Or BS Degree/Some Grad School" = "Ba Or Bs Degree/some Grad School",
        "MA Or MS Degree" = "Ma Or Ms Degree",
        "Ph.D. Or Equivalent" = "Ph.d. Or Equivalent"
      ),
      lnghmt_binary = forcats::fct_recode(
        lnghmt_p1,
        "English" = "English & Spanish",
        "English" = "English And One Other Language",
        "Non-English" = "Spanish Or Other One Language Only",
        "Non-English" = "Other Languages (2+)"
      ),
      # lnghmt_binary = forcats::fct_recode(
      #   langhome,
      #   "English" = "English & Chinese",
      #   "English" = "English & Spanish",
      #   "English" = "English & One Other Language",
      #   "English" = "English & European Language",
      #   "English" = "English & Another Asian Language",
      #   "Non-English" = "Spanish",
      #   "Non-English" = "Chinese",
      #   "Non-English" = "Korean",
      #   "Non-English" = "Vietnamese",
      #   "Non-English" = "Other Languages (2+)",
      #   "Non-English" = "Other Asian Language (1 Only)",
      #   "Non-English" = "Other Non-asian Language (1 Only)"
      # ),
      uninsured = factor(ifelse(
        as.character(instype) == "Uninsured",
        "Yes",
        "No"
      )),
      health_office = forcats::fct_recode(
        tf2,
        "Primary care office" = "Doctor's Office/kaiser/hmo",
        "Primary care office" = "Clinic/health Center/hospital Clinic",
        "Primary care office" = "Other Health Professional (not Md)/alternative Medicine",
        "Not primary care office" = "Inapplicable",
        "Not primary care office" = "Emergency Room",
        "Not primary care office" = "Family Members/friends Residence/who Are Health Professionals",
        "Not primary care office" = "Some Other Place",
        "Not primary care office" = "No One Place"
      ),
      school_last_week = case_when(
        ta4 == "Yes" ~ "Yes",
        ta4 == "Home Schooled" ~ "Yes",
        ta4 == "No" ~ "No",
        ta4 == "On Vacation" ~ "No",
        ta4 %in% c("Refused", "Don't know") ~ NA_character_
      ),
      health_missed_days = dplyr::case_when(
        school_last_week == "Yes" & tb4 >= 0 ~ as.numeric(tb4),
        school_last_week == "Yes" & tb4 %in% c(-7, -8) ~ NA_real_,
        school_last_week == "No" ~ 0,
        TRUE ~ NA_real_
      ),
      tl53 = forcats::fct_rev(tl53),
      tl25_pos = case_when(
        tl25 == "Somewhat Agree" ~ "Yes",
        tl25 == "Strongly Agree" ~ "Yes",
        TRUE ~ "No"
      ),
      tl27_pos = case_when(
        tl27 == "Somewhat Agree" ~ "Yes",
        tl27 == "Strongly Agree" ~ "Yes",
        TRUE ~ "No"
      ),
      tl53_pos = case_when(
        tl53 == "Definitely Could" ~ "Yes",
        tl53 == "Probably Could" ~ "Yes",
        tl53 == "Maybe Could" ~ "Yes",
        TRUE ~ "No"
      ),
      tq10_pos = case_when(
        tq10 == "All Of The Time" ~ "Yes",
        tq10 == "Most Of The Time" ~ "Yes",
        tq10 == "Some Of The Time" ~ "Yes",
        TRUE ~ "No"
      ),
      tq11_pos = case_when(
        tq11 == "All Of The Time" ~ "Yes",
        tq11 == "Most Of The Time" ~ "Yes",
        tq11 == "Some Of The Time" ~ "Yes",
        TRUE ~ "No"
      ),
      tq14_pos = case_when(
        tq14 == "All Of The Time" ~ "Yes",
        tq14 == "Most Of The Time" ~ "Yes",
        tq14 == "Some Of The Time" ~ "Yes",
        TRUE ~ "No"
      ),
      tq15_pos = case_when(
        tq15 == "All Of The Time" ~ "Yes",
        tq15 == "Most Of The Time" ~ "Yes",
        tq15 == "Some Of The Time" ~ "Yes",
        TRUE ~ "No"
      ),
      tq16_pos = case_when(
        tq16 == "All Of The Time" ~ "Yes",
        tq16 == "Most Of The Time" ~ "Yes",
        tq16 == "Some Of The Time" ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    mutate(
      survey_dates = parse_my_to_date(tadate_mm),
      survey_years = lubridate::year(survey_dates),
      survey_months = lubridate::month(survey_dates)
    )
}

chis_clean <- function(data) {
  pooling(data) %>%
    mutate(
      age_cat = factor(as.character(srage)),
      age_group = factor(ifelse(
        srage %in% c("12", "13", "14"),
        "12-14",
        "15-17"
      )),
      cont_age = as.numeric(as.character(srage)),
      sch_typ = forcats::fct_recode(
        as.factor(sch_typ),
        "Other" = "Other",
        "Other" = "Inapplicable"
        # c(`NOT ASCERTAINED` = -9, `DON'T KNOW` = -8, REFUSED = -7,
        #   INAPPLICABLE = -1, `PUBLIC SCHOOL` = 1, `PRIVATE SCHOOL` = 2,
        #   OTHER = 3)
      ),
      ombsrreo = forcats::fct_recode(
        ombsrreo,
        "Hispanic" = "Hispanic",
        "White" = "White, Non-hispanic (nh)",
        "Asian/Pacific Islander" = "Asian Only, Nh",
        "African American" = "African American Only, Not Hispanic",
        "Multiracial and Other" = "American Indian/alaska Native Only, Nh",
        "Asian/Pacific Islander" = "Native Hawaiian/pacific Islander, Nh",
        "Multiracial and Other" = "Two Or More Races, Nh"
      ),
      povll_binary = forcats::fct_recode(
        povll,
        "Less than 300% FPL" = "0-99% Fpl",
        "Less than 300% FPL" = "100-199% Fpl",
        "Less than 300% FPL" = "200-299% Fpl",
        "300% FPL And Above" = "300% Fpl And Above"
      ),
      povll = forcats::fct_recode(
        povll,
        "0-99% FPL" = "0-99% Fpl",
        "100-199% FPL" = "100-199% Fpl",
        "200-299% FPL" = "200-299% Fpl",
        "300% FPL And Above" = "300% Fpl And Above"
      ),
      ahedtc_binary = forcats::fct_recode(
        aheduc,
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
      lnghmt_binary = forcats::fct_recode(
        langhome,
        "English" = "English & Chinese",
        "English" = "English & Spanish",
        "English" = "English & One Other Language",
        "English" = "English & European Language",
        "English" = "English & Another Asian Language",
        "Non-English" = "Spanish",
        "Non-English" = "Chinese",
        "Non-English" = "Korean",
        "Non-English" = "Vietnamese",
        "Non-English" = "Other Languages (2+)",
        "Non-English" = "Other Asian Language (1 Only)",
        "Non-English" = "Other Non-asian Language (1 Only)"
      ),
      uninsured = factor(ifelse(
        as.character(instype) == "Uninsured",
        "Yes",
        "No"
      )),
      health_office = forcats::fct_recode(
        tf2,
        "Primary care office" = "Doctor's Office/kaiser/hmo",
        "Primary care office" = "Clinic/health Center/hospital Clinic",
        "Primary care office" = "Other Health Professional (not Md)/alternative Medicine",
        "Not primary care office" = "Inapplicable",
        "Not primary care office" = "Emergency Room",
        "Not primary care office" = "Family Members/friends Residence/who Are Health Professionals",
        "Not primary care office" = "Some Other Place",
        "Not primary care office" = "No One Place"
      ),
      school_last_week = forcats::fct_recode(
        ta4,
        "No" = "On Vacation",
        "No" = "Not Ascertained",
        "No" = "Don't Know",
        "No" = "Refused",
        "No" = "Inapplicable",
        "Yes" = "Home Schooled"
      ),
      health_missed_days = dplyr::case_when(
        school_last_week == "Yes" & tb4 >= 0 ~ as.numeric(tb4),
        school_last_week == "Yes" & tb4 %in% c(-7, -8) ~ NA_real_,
        school_last_week == "No" ~ 0,
        TRUE ~ NA_real_
      ),
      tl53 = forcats::fct_rev(tl53),
      tl25_pos = case_when(
        tl25 == "Somewhat Agree" ~ "Yes",
        tl25 == "Strongly Agree" ~ "Yes",
        TRUE ~ "No"
      ),
      tl27_pos = case_when(
        tl27 == "Somewhat Agree" ~ "Yes",
        tl27 == "Strongly Agree" ~ "Yes",
        TRUE ~ "No"
      ),
      tl53_pos = case_when(
        tl53 == "Definitely Could" ~ "Yes",
        tl53 == "Probably Could" ~ "Yes",
        tl53 == "Maybe Could" ~ "Yes",
        TRUE ~ "No"
      ),
      tq10_pos = case_when(
        tq10 == "All Of The Time" ~ "Yes",
        tq10 == "Most Of The Time" ~ "Yes",
        tq10 == "Some Of The Time" ~ "Yes",
        TRUE ~ "No"
      ),
      tq11_pos = case_when(
        tq11 == "All Of The Time" ~ "Yes",
        tq11 == "Most Of The Time" ~ "Yes",
        tq11 == "Some Of The Time" ~ "Yes",
        TRUE ~ "No"
      ),
      tq14_pos = case_when(
        tq14 == "All Of The Time" ~ "Yes",
        tq14 == "Most Of The Time" ~ "Yes",
        tq14 == "Some Of The Time" ~ "Yes",
        TRUE ~ "No"
      ),
      tq15_pos = case_when(
        tq15 == "All Of The Time" ~ "Yes",
        tq15 == "Most Of The Time" ~ "Yes",
        tq15 == "Some Of The Time" ~ "Yes",
        TRUE ~ "No"
      ),
      tq16_pos = case_when(
        tq16 == "All Of The Time" ~ "Yes",
        tq16 == "Most Of The Time" ~ "Yes",
        tq16 == "Some Of The Time" ~ "Yes",
        TRUE ~ "No"
      )
    ) %>%
    mutate(
      survey_dates = parse_my_to_date(tadate_mm),
      survey_years = lubridate::year(survey_dates),
      survey_months = lubridate::month(survey_dates)
    ) %>%
    mutate(across(where(is.factor), fct_drop))
}


# adapted from lemon, not currently in DAC packages unfortunately
fix_reposition_legend <- function(
  aplot,
  position = NULL,
  legend = NULL,
  panel = 'panel',
  x = NULL,
  y = NULL,
  just = NULL,
  name = 'guide-box',
  clip = 'on',
  offset = c(0, 0),
  z = Inf,
  plot = TRUE
) {
  g_legend <- function(a.gplot) {
    if (!gtable::is.gtable(a.gplot)) {
      a.gplot <- ggplotGrob(a.gplot)
    }
    #gtable_filter(a.gplot, 'guide-box', fixed=TRUE)
    leg <- which(sapply(a.gplot$grobs, function(x) x$name) == "guide-box")
    a.gplot$grobs[[leg]]
  }

  # Work out positioning
  if (!is.null(position)) {
    if (length(offset) == 1) {
      offset <- offset[c(1, 1)]
    }
    if (!grid::is.unit(offset)) {
      offset <- grid::unit(offset, 'npc')
    }

    position <- match.arg(
      position,
      c(
        'bottom right',
        'right bottom',
        'bottom',
        'bottom left',
        'left bottom',
        'left',
        'top left',
        'top',
        'top right',
        'left top',
        'right top',
        'right',
        'center'
      )
    )
    just <- switch(
      position,
      'bottom right' = c(x = 1, y = 0),
      'right bottom' = c(x = 1, y = 0),
      'bottom' = c(x = 0.5, y = 0),
      'bottom left' = c(x = 0, y = 0),
      'left bottom' = c(x = 0, y = 0),
      'left' = c(x = 0, y = 0.5),
      'top left' = c(x = 0, y = 1),
      'left top' = c(x = 0, y = 1),
      'top' = c(x = 0.5, y = 1),
      'top right' = c(x = 1, y = 1),
      'right top' = c(x = 1, y = 1),
      'right' = c(x = 1, y = 0.5),
      'center' = c(x = 0.5, y = 0.5)
    )
    if (is.null(x)) {
      x <- unit(just[1], 'npc') +
        offset[1] *
          ifelse(
            grepl('right', position),
            -1,
            ifelse(grepl('left', position), 1, 0)
          )
    }
    if (is.null(y)) {
      y <- unit(just[2], 'npc') +
        offset[2] *
          ifelse(
            grepl('top', position),
            -1,
            ifelse(grepl('bottom', position), 1, 0)
          )
    }
  }
  if (!is.null(x) && !grid::is.unit(x)) {
    x <- unit(x, 'npc')
  }
  if (!is.null(y) && !grid::is.unit(y)) {
    y <- unit(y, 'npc')
  }

  if (is.null(x) | is.null(y) | is.null(just)) {
    stop('Please supply either `position`, or `x`, `y`, and `just` arguments.')
  }

  # Extract legends and gtable
  if (is.null(legend) & inherits(aplot, 'ggplot')) {
    legend <- g_legend(aplot)
  }

  if (is.null(legend) || inherits(legend, 'zeroGrob')) {
    stop('No legend given in arguments, or could not extract legend from plot.')
  }

  if (!inherits(aplot, 'gtable')) {
    aplot$theme$legend.position <- 'hidden'
    aplot <- ggplotGrob(aplot)
    #aplot <- ggplot_gtable(ggplot_build(aplot + theme(legend.position='hidden')))
  }

  # Update name if already found. No idea why this is necessary.
  if (any(grepl(name, aplot$layout$name))) {
    add <- gregexpr(
      paste0('(?!', name, '-?)([[:digit:]]+)'),
      aplot$layout$name,
      perl = TRUE
    )
    add <- regmatches(aplot$layout$name, add)
    add <- as.integer(unlist(add))
    if (length(add) > 0) {
      add <- max(add) + 1
    } else {
      add <- 1
    }
    name <- paste(name, add, sep = '-')
  }

  legend$vp <- grid::viewport(
    x = x,
    y = y,
    just = just,
    width = sum(legend$widths),
    height = sum(legend$heights)
  )
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
    if (length(pn) == 0) {
      stop('Could not find panel named `', panel, '`.')
    }

    aplot <- with(
      aplot$layout[pn, ],
      gtable::gtable_add_grob(
        aplot,
        legend,
        t = min(t),
        l = min(l),
        r = max(r),
        b = max(b),
        z = .z,
        clip = .clip,
        name = legend$name
      )
    )
  } else if (
    (is.numeric(panel) | is.integer(panel)) & length(panel) %in% c(2, 4)
  ) {
    panel <- rep(as.integer(panel), length.out = 4)
    aplot <- gtable::gtable_add_grob(
      aplot,
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

load_chain <- function(..data, ..year, haven_loader, stata_or_sas) {
  stata_or_sas <- match.arg(stata_or_sas, choices = c("stata", "sas"))
  ..data <- haven_loader(..data) %>%
    rename_all(tolower)

  if (stata_or_sas == "sas") {
    ..data <- ..data %>%
      sas_to_label(sas_to_factor_list[[as.character(..year)]])
  }
  ..data %>%
    select(any_of(varnames_teeny)) %>%
    mutate(year = ..year) %>%
    haven::as_factor()
}

verify_file_names_and_data_type <- function(
  file_name_2023 = NULL,
  file_name_2022 = NULL,
  file_name_2021 = NULL,
  stata_or_sas
) {
  stata_or_sas <- match.arg(stata_or_sas, choices = c("stata", "sas"))

  if (stata_or_sas == "stata") {
    if (is.null(file_name_2021)) {
      file_name_2021 <- here::here(
        'Data',
        'dummyfile_2023_teen_stata',
        'TEEN_with_format.dta'
      )
    }
    if (is.null(file_name_2022)) {
      file_name_2022 <- here::here(
        'Data',
        'teen 2022 dummy STATA',
        'TEEN_with_format.dta'
      )
    }
    if (is.null(file_name_2023)) {
      file_name_2023 <- here::here(
        'Data',
        'dummyfile_2021_teen_stata',
        'TEEN_with_format.dta'
      )
    }

    haven_loader <- haven::read_dta
  } else if (stata_or_sas == "sas") {
    if (is.null(file_name_2021)) {
      file_name_2021 <- here::here(
        'Data',
        'dummyfiles_2021_teen_sas',
        'dummy_teen.sas7bdat'
      )
    }
    if (is.null(file_name_2022)) {
      file_name_2022 <- here::here(
        'Data',
        'dummyfiles_2022_teen_sas',
        'dummy_teen.sas7bdat'
      )
    }
    if (is.null(file_name_2023)) {
      file_name_2023 <- here::here(
        'Data',
        'dummyfiles_2023_teen_sas',
        'dummy_teen.sas7bdat'
      )
    }

    haven_loader <- haven::read_sas
  } else {
    warning(
      "Data file type 'stata' or 'sas' not specified. Defaulting to using stata."
    )

    if (is.null(file_name_2021)) {
      file_name_2021 <- here::here(
        'Data',
        'dummyfile_2023_teen_stata',
        'TEEN_with_format.dta'
      )
    }
    if (is.null(file_name_2022)) {
      file_name_2022 <- here::here(
        'Data',
        'teen 2022 dummy STATA',
        'TEEN_with_format.dta'
      )
    }
    if (is.null(file_name_2023)) {
      file_name_2023 <- here::here(
        'Data',
        'dummyfile_2021_teen_stata',
        'TEEN_with_format.dta'
      )
    }

    haven_loader <- haven::read_dta
  }

  return(
    list(
      file_name_2021 = file_name_2021,
      file_name_2022 = file_name_2022,
      file_name_2023 = file_name_2023,
      haven_loader = haven_loader
    )
  )
}

sas_in_filenames <- function(...) {
  dots <- list(...)
  sas_found <- lapply(dots, function(x) {
    grepl("\\.sas", tolower(x))
  }) %>%
    unlist()
  if (length(sas_found) == 0) {
    return(FALSE)
  } # no files provided

  # check if filenames are specified
  sas_present <- all(sas_found)
  if (any(!sas_found) && !all(!sas_found)) {
    warning(
      "Some file names do not appear to be sas files based on file extensions"
    )
  }

  return(sas_present)
}

set_sas_or_stata <- function(
  stata_or_sas = NA,
  file_name_2021,
  file_name_2022,
  file_name_2023
) {
  # is sas extension present in all file names?
  sas_present <- sas_in_filenames(
    file_name_2021,
    file_name_2022,
    file_name_2023
  )

  # check if stata_or_sas is set via environment variable
  if (is.na(stata_or_sas) || identical(stata_or_sas, "")) {
    if (
      !is.null(file_name_2021) &&
        !is.null(file_name_2022) &&
        !is.null(file_name_2023)
    ) {
      if (sas_present) {
        stata_or_sas <- "sas"
      }
      warning(
        "Environment variable 'CHIS_DATA_TYPE' not set but file names appear to be sas files. Using sas prep code"
      )
      stata_or_sas <- "stata"
    } else {
      warning(
        "Environment variable 'CHIS_DATA_TYPE' not set. Defaulting to using stata."
      )
      stata_or_sas <- "stata"
    }
  } else {
    if (isTRUE(sas_present) && stata_or_sas == "stata") {
      warning(
        "Environment variable 'CHIS_DATA_TYPE' set to 'stata' but file names appear to be sas files. Please make sure this is correct as there will likely be an error downstream"
      )
    }
  }

  stata_or_sas <- match.arg(stata_or_sas, choices = c("stata", "sas"))
  return(stata_or_sas)
}

sas_to_label <- function(data, chis_label_list) {
  data_out <- data %>%
    mutate(
      across(
        any_of(names(chis_label_list)),
        ~ factor(
          as.character(.),
          labels = names(chis_label_list[[cur_column()]]),
          levels = chis_label_list[[cur_column()]]
        )
      )
    )
  return(data_out)
}

recover_df <- function(q, p) {
  pt_val <- p * 0.5
  logp <- log(pt_val)

  obj <- function(x) {
    (logp - stats::pt(abs(q), x, lower.tail = FALSE, log.p = TRUE))^2
  }
  rs <- optimize(f = obj, interval = c(1, 1e6))
  return(rs$minimum)
}

make_stat_display <- function(n, p, m, s) {
  fmt <- function(x) {
    formatC(x, big.mark = ',', digits = 1, format = "f")
  }
  dplyr::case_when(
    !is.na(m) & !is.na(s) ~ sprintf("%s (%.1f)", fmt(m), s),
    !is.na(n) & !is.na(p) ~ sprintf("%s (%.1f%%)", fmt(n), p),
    !is.na(n) ~ sprintf("%s", fmt(n)),
    TRUE ~ NA_character_
  )
}

mode_from_table1 <- function(data, var_name, collapse_fun = NULL) {
  mode_df <- data |>
    dplyr::filter(
      variable == var_name,
      !is.na(label),
      label != "",
      !is.na(N)
    ) |>
    dplyr::transmute(label = as.character(label), N = as.numeric(N))

  if (!is.null(collapse_fun)) {
    mode_df <- mode_df |>
      dplyr::mutate(label = collapse_fun(label)) |>
      dplyr::filter(!is.na(label), label != "")
  }

  if (nrow(mode_df) == 0) {
    return(NA_character_)
  }

  mode_df |>
    dplyr::group_by(label) |>
    dplyr::summarise(N = sum(N, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(N), label) |>
    dplyr::slice(1) |>
    dplyr::pull(label)
}

collapse_ahedtc_binary <- function(x) {
  dplyr::case_when(
    x %in%
      c(
        "No Formal Education",
        "Grade 1-8",
        "Grade 9-11",
        "Grade 12/H.S. Diploma",
        "Grade 12/h.s. Diploma"
      ) ~ "No college",
    x %in%
      c(
        "Some College",
        "Vocational School",
        "AA or AS Degree",
        "Aa Or As Degree",
        "BA or BS Degree",
        "Ba Or Bs Degree",
        "Some Grad. School",
        "MA or MS Degree",
        "Ma Or Ms Degree",
        "Ph.D. or Equivalent",
        "Ph.d. Or Equivalent"
      ) ~ "College or more",
    TRUE ~ NA_character_
  )
}

collapse_povll_binary <- function(x) {
  dplyr::case_when(
    grepl("^300%", x) ~ "300% FPL And Above",
    grepl("^[0-9]+-[0-9]+%", x) ~ "Less than 300% FPL",
    TRUE ~ NA_character_
  )
}

collapse_lnghmt_binary <- function(x) {
  dplyr::case_when(
    grepl("^English", x) ~ "English",
    TRUE ~ "Non-English"
  )
}

extract_model_var <- function(term_name) {
  dplyr::case_when(
    term_name == "(Intercept)" ~ NA_character_,
    grepl("^age_group", term_name) ~ "age_group",
    grepl("^srsex", term_name) ~ "srsex",
    grepl("^ombsrreo", term_name) ~ "ombsrreo",
    grepl("^sch_typ", term_name) ~ "sch_typ",
    grepl("^ahedtc_binary", term_name) ~ "ahedtc_binary",
    grepl("^povll_binary", term_name) ~ "povll_binary",
    grepl("^lnghmt_binary", term_name) ~ "lnghmt_binary",
    grepl("^ur_clrt2", term_name) ~ "ur_clrt2",
    grepl("^uninsured", term_name) ~ "uninsured",
    grepl("^health_office", term_name) ~ "health_office",
    grepl("^tf9", term_name) ~ "tf9",
    grepl("^tl25_pos", term_name) ~ "tl25_pos",
    grepl("^tl27_pos", term_name) ~ "tl27_pos",
    grepl("^tl50", term_name) ~ "tl50",
    grepl("^tl53_pos", term_name) ~ "tl53_pos",
    grepl("^tq10_pos", term_name) ~ "tq10_pos",
    grepl("^tq11_pos", term_name) ~ "tq11_pos",
    grepl("^tq14_pos", term_name) ~ "tq14_pos",
    grepl("^tq16_pos", term_name) ~ "tq16_pos",
    grepl("^tl10", term_name) ~ "tl10",
    grepl("^school_last_week", term_name) ~ "school_last_week",
    grepl(
      "^scale\\(I\\(as.numeric\\(school_last_week == \"Yes\"\\)\\) \\* tb4\\)$",
      term_name
    ) ~ "tb4",
    grepl("^tq15_pos", term_name) ~ "tq15_pos",
    TRUE ~ term_name
  )
}
set_missing_baseline <- function(data, var_name, baseline_value) {
  if (!(var_name %in% names(data))) {
    return(data)
  }
  cur <- data[[var_name]]
  if (all(is.na(cur))) {
    if (is.factor(cur)) {
      data[[var_name]] <- factor(baseline_value, levels = levels(cur))
    } else {
      data[[var_name]] <- baseline_value
    }
  }
  data
}

# Create a custom function
get_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

term_to_design_value <- function(term_name, data_row) {
  if (term_name == "(Intercept)") {
    return(1)
  }

  if (term_name == "I((as.numeric(school_last_week == \"Yes\")) * tb4)") {
    return(
      as.numeric(as.character(data_row$school_last_week) == "Yes") *
        as.numeric(data_row$tb4)
    )
  }

  if (term_name %in% names(data_row)) {
    return(as.numeric(data_row[[term_name]]))
  }

  factor_map <- data.frame(
    pattern = c(
      "^age_group",
      "^srsex",
      "^ombsrreo",
      "^sch_typ",
      "^ahedtc_binary",
      "^povll_binary",
      "^lnghmt_binary",
      "^ur_clrt2",
      "^uninsured",
      "^health_office",
      "^tf9",
      "^tl25_pos",
      "^tl27_pos",
      "^tl50",
      "^tl53_pos",
      "^tq10_pos",
      "^tq11_pos",
      "^tq14_pos",
      "^tq16_pos",
      "^school_last_week",
      "^tl10",
      "^tq15_pos"
    ),
    var = c(
      "age_group",
      "srsex",
      "ombsrreo",
      "sch_typ",
      "ahedtc_binary",
      "povll_binary",
      "lnghmt_binary",
      "ur_clrt2",
      "uninsured",
      "health_office",
      "tf9",
      "tl25_pos",
      "tl27_pos",
      "tl50",
      "tl53_pos",
      "tq10_pos",
      "tq11_pos",
      "tq14_pos",
      "tq16_pos",
      "school_last_week",
      "tl10",
      "tq15_pos"
    ),
    stringsAsFactors = FALSE
  )

  for (ii in seq_len(nrow(factor_map))) {
    patt <- factor_map$pattern[ii]
    var_name <- factor_map$var[ii]
    if (grepl(patt, term_name) && var_name %in% names(data_row)) {
      level_name <- sub(patt, "", term_name)
      return(as.numeric(as.character(data_row[[var_name]]) == level_name))
    }
  }

  NA_real_
}

pooled_mean <- function(n, mean, ...) {
  weighted.mean(x = mean, w = n, ...)
}

pooled_sd <- function(n, mean, sd) {
  keep <- !is.na(n) & !is.na(mean) & !is.na(sd) & n > 0

  n <- n[keep]
  mean <- mean[keep]
  sd <- sd[keep]

  N <- sum(n)
  M <- sum(n * mean) / N

  sqrt(
    sum((n - 1) * sd^2 + n * (mean - M)^2) /
      (N - 1)
  )
}

apply_column_suppression <- function(
  dat,
  threshold = 500
) {
  dat <- dat |>
    dplyr::group_by(variable, year, status) |>
    dplyr::mutate(
      # Number of already bounded cells in this
      # variable × year × status column
      n_bounded_col = sum(
        bound != "",
        na.rm = TRUE
      ),

      # Add a complementary bound only if there is
      # exactly one bounded cell, and that cell is
      # a primary "<" bound.
      trigger_secondary = n_bounded_col == 1 &
        any(bound == "<"),

      eligible_secondary = trigger_secondary &
        bound == "" &
        N > 0 &
        variable != "Age*" &
        variable != "Total (%)",

      # Choose smallest positive exact cell
      candidate_rank = dplyr::if_else(
        eligible_secondary,
        rank(
          dplyr::if_else(
            eligible_secondary,
            N,
            Inf
          ),
          ties.method = "first"
        ),
        Inf
      ),

      add_secondary = eligible_secondary &
        candidate_rank == 1,

      # Simple complementary rule:
      #
      # exact N -> report > N - 500
      N_display = dplyr::if_else(
        add_secondary,
        pmax(N - threshold, 0),
        N_display
      ),

      bound = dplyr::if_else(
        add_secondary,
        ">",
        bound
      ),

      suppress_secondary = suppress_secondary |
        add_secondary
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      -n_bounded_col,
      -trigger_secondary,
      -eligible_secondary,
      -candidate_rank,
      -add_secondary
    )

  # Propagate the complementary bound to the opposite
  # Yes/No member of the SAME row.
  dat |>
    dplyr::group_by(variable, label, year) |>
    dplyr::mutate(
      n_secondary_yn = sum(
        suppress_secondary &
          status %in% c("Yes", "No"),
        na.rm = TRUE
      ),

      secondary_status = dplyr::if_else(
        n_secondary_yn == 1,
        as.character(
          status[
            suppress_secondary &
              status %in% c("Yes", "No")
          ][1]
        ),
        NA_character_
      ),

      secondary_value = dplyr::if_else(
        n_secondary_yn == 1,
        N_display[
          suppress_secondary &
            status %in% c("Yes", "No")
        ][1],
        NA_real_
      ),

      secondary_bound = dplyr::if_else(
        n_secondary_yn == 1,
        bound[
          suppress_secondary &
            status %in% c("Yes", "No")
        ][1],
        NA_character_
      ),

      propagate_to_partner = n_secondary_yn == 1 &
        status %in% c("Yes", "No") &
        as.character(status) != secondary_status &
        bound == "" &
        N > 0,

      N_display = dplyr::if_else(
        propagate_to_partner,
        dplyr::case_when(
          # If one side is > N - 500,
          # its partner is < N_partner + 500
          secondary_bound == ">" ~ N + threshold,

          # Conversely, if one side is < N + 500,
          # partner is > N_partner - 500
          secondary_bound == "<" ~ pmax(N - threshold, 0),

          TRUE ~ N_display
        ),
        N_display
      ),

      bound = dplyr::if_else(
        propagate_to_partner,
        dplyr::case_when(
          secondary_bound == ">" ~ "<",
          secondary_bound == "<" ~ ">",
          TRUE ~ bound
        ),
        bound
      ),

      suppress_secondary = suppress_secondary |
        propagate_to_partner,

      suppress_final = bound != ""
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      -n_secondary_yn,
      -secondary_status,
      -secondary_value,
      -secondary_bound,
      -propagate_to_partner
    )
}

apply_yes_no_bounds <- function(dat) {
  dat |>
    dplyr::mutate(
      suppress_secondary = FALSE,
      N_display = N,
      bound = ""
    ) |>
    dplyr::group_by(variable, label, year) |>
    dplyr::mutate(
      n_suppressed_row = sum(
        suppress_primary &
          status %in% c("Yes", "No"),
        na.rm = TRUE
      ),

      # Upper bound associated with the single primary
      # suppressed Yes/No cell
      primary_upper = dplyr::if_else(
        n_suppressed_row == 1,
        N_upper[
          suppress_primary &
            status %in% c("Yes", "No")
        ][1],
        NA_real_
      ),

      # --------------------------------------------------
      # Primary Yes/No suppression:
      #     hidden cell < x
      # --------------------------------------------------
      N_display = dplyr::if_else(
        suppress_primary &
          status %in% c("Yes", "No"),
        N_upper,
        N_display
      ),

      bound = dplyr::if_else(
        suppress_primary &
          status %in% c("Yes", "No"),
        "<",
        bound
      ),

      # --------------------------------------------------
      # Row complement:
      #
      #     hidden < x
      #     other > T - x
      #
      # BUT:
      # leave a known zero alone.
      # Also don't create useless negative bounds.
      # --------------------------------------------------
      row_lower = row_total_N - primary_upper,

      make_row_complement = n_suppressed_row == 1 &
        status %in% c("Yes", "No") &
        !suppress_primary &
        N > 0,

      N_display = dplyr::if_else(
        make_row_complement,
        pmax(row_lower, 0),
        N_display
      ),

      bound = dplyr::if_else(
        make_row_complement,
        ">",
        bound
      ),

      # --------------------------------------------------
      # Derived Total:
      #
      # if Yes or No contains suppressed information,
      # don't reveal the exact derived row total.
      # --------------------------------------------------
      N_display = dplyr::if_else(
        status == "Total" &
          suppress_primary,
        N_upper,
        N_display
      ),

      bound = dplyr::if_else(
        status == "Total" &
          suppress_primary,
        "<",
        bound
      ),

      suppress_final = bound != ""
    ) |>
    dplyr::ungroup() |>
    dplyr::select(
      -n_suppressed_row,
      -primary_upper,
      -row_lower,
      -make_row_complement
    )
}

build_table1 <- function(
  tabl_1_long,
  suppression_cutoff = 500
) {
  tab1_raw <- tabl_1_long |>
    dplyr::mutate(
      N = dplyr::if_else(
        variable != "Age" & is.na(N),
        0,
        N
      ),
      perc = dplyr::if_else(
        variable != "Age" & is.na(perc),
        0,
        perc
      )
    ) |>
    tidyr::pivot_wider(
      id_cols = c("label", "variable"),
      names_from = c(year, tf45),
      values_from = c(
        "N",
        "perc",
        "mean",
        "sd",
        "suppress"
      )
    ) |>
    dplyr::select(variable, label, dplyr::everything()) |>
    dplyr::filter(
      !(variable == "Age" & !is.na(label))
    ) |>
    dplyr::mutate(
      variable = forcats::fct_recode(
        factor(variable),
        "Rural/Urban" = "Rural/Urban (Claritas ZIP, 2-level)"
      ),
      label = forcats::fct_recode(
        factor(label),
        "Grade 12/H.S. Diploma" = "Grade 12/h.s. Diploma",
        "AA or AS Degree" = "Aa Or As Degree",
        "BA or BS Degree" = "Ba Or Bs Degree",
        "MA or MS Degree" = "Ma Or Ms Degree",
        "Ph.D. or Equivalent" = "Ph.d. Or Equivalent"
      )
    ) |>
    dplyr::mutate(
      label_order = dplyr::if_else(
        as.character(variable) == "Parents' Educational Attainment",
        match(
          as.character(label),
          c(
            "No Formal Education",
            "Grade 1-8",
            "Grade 9-11",
            "Grade 12/H.S. Diploma",
            "Some College",
            "Vocational School",
            "AA or AS Degree",
            "BA or BS Degree",
            "Some Grad. School",
            "MA or MS Degree",
            "Ph.D. or Equivalent"
          )
        ),
        NA_integer_
      )
    ) |>
    dplyr::arrange(
      variable,
      dplyr::coalesce(label_order, 9999L),
      as.character(label)
    ) |>
    dplyr::select(-label_order) |>
    dplyr::mutate(
      label = dplyr::if_else(
        variable == "Age",
        "",
        as.character(label)
      ),
      variable = dplyr::if_else(
        variable == "Age",
        "Age*",
        as.character(variable)
      )
    ) |>
    dplyr::mutate(
      label = dplyr::case_when(
        variable == "Ethnicity" &
          label %in%
            c(
              "Asian",
              "Pacific Islander"
            ) ~
          "Asian/Pacific Islander",

        variable == "Ethnicity" &
          label %in%
            c(
              "American Indian",
              "Two or More Races",
              "Two Or More Races"
            ) ~
          "Multiracial and Other",

        variable == "Language Spoken at Home" &
          label %in%
            c(
              "English",
              "English & Another Asian Language",
              "English & Chinese",
              "English & European Language",
              "English & One Other Language",
              "English & Spanish"
            ) ~
          "English",

        variable == "Language Spoken at Home" &
          label %in%
            c(
              "Chinese",
              "Korean",
              "Other Asian Language (1 Only)",
              "Other Languages (2+)",
              "Other Non-asian Language (1 Only)",
              "Spanish",
              "Vietnamese"
            ) ~
          "Other",

        variable == "Type of School Attended" &
          label %in%
            c(
              "Inapplicable",
              "Other"
            ) ~
          "Other",

        variable == "Parents' Educational Attainment" &
          label %in%
            c(
              "No Formal Education",
              "Grade 1-8",
              "Grade 9-11",
              "Grade 12/H.S. Diploma"
            ) ~
          "Less than college degree",

        variable == "Parents' Educational Attainment" &
          label %in%
            c(
              "Some College",
              "Vocational School",
              "AA or AS Degree",
              "BA or BS Degree",
              "Some Grad. School",
              "MA or MS Degree",
              "Ph.D. or Equivalent"
            ) ~
          "Attended college",

        TRUE ~ label
      )
    ) |>
    tidyr::pivot_longer(
      cols = tidyselect::matches(
        "^(N|perc|mean|sd|suppress)_"
      ),
      names_to = c(".value", "year", "status"),
      names_pattern = "(N|perc|mean|sd|suppress)_(\\d{4})_(Yes|No)"
    ) |>
    dplyr::mutate(
      suppress = dplyr::coalesce(
        suppress,
        FALSE
      ),
      N_upper = dplyr::if_else(
        suppress,
        suppression_cutoff,
        N
      )
    ) |>

    # collapse recoded labels
    dplyr::group_by(
      variable,
      label,
      year,
      status
    ) |>
    dplyr::summarise(
      sd = if (dplyr::first(variable) == "Age*") {
        dplyr::first(sd)
      } else {
        pooled_sd(N, mean, sd)
      },
      mean = if (dplyr::first(variable) == "Age*") {
        dplyr::first(mean)
      } else {
        pooled_mean(N, mean)
      },
      N = sum(N, na.rm = TRUE),
      N_upper = sum(N_upper, na.rm = TRUE),
      suppress_primary = all(suppress),
      .groups = "drop"
    ) |>

    # year/status denominators
    dplyr::group_by(year, status) |>
    dplyr::mutate(
      total_N = sum(
        N[variable == "Ethnicity"],
        na.rm = TRUE
      ),
      N = dplyr::if_else(
        variable == "Age*",
        total_N,
        N
      ),
      N_upper = dplyr::if_else(
        variable == "Age*",
        total_N,
        N_upper
      )
    ) |>
    dplyr::ungroup() %>%

    # add Total status
    {
      dplyr::bind_rows(
        . |>
          dplyr::group_by(
            variable,
            label,
            year
          ) |>
          dplyr::mutate(
            perc = 100 * N / sum(N, na.rm = TRUE)
          ) |>
          dplyr::ungroup(),

        . |>
          dplyr::group_by(
            variable,
            label,
            year
          ) |>
          dplyr::summarise(
            status = "Total",
            sd = pooled_sd(
              N,
              mean,
              sd
            ),
            mean = pooled_mean(
              N,
              mean
            ),
            N = sum(
              N,
              na.rm = TRUE
            ),
            N_upper = sum(
              N_upper,
              na.rm = TRUE
            ),
            total_N = sum(
              total_N,
              na.rm = TRUE
            ),
            suppress_primary = any(suppress_primary),
            .groups = "drop"
          ) |>
          dplyr::mutate(
            perc = 100 * N / total_N
          )
      )
    } |>
    dplyr::group_by(
      variable,
      label,
      year
    ) |>
    dplyr::mutate(
      row_total_N = N[status == "Total"][1]
    ) |>
    dplyr::ungroup() |>
    apply_yes_no_bounds() |>
    apply_column_suppression(
      threshold = suppression_cutoff
    ) %>%

    {
      dplyr::bind_rows(
        .,
        . |>
          dplyr::filter(
            variable == "Age*"
          ) |>
          dplyr::mutate(
            variable = "Total (%)",
            mean = NA_real_,
            sd = NA_real_
          )
      )
    } |>
    # denominator for Yes/No row percentages
    dplyr::group_by(variable, label, year) |>
    dplyr::mutate(
      row_total_display = sum(
        N_display[status %in% c("Yes", "No")],
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>

    # denominator for Total column percentages
    dplyr::group_by(year, status) |>
    dplyr::mutate(
      total_N_display = sum(
        N_display[variable == "Ethnicity"],
        na.rm = TRUE
      )
    ) |>
    dplyr::ungroup() |>

    dplyr::mutate(
      perc_display = dplyr::case_when(
        variable == "Age*" ~ NA_real_,

        status %in% c("Yes", "No") ~
          100 * N_display / row_total_display,

        status == "Total" ~
          100 * N_display / total_N_display,

        TRUE ~ NA_real_
      )
    ) |>
    dplyr::mutate(
      status = forcats::fct_relevel(
        factor(status),
        "Yes",
        "No",
        "Total"
      ),
      year = forcats::fct_relevel(
        factor(year),
        "2021",
        "2022",
        "2023"
      ),
      variable = forcats::fct_relevel(
        factor(variable),
        "Age*",
        "Ethnicity",
        "Language Spoken at Home",
        "Parents' Educational Attainment",
        "Poverty Level",
        "Rural/Urban",
        "Sex",
        "Type of School Attended",
        "Total (%)"
      )
    ) |>
    dplyr::arrange(
      variable,
      label,
      year,
      status
    )

  tab1_display <- tab1_raw |>
    dplyr::mutate(
      value = dplyr::case_when(
        variable == "Age*" ~
          sprintf(
            "%.1f (%.1f)",
            mean,
            sd
          ),

        suppress_final ~
          sprintf(
            "%s%s (%s%.1f%%)",
            bound,
            scales::comma(
              round(
                N_display,
                1
              )
            ),
            bound,
            perc_display
          ),

        TRUE ~
          sprintf(
            "%s (%.1f%%)",
            scales::comma(
              round(N, 1)
            ),
            perc_display
          )
      )
    ) |>
    dplyr::select(
      variable,
      label,
      year,
      status,
      value
    ) |>
    tidyr::pivot_wider(
      names_from = c(year, status),
      values_from = value,
      names_glue = "stat_{year}_{status}",
      names_sort = FALSE
    )

  list(
    raw = tab1_raw,
    display = tab1_display
  )
}

render_table1 <- function(
  tab1_display,
  doc_dir,
  prefix = "table1_",
  suffix = ""
) {
  tab1 <- tab1_display |>
    gt::gt(
      rowname_col = "label",
      groupname_col = "variable"
    ) |>
    gt::tab_stubhead(label = "") |>
    gt::tab_spanner(
      label = "2021",
      columns = c(
        stat_2021_Yes,
        stat_2021_No,
        stat_2021_Total
      )
    ) |>
    gt::tab_spanner(
      label = "2022",
      columns = c(
        stat_2022_Yes,
        stat_2022_No,
        stat_2022_Total
      )
    ) |>
    gt::tab_spanner(
      label = "2023",
      columns = c(
        stat_2023_Yes,
        stat_2023_No,
        stat_2023_Total
      )
    ) |>
    gt::cols_label(
      stat_2021_Yes = "Yes",
      stat_2021_No = "No",
      stat_2021_Total = "Total",
      stat_2022_Yes = "Yes",
      stat_2022_No = "No",
      stat_2022_Total = "Total",
      stat_2023_Yes = "Yes",
      stat_2023_No = "No",
      stat_2023_Total = "Total"
    ) |>
    gt::fmt_number(sep_mark = ",")

  # Save gt versions
  saveRDS(
    tab1,
    file = here::here(doc_dir, glue::glue("{prefix}_gt{suffix}.rds"))
  )

  gt::gtsave(
    tab1,
    filename = here::here(doc_dir, glue::glue("{prefix}_gt{suffix}.html"))
  )

  # Extract table body
  tab1df <- tab1 |>
    gt::extract_body() |>
    dplyr::mutate(
      `::rowname::` = dplyr::if_else(
        grepl("&amp;", `::rowname::`),
        gsub("&amp;", "&", `::rowname::`),
        `::rowname::`
      )
    )

  group_index <- tab1df |>
    dplyr::mutate(row = dplyr::row_number()) |>
    dplyr::summarise(
      start = min(row),
      end = max(row),
      .by = `::group_id::`
    )

  # Explicit names are safer than using tab1$`_boxhead`
  col_names <- c(
    " ",
    " ",
    "Yes",
    "No",
    "Total",
    "Yes",
    "No",
    "Total",
    "Yes",
    "No",
    "Total"
  )

  latex_tab <- tab1df |>
    stats::setNames(col_names) |>
    subset(select = -1) |>
    kableExtra::kbl(
      format = "latex",
      booktabs = TRUE,
      escape = TRUE,
      align = c("l", rep("r", 9))
    ) |>
    purrr::reduce(
      seq_len(nrow(group_index)),
      .init = _,
      .f = function(tbl, i) {
        kableExtra::group_rows(
          tbl,
          group_label = group_index$`::group_id::`[i],
          start_row = group_index$start[i],
          end_row = group_index$end[i]
        )
      }
    ) |>
    kableExtra::kable_styling(
      latex_options = c(
        "hold_position",
        "scale_down"
      )
    ) |>
    kableExtra::add_header_above(
      c(
        " " = 1,
        "2021" = 3,
        "2022" = 3,
        "2023" = 3
      )
    ) |>
    kableExtra::row_spec(
      0,
      align = "c"
    ) |>
    kableExtra::row_spec(
      seq(1, nrow(tab1df), 2),
      background = "#f5f5f5"
    )

  cat(
    latex_tab,
    file = here::here(doc_dir, glue::glue("{prefix}{suffix}.tex"))
  )

  # Word/flextable version
  tab1ft <- tab1df |>
    flextable::as_grouped_data(
      groups = "::group_id::"
    ) |>
    flextable::flextable() |>
    flextable::set_header_labels(
      "::group_id::" = "",
      "::rowname::" = "",
      stat_2021_Yes = "Yes",
      stat_2021_No = "No",
      stat_2021_Total = "Total",
      stat_2022_Yes = "Yes",
      stat_2022_No = "No",
      stat_2022_Total = "Total",
      stat_2023_Yes = "Yes",
      stat_2023_No = "No",
      stat_2023_Total = "Total"
    ) |>
    flextable::add_header_row(
      values = c(
        "",
        "2021",
        "2022",
        "2023"
      ),
      colwidths = c(
        2,
        3,
        3,
        3
      )
    ) |>
    flextable::vline(
      j = c(2, 5, 8),
      border = officer::fp_border(
        color = "gray50",
        width = 1
      ),
      part = "all"
    ) |>
    flextable::fit_to_width(
      max_width = 6.5
    ) |>
    flextable::fontsize(
      size = 8,
      part = "all"
    )

  flextable::save_as_docx(
    tab1ft,
    path = here::here(
      doc_dir,
      glue::glue("{prefix}{suffix}.docx")
    )
  )

  saveRDS(
    tab1ft,
    file = here::here(
      doc_dir,
      glue::glue("{prefix}{suffix}_ft.rds")
    )
  )

  invisible(
    list(
      gt = tab1,
      data = tab1df,
      flextable = tab1ft
    )
  )
}

###### Below is the lists used for SAS factor variable labeling #######
