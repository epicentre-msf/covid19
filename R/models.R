
#' Split dataset into a list of time-series, one for each country (splitting uses the iso_a3 variable)
#' Filter the time-series to a time-frame defined by time_unit_extent
#' Fill the gaps in the time-series, assuming that at missing date, cases and deaths = 0 
#' Smoothing using moving average with a parameterable time-window (in days)
#' Model time-series using a linear regression

make_model_cnt_linear <- function(series, df = df_ecdc, time_unit_extent = 12, ma_window = 3, min_sum = 30){
  #browser()
  lst_dta <- df %>% 
    mutate(
      iso_a3 = case_when(
        iso_a3 == "N/A" ~ NA_character_,
        TRUE ~ iso_a3)
    ) %>% 
    tidyr::drop_na(iso_a3) %>% 
    group_by(iso_a3) %>% 
    filter(between(date, max(date, na.rm = TRUE) - (time_unit_extent - 1), max(date, na.rm = TRUE))) %>% 
    multisplit("iso_a3")
  
  lst_mdls  <- list()
  lst_preds <- list()
  tbl_coeffs <- tibble(iso_a3 = character(), 
                       coeff  = numeric(), 
                       lwr    = numeric(), 
                       upr    = numeric())
  
  for (i in names(lst_dta)) {
    
    dta <- lst_dta[[i]] %>% 
      tidyr::complete(date = seq.Date(min(date, na.rm = TRUE), 
                                      max(date, na.rm = TRUE), by = 1), 
                      fill = list(cases = NA_real_, deaths = NA_real_))
    
    if (dim(dta)[1] > ma_window & sum(dta[[series]], na.rm = TRUE) > min_sum) {
      
      dta$ma <- forecast::ma(dta[series], order = ma_window)
      dta$ma <- na_if(dta$ma, 0) # Replace 0 values as NA
      
      mdl <- lm(log(ma) ~ date, data = dta)
      
      # Matrix of predictions
      mdl_preds <- matrix(data = NA, 
                          nrow = time_unit_extent, 
                          ncol = 3, 
                          dimnames = list(c(1:time_unit_extent), c('fit', 'lwr', 'upr')))
      
      preds <- exp(predict(mdl, interval = 'confidence'))
      
      matched_rows <- match(rownames(preds), rownames(mdl_preds))
      matched_cols <- match(colnames(preds), colnames(mdl_preds))
      mdl_preds[matched_rows, matched_cols] <- preds
      
      mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                           lwr   = confint(mdl)[2,1], 
                           upr   = confint(mdl)[2,2])
      
    } else {
      mdl <- NA_character_
      
      mdl_preds  <- tibble(fit = rep(NA_real_, dim(dta)[1]), 
                           lwr = rep(NA_real_, dim(dta)[1]), 
                           upr = rep(NA_real_, dim(dta)[1]))
      
      mdl_coeffs <- tibble(coeff = NA_real_, 
                           lwr   = NA_real_, 
                           upr   = NA_real_)
    }
    
    lst_mdls[[i]] <- mdl
    
    lst_preds[[i]] <- mdl_preds %>% as_tibble()
    
    tbl_coeffs <- tbl_coeffs %>% 
      add_row(iso_a3 = i, mdl_coeffs)
    
  }
  
  # Calculate doubling time
  tbl_doubling_time <- linear_doubling_time(lst_mdls)
  
  return(list(mdl = lst_mdls, 
              preds = lst_preds, 
              coeffs = tbl_coeffs, 
              doubling_time = tbl_doubling_time, 
              par = list(time_unit_sourced  = time_unit_extent, 
                         time_unit_modelled = time_unit_extent - (ma_window - 1), 
                         model = 'lm(log(ma) ~ date, data = dta)', 
                         moving_average_extent = ma_window, 
                         minimum_observations_sum = min_sum)))
}



#' Split dataset in a list of data frames
#' Create a cumulative sum
#' Filter to a time frame defined by time_unit_extent
#' Model data for each country using a linear regression of the cumulative count

make_model_cml_linear <- function(series, df = dfc_ecdc, time_unit_extent = 12, min_sum = 100){
  
  lst_dta <- df %>% 
    mutate(
      iso_a3 = case_when(
        iso_a3 == "N/A" ~ NA_character_,
        TRUE ~iso_a3)
    ) %>% 
    tidyr::drop_na(iso_a3) %>% 
    group_by(iso_a3) %>% 
    multisplit("iso_a3")
  
  lst_dta <- base::lapply(lst_dta, function(x) {cbind(x, cml = cumsum(x[[series]]))})
  
  lst_dta <- base::lapply(lst_dta, function(x) {x %>% filter(between(date, max(date, na.rm = TRUE) - (time_unit_extent - 1), max(date, na.rm = TRUE)))})
  
  lst_mdls  <- list()
  lst_preds <- list()
  tbl_coeffs <- tibble(iso_a3 = character(), 
                       coeff  = numeric(), 
                       lwr    = numeric(), 
                       upr    = numeric())
  
  for (i in names(lst_dta)) {
    
    dta <- lst_dta[[i]]
    dta$cml[1] <- ifelse(dta$cml[1] == 0, 0.001, dta$cml[1])
    
    if (dta[['cml']][nrow(dta)] > min_sum) {
      mdl <- lm(log(cml) ~ date, data = dta) 
      
      mdl_preds <- tibble(exp(data.frame(predict(mdl, interval = 'confidence'))))
      
      mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                           lwr   = confint(mdl)[2,1], 
                           upr   = confint(mdl)[2,2])
    } else {
      
      mdl <- NA_character_
      
      mdl_preds  <- tibble(prd = rep(NA_real_, dim(dta)[1]), 
                           lwr = rep(NA_real_, dim(dta)[1]), 
                           upr = rep(NA_real_, dim(dta)[1]))
      
      mdl_coeffs <- tibble(coeff = NA_real_, 
                           lwr   = NA_real_, 
                           upr   = NA_real_)
    }
    
    lst_mdls[[i]] <- mdl
    
    lst_preds[[i]] <- mdl_preds
    
    tbl_coeffs <- tbl_coeffs %>% 
      tibble::add_row(iso_a3 = i, mdl_coeffs)
  }
  
  # Calculate doubling time
  tbl_doubling_time <- linear_doubling_time(lst_mdls)
  
  return(list(mdl = lst_mdls, 
              preds = lst_preds, 
              coeffs = tbl_coeffs,  
              doubling_time = tbl_doubling_time, 
              par = list(time_unit_modelled  = time_unit_extent, 
                         model = 'lm(log(cml) ~ date, data = dta)',
                         minimum_observations_sum = min_sum)))  
}



# Split dataset in a list of data frames and filter to a time frame defined by time_unit_extent
# Model data for each country based on a Poisson regression
# (Poisson distribution is used mostly because of the zero values)

make_model_cnt_poisson <- function(df, series, time_unit_extent = 12, ma_window = 3, min_sum = 30){
  
  lst_dta <- df %>% 
    mutate(
      iso_a3 = case_when(
        iso_a3 == "N/A" ~ NA_character_,
        TRUE ~iso_a3)
    ) %>% 
    tidyr::drop_na(iso_a3) %>% 
    group_by(iso_a3) %>% 
    filter(between(date, max(date, na.rm = TRUE) - (time_unit_extent - 1), max(date, na.rm = TRUE))) %>% 
    multisplit("iso_a3")
  
  
  # The Model (Poisson regression)
  lst_mdls  <- list()
  lst_preds <- list()
  tbl_coeffs <- tibble(iso_a3 = character(), 
                       coeff  = numeric(), 
                       lwr    = numeric(), 
                       upr    = numeric())
  
  for (i in names(lst_dta)) {
    
    dta <- lst_dta[[i]]
    
    if (sum(dta[series]) > min_sum) {
      
      dta$obs <- dta[[series]]
      
      mdl <- glm(obs ~ date, family = poisson(link = 'log'), data = dta)
      
      log_fit <- predict(mdl, type = 'link')
      
      rse <- sqrt(diag(vcovHC(mdl)))[['date']] # To ask Francisco what is the better dispersion parameter to be used
      
      fit <- exp(log_fit)
      lwr <- exp(log_fit - (1.96 * confint(mdl)[2,1]))
      upr <- exp(log_fit + (1.96 * confint(mdl)[2,2]))
      
      mdl_preds  <- tibble(fit = fit, 
                           lwr = lwr, 
                           upr = upr)
      
      mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                           lwr   = confint(mdl)[2,1], 
                           upr   = confint(mdl)[2,2])
      
    } else {
      
      mdl <- NA_character_
      
      mdl_preds  <- tibble(fit = rep(NA_real_, dim(dta)[1]), 
                           lwr = rep(NA_real_, dim(dta)[1]), 
                           upr = rep(NA_real_, dim(dta)[1]))
      
      mdl_coeffs <- tibble(coeff = NA_real_, 
                           lwr   = NA_real_, 
                           upr   = NA_real_)
    }
    
    lst_mdls[[i]] <- mdl
    
    lst_preds[[i]] <- mdl_preds
    
    tbl_coeffs <- tbl_coeffs %>% 
      add_row(iso_a3 = i, mdl_coeffs)
  }
  
  # Calculate doubling time
  tbl_doubling_time <- poisson_doubling_time(lst_mdls)
  
  return(list(mdl = lst_mdls, 
              preds = lst_preds, 
              coeffs = tbl_coeffs, 
              doubling_time = tbl_doubling_time, 
              par = list(time_unit_sourced  = time_unit_extent, 
                         model = 'glm(obs ~ date, family = poisson(link = "log"), data = dta)', 
                         minimum_observations_sum = min_sum)))
}



#' Split dataset in a list of data frames
#' Create a cumulative sum
#' Filter to a time frame defined by time_unit_extent
#' Model data for each country
#' smoodthing using a predefined window 
#' Then making a linear regression

make_model_cml_poisson <- function(series, df, time_unit_extent = 12, min_sum = 100){
  
  lst_dta <- df %>% 
    mutate(
      iso_a3 = case_when(
        iso_a3 == "N/A" ~NA_character_,
        TRUE ~iso_a3)
    ) %>% 
    tidyr::drop_na(iso_a3) %>% 
    group_by(iso_a3) %>% 
    multisplit("iso_a3")
  
  lst_dta <- base::lapply(lst_dta, function(x) {cbind(x, cml = cumsum(x[[series]]))})
  
  lst_dta <- base::lapply(lst_dta, function(x) {x %>% filter(between(date, max(date, na.rm = TRUE) - (time_unit_extent - 1), max(date, na.rm = TRUE)))})
  
  lst_mdls  <- list()
  lst_preds <- list()
  tbl_coeffs <- tibble(iso_a3 = character(), 
                       coeff  = numeric(), 
                       lwr    = numeric(), 
                       upr    = numeric())
  
  for (i in names(lst_dta)) {
    
    dta <- lst_dta[[i]]
    
    if (dta[['cml']][nrow(dta)] > min_sum) {
      
      mdl <- glm(cml ~ date, family = poisson(link = 'log'), data = dta)
      
      log_fit <- predict(mdl)
      
      rse <- sqrt(diag(vcovHC(mdl)))[['date']] # See question above in cnt_poisson
      
      fit <- exp(log_fit)
      lwr <- exp(log_fit - (1.96 * confint(mdl)[2,1]))
      upr <- exp(log_fit + (1.96 * confint(mdl)[2,2]))
      
      mdl_preds  <- tibble(fit = fit, 
                           lwr = lwr, 
                           upr = upr)
      
      mdl_coeffs <- tibble(coeff = coefficients(mdl)[[2]], 
                           lwr   = confint(mdl)[2,1], 
                           upr   = confint(mdl)[2,2])
    } else {
      
      mdl <- NA_character_
      
      mdl_preds  <- tibble(fit = rep(NA_real_, dim(dta)[1]), 
                           lwr = rep(NA_real_, dim(dta)[1]), 
                           upr = rep(NA_real_, dim(dta)[1]))
      
      mdl_coeffs <- tibble(coeff = NA_real_, 
                           lwr   = NA_real_, 
                           upr   = NA_real_)
    }
    lst_mdls[[i]] <- mdl
    
    lst_preds[[i]] <- mdl_preds
    
    tbl_coeffs <- tbl_coeffs %>% 
      add_row(iso_a3 = i, mdl_coeffs)
  }
  
  # Calculate doubling time
  tbl_doubling_time <- poisson_doubling_time(lst_mdls)
  
  return(list(mdl = lst_mdls, 
              preds = lst_preds, 
              coeffs = tbl_coeffs,  
              doubling_time = tbl_doubling_time, 
              parameters = list(time_unit_modelled  = time_unit_extent, 
                                minimum_observations_sum = min_sum)))   
}



# Calculate linear doubling time
linear_doubling_time <- function(lst) {
  
  df_doubling_time <- tibble(iso_a3 = as.character(), 
                             est    = as.numeric(),
                             lwr    = as.numeric(),
                             upr    = as.numeric())
  
  for (i in names(lst)) {
    
    mdl <- lst[i]
    
    if (!is.na(mdl)) {
      mdl <- mdl[[1]]
      mdl_coeff <- coefficients(mdl)[[2]]
      mdl_lwrci <- confint(mdl)[2,1]
      mdl_uprci <- confint(mdl)[2,2]
      doubling_time_est <- ifelse(mdl_coeff > 0, log(2)/mdl_coeff, ifelse(mdl_coeff < 0, log(0.5)/mdl_coeff, Inf))
      doubling_time_lwr <- ifelse(mdl_uprci > 0, log(2)/mdl_uprci, ifelse(mdl_uprci < 0, log(0.5)/mdl_uprci, Inf))
      doubling_time_upr <- ifelse(mdl_lwrci > 0, log(2)/mdl_lwrci, ifelse(mdl_lwrci < 0, log(0.5)/mdl_lwrci, Inf))
    } else {
      doubling_time_est <- NA_real_
      doubling_time_lwr <- NA_real_
      doubling_time_upr <- NA_real_
    }
    
    df_doubling_time <- df_doubling_time %>% 
      add_row(iso_a3 = i, 
              est = doubling_time_est,
              lwr = doubling_time_lwr, 
              upr = doubling_time_upr)
  }
  return(df_doubling_time)
}


# Calculate Poisson doubling time
poisson_doubling_time <- function(lst) {
  
  df_doubling_time <- tibble(iso_a3 = character(), 
                             est = numeric(), 
                             lwr = numeric(), 
                             upr = numeric())
  
  for (i in names(lst)) {
    
    mdl <- lst[[i]]
    
    if (!is.na(mdl)) {
      doubling_time_est <- log(2)/(coefficients(mdl)[[2]])
      doubling_time_lwr <- log(2)/(coefficients(mdl)[[2]] + 1.96 * coefficients(summary(mdl))[2,2])
      doubling_time_upr <- log(2)/(coefficients(mdl)[[2]] - 1.96 * coefficients(summary(mdl))[2,2])
    } else {
      doubling_time_est <- NA_real_
      doubling_time_lwr <- NA_real_
      doubling_time_upr <- NA_real_
    }
    
    df_doubling_time <- df_doubling_time %>% 
      add_row(iso_a3 = i, 
              est = doubling_time_est,
              lwr = doubling_time_lwr,
              upr = doubling_time_upr)
  }
  return(df_doubling_time)
}


