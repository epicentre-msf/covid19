
#' @export
get_trends_data_new <- function(df, time_unit_extent = 14) {
  
  ## params
  last_date <- max(df$date, na.rm = TRUE)
  dates_extent <- c(last_date - (time_unit_extent - 1), last_date)
  
  ## get trends
  trends_all <- df %>% 
    dplyr::group_by(iso_a3) %>% 
    tidyr::nest() %>% 
    dplyr::mutate(model = map(data, model_trends, dates_extent)) %>% 
    dplyr::select(-data) %>% 
    tidyr::unnest(model) 
  
  df %>%
    dplyr::group_by(continent, region, country, iso_a3) %>%
    dplyr::summarise(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(trends_all)
}

model_trends <- function(x,
                         dates_extent,
                         ma_window = 3,
                         min_sum = 30) {
  
  # filter data to date range of interest
  xsub <- x %>% 
    filter(between(date, dates_extent[1], dates_extent[2])) %>% 
    tidyr::complete(
      date = seq.Date(min(date, na.rm = TRUE), max(date, na.rm = TRUE), by = 1), 
      fill = list(cases = NA_real_, deaths = NA_real_)
    )
  
  tibble::tibble(
    trend_cases = get_trend(xsub, "cases", min_sum, ma_window),
    trend_deaths = get_trend(xsub, "deaths", min_sum, ma_window)
  )
}

get_trend <- function(xsub, var, min_sum, ma_window) {
  if (nrow(xsub) > ma_window & sum(xsub[[var]], na.rm = TRUE) > min_sum) {
    # moving average
    xsub$ma <- as.numeric(forecast::ma(xsub[[var]], order = ma_window))
    xsub$ma <- dplyr::na_if(xsub$ma, 0) # PB: I don't think this is good idea, but kept from prev code
    
    # linear model and confidence intervals
    mdl <- lm(log(ma) ~ date, data = xsub)
    ci80 <- confint(mdl, level = 0.80)
    ci95 <- confint(mdl, level = 0.95)
    
    # prep output
    tibble::tibble(
      coeff = coefficients(mdl)[[2]], 
      lwr80  = ci80[2,1], 
      upr80  = ci80[2,2],
      lwr95  = ci95[2,1], 
      upr95  = ci95[2,2]
    ) %>% 
      mutate(
        trend = case_when(
          lwr95 > 0 ~ "Increasing",
          lwr95 <= 0 & lwr80 > 0 ~ "Likely increasing",
          upr95 < 0 ~ "Decreasing",
          upr95 >= 0 & upr80 < 0  ~ "Likely decreasing",
          lwr80 < 0 & upr80 > 0 ~ "Stable",
          TRUE ~ NA_character_
        )
      ) %>% 
      pull(trend)
  } else {
    NA_character_
  }
} 
