library(tidyverse)
library(growthrates)
library(forecast)
library(sandwich)
pkgload::load_all()

model_cnt_cases_linear  <- make_model_cnt_linear(df = df_ecdc, series = 'cases', time_unit_extent = 12, min_sum = 30)
model_cnt_deaths_linear  <- make_model_cnt_linear(df = df_ecdc, series = 'deaths', time_unit_extent = 12, min_sum = 30)

o_vars <-  c("coeff", "lwr", "upr")
l_cnt_vars <- setNames(as.vector(o_vars), paste0('l_cnt_', o_vars))

df_cases <- rename(model_cnt_cases_linear[[3]] , all_of(l_cnt_vars)) %>% 
  mutate(
    trend_linear = case_when(
      l_cnt_lwr > 0 ~ "increasing", 
      l_cnt_upr < 0 ~ "declining", 
      l_cnt_upr > 0 & l_cnt_lwr < 0 ~ "stable", 
      TRUE ~ NA_character_) %>% 
      factor(levels = c('increasing', 'stable','declining'), 
             labels = c('Increasing', 'Stable','Declining'))
  ) %>% 
  select(iso_a3, trend_cases = trend_linear)

df_deaths <- rename(model_cnt_deaths_linear[[3]] , all_of(l_cnt_vars)) %>% 
  mutate(
    trend_linear = case_when(
      l_cnt_lwr > 0 ~ "increasing", 
      l_cnt_upr < 0 ~ "declining", 
      l_cnt_upr > 0 & l_cnt_lwr < 0 ~ "stable", 
      TRUE ~ NA_character_) %>% 
      factor(levels = c('increasing', 'stable','declining'), 
             labels = c('Increasing', 'Stable','Declining'))
  ) %>% 
  select(iso_a3, trend_deaths = trend_linear)

df_trends <- distinct(df_ecdc, continent, region, country, iso_a3) %>% 
  inner_join(full_join(df_cases, df_deaths))
  
usethis::use_data(df_trends, overwrite = TRUE)

if (Sys.info()["nodename"] == "Pauls-MacBook-Pro.local") {
  df_trends %>% 
    select(iso = iso_a3, trend = trend_cases) %>% 
    write_csv(paste0("~/MSF/GIS @ MSF - EPI csv data/epi-case-trends-", Sys.Date(), ".csv"))
}

