library(tidyverse)
library(readxl)
library(fs)
library(here)
library(janitor)
library(lubridate)
library(here)
library(DT)
library(writexl)

# ==========================================================================
# Setup
# ==========================================================================

dir_data <- here::here("data-raw")

# ==========================================================================
# Read data
# ==========================================================================

#
# ECDC
# download from : https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
# direct access to data (change date) https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-17.xlsx
# I don't think there is yet a package to download automatically. Also format slightly changed recently so might not be stable.
# I put several examples in data-raw so you can see it changed

# this doesn't seem to be working :(
# ecdc_url <- glue::glue("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-{Sys.Date()}.xls")
# download.file(ecdc_url, destfile = here::here("data-raw/ecdc_data.xlsx"))

file_data_ecdc <- dir_ls(path(dir_data, "ecdc"), regexp = "COVID-19-geographic-disbtribution-worldwide-") %>% max()

df_data_ecdc <- read_excel(file_data_ecdc) %>%
  janitor::clean_names() %>%
  mutate(
    date_rep = as.Date(date_rep), 
    source = "ECDC", 
    countries_and_territories = countries_and_territories %>% recode(`United_Kingdom` = "United Kingdom")
  ) %>%
  rename(n_cases = cases, n_deaths = deaths, country = countries_and_territories) %>%
  pivot_longer(cols = c(n_cases, n_deaths), names_to = "time_series", values_to = "value") %>%
  group_by(country, time_series) %>%
  arrange(date_rep) %>%
  mutate(value_cum = cumsum(value)) %>%
  ungroup()

#
# JHU
# I just cloned their data repo and sync every day: 	https://github.com/CSSEGISandData/COVID-19
# data stored as csv

jhu_conf_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
jhu_death_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"

df_data_jhu_conf <- read_csv(jhu_conf_url) %>%
  rename(province_state = `Province/State`, country_region = `Country/Region`, lat = Lat, long = Long) %>%
  pivot_longer(cols = -c(province_state, country_region, lat, long), names_to = "date_rep", values_to = "value_cum") %>%
  mutate(time_series = "n_cases")

df_data_jhu_death <- read_csv(jhu_death_url) %>%
  rename(province_state = `Province/State`, country_region = `Country/Region`, lat = Lat, long = Long) %>%
  pivot_longer(cols = -c(province_state, country_region, lat, long), names_to = "date_rep", values_to = "value_cum") %>%
  mutate(time_series = "n_deaths")

df_data_jhu <- df_data_jhu_conf %>%
  bind_rows(df_data_jhu_death) %>%
  mutate(date_rep = lubridate::mdy(date_rep), source = "JHU") %>%
  group_by(province_state, country_region, time_series) %>%
  arrange(date_rep) %>%
  mutate(value = c(0, diff(value_cum))) %>%
  ungroup() %>%
  arrange(country_region, province_state, date_rep, time_series)

df_data_jhu_country <- df_data_jhu %>%
  group_by(country_region, date_rep, time_series, source) %>%
  summarize(value = sum(value), value_cum = sum(value_cum)) %>%
  ungroup() %>%
  rename(country = country_region)

#
# SPF: Santé Publique France

read_spf_yaml <- function(path) {
  ydat <- yaml::read_yaml(path)
  tibble::tibble(
    date_rep = as.Date(ydat$date),
    country = "France",
    source = "SPF",
    n_cases = ydat$donneesNationales$casConfirmes,
    n_deaths = ydat$donneesNationales$deces
  )
}

read_spf_regions_yaml <- function(path) {
  ydat <- yaml::read_yaml(path)
  regions <- ydat$donneesRegionales
  
  tibble::tibble(
    date_rep = as.Date(ydat$date),
    country = "France",
    source = "SPF",
    region = map_chr(regions, "nom"),
    code = map_chr(regions, "code"),
    n_cases = map_int(regions, "casConfirmes")
  )
}

# github api url to list contents of folder (daily data is stored in separate yaml files)
spf_url <- "https://api.github.com/repos/opencovid19-fr/data/contents/sante-publique-france"
spf_ls <- httr::GET(spf_url) %>% httr::content() %>% map_chr("download_url")

df_data_spf <- spf_ls %>%
  map_dfr(read_spf_yaml) %>%
  arrange(date_rep) %>%
  mutate_if(is.numeric, ~ c(.[1], diff(.))) %>%
  pivot_longer(cols = starts_with("n_"), names_to = "time_series", values_to = "value") %>%
  group_by(country, time_series) %>%
  arrange(time_series, date_rep) %>%
  mutate(value_cum = cumsum(value)) %>%
  ungroup()

df_data_spf_regions <- spf_ls %>%
  map_dfr(read_spf_regions_yaml) %>%
  arrange(date_rep) %>%
  group_by(region) %>% 
  mutate_if(is.numeric, ~ c(.[1], diff(.))) %>%
  ungroup() %>% 
  pivot_longer(cols = starts_with("n_"), names_to = "time_series", values_to = "value") %>%
  group_by(region, time_series) %>%
  arrange(time_series, date_rep) %>%
  mutate(value_cum = cumsum(value)) %>%
  ungroup()


#
# WHO sitrep
# R package but failed at installation last time so
# I just cloned the reporting and sync every day: https://github.com/eebrown/data2019nCoV
# the data is stored as an R object

who_url <- "https://raw.githubusercontent.com/eebrown/data2019nCoV/master/data/sarscov2_who_2019.rda"
download.file(who_url, destfile = here::here("data-raw/sarscov2_who_2019.rda"))
load(here::here("data-raw/sarscov2_who_2019.rda"))

df_data_who <- sarscov2_who_2019 %>%
  as_tibble() %>%
  select(date_rep = date, starts_with("cases"), starts_with("deaths")) %>%
  pivot_longer(
    cols = -date_rep,
    names_to = c("time_series", "country_abr", "region"),
    names_sep = "_",
    values_to = "value_cum"
  ) %>%
  group_by(time_series, country_abr, region) %>%
  arrange(date_rep) %>%
  mutate(value = c(0, diff(value_cum))) %>%
  ungroup() %>%
  arrange(country_abr, region, date_rep, time_series) %>%
  mutate(
    country = recode(country_abr,
      fra = "France",
      ita = "Italy",
      chn = "China"
    ),
    region = str_to_title(region),
    time_series = sprintf("n_%s", time_series),
    source = "WHO"
  )


#
# Lockdown
# Manual file but I'm sure there is someone somewhere who is summarizing this information

df_data_intervention <- read_excel(path(dir_data, "data_intervention.xlsx")) %>% mutate_at(vars(starts_with("date")), as.Date)

