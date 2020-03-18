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

# TODO: edit path see comments below
dir_data_jhu <- "COVID-19/csse_covid_19_data/csse_covid_19_time_series"
dir_data_who <- "data2019nCoV/data"

# ==========================================================================
# Read data
# ==========================================================================

#
# ECDC
# download from : https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
# direct access to data (change date) https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-2020-03-17.xlsx
# I don't think there is yet a package to download automatically. Also format slightly changed recently so might not be stable.
# I put several examples in data-raw so you can see it changed

file_data_ecdc <- dir_ls(dir_data, regexp = "COVID-19-geographic-disbtribution-worldwide-") %>% max

df_data_ecdc <- read_excel(file_data_ecdc) %>% janitor::clean_names() %>%
mutate(date_rep = as.Date(date_rep), source = "ECDC", countries_and_territories = countries_and_territories %>% recode(`United_Kingdom` = "United Kingdom")) %>% 
rename(n_cases = cases, n_deaths = deaths, country = countries_and_territories) %>% 
pivot_longer(cols = c(n_cases, n_deaths), names_to = "time_series", values_to = "value") %>% 
group_by(country, time_series) %>% arrange(date_rep) %>% mutate(value_cum = cumsum(value)) %>% ungroup

#
# JHU
# I just cloned their data repo and sync every day: 	https://github.com/CSSEGISandData/COVID-19
# data stored as csv

df_data_jhu_conf <- read_csv(path(dir_data_jhu, "time_series_19-covid-Confirmed.csv")) %>%
rename(province_state = `Province/State`, country_region = `Country/Region`, lat = Lat, long = Long) %>% 
pivot_longer(cols = -c(province_state, country_region, lat, long), names_to = "date_rep", values_to = "value_cum") %>% 
mutate(time_series = "n_cases")

df_data_jhu_death <- read_csv(path(dir_data_jhu, "time_series_19-covid-Deaths.csv")) %>%
rename(province_state = `Province/State`, country_region = `Country/Region`, lat = Lat, long = Long) %>% 
pivot_longer(cols = -c(province_state, country_region, lat, long), names_to = "date_rep", values_to = "value_cum") %>% 
mutate(time_series = "n_deaths")

df_data_jhu <- df_data_jhu_conf %>% bind_rows(df_data_jhu_death) %>% mutate(date_rep = lubridate::mdy(date_rep), source = "JHU") %>% 
group_by(province_state, country_region, time_series) %>% 
arrange(date_rep) %>% mutate(value = c(0, diff(value_cum))) %>% ungroup %>% arrange(country_region, province_state, date_rep, time_series)

df_data_jhu_country <- df_data_jhu %>% group_by(country_region, date_rep, time_series, source) %>% summarize(value = sum(value), value_cum = sum(value_cum)) %>% ungroup %>% rename(country = country_region)

#
# SPF: Santé Publique France
# I scrapped the data on a website but actually found a separate data repo : https://github.com/opencovid19-fr/data
# There is already a dashboard: https://veille-coronavirus.fr
df_data_spf <- read_excel(path(dir_data, "data_spf.xlsx")) %>% mutate(date_rep = as.Date(date_rep), source = "SPF") %>% 
pivot_longer(cols = n_cases, names_to = "time_series", values_to = "value") %>% 
group_by(country, time_series) %>% arrange(date_rep) %>% mutate(value_cum = cumsum(value)) %>% ungroup


#
# WHO sitrep
# R package but failed at installation last time so
# I just cloned the reporting and sync every day: https://github.com/eebrown/data2019nCoV
# the data is stored as an R object
load(path(dir_data_who, "sarscov2_who_2019.rda"))

df_data_who <- sarscov2_who_2019 %>% as_tibble %>% select(date_rep = date, starts_with("cases"), starts_with("deaths")) %>% 
pivot_longer(
	cols = -date_rep,
  names_to = c("time_series", "country_abr", "region"), 
  names_sep = "_",
  values_to = "value_cum"
) %>%
group_by(time_series, country_abr, region) %>% 
arrange(date_rep) %>% 
mutate(value = c(0, diff(value_cum))) %>% ungroup %>% 
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


# ==========================================================================
# Some random plots
# ==========================================================================

#
# Compare countries
#

lag_FR <- 8
lag_IT <- 0
lag_UK <- 14

df_data_ecdc %>% count(country) %>% as.data.frame
	
df_data_ecdc %>% mutate( 
	date_rep = case_when(
	country == "France" ~ date_rep - lag_FR,
	country == "Italy" ~ date_rep - lag_IT,
	country == "United Kingdom" ~ date_rep - lag_UK,
	TRUE ~ date_rep
)) %>% filter(country %in% c("France", "Italy", "United Kingdom"), date_rep > as.Date("2020-02-15")) %>%
ggplot(aes(x = date_rep, y =  value, colour = country)) + facet_wrap(~time_series, scales = "free_y", ncol = 1) + geom_line() + geom_point(aes(shape = country)) + theme_light() + scale_y_log10()

#
# Compare lockdown countries
#

date_lockdown_china <- df_data_intervention %>% filter(country == "China", intervention == "Lockdown") %>% .$date_start
df_data_inter_plot <- df_data_intervention %>% filter(intervention == "Lockdown", !region %in% c("Lombardie")) %>% 
mutate(lag = date_start - date_lockdown_china, date_start_rescaled = date_start - lag)

lag_FR <- df_data_inter_plot %>% filter(country == "France") %>% .$lag
lag_IT <- df_data_inter_plot %>% filter(country == "Italy") %>% .$lag

df_data_plot <- df_data_who %>% filter(country %in% c("Italy", "France", "China") & is.na(region), value < 10000, date_rep > as.Date("2020-01-01")) %>% 
mutate(
date_rep = case_when(
	country == "France" ~ date_rep - lag_FR,
	country == "Italy" ~ date_rep - lag_IT,
	country == "China" ~ date_rep,
	TRUE ~ date_rep
)) 

p <- ggplot(df_data_plot) + facet_wrap(~time_series, scales = "free_y", ncol = 1) 
p <- p + geom_line(data = df_data_plot, aes(x = date_rep, y = value, colour = country)) 
p <- p + geom_vline(data = df_data_inter_plot, aes(xintercept = date_start_rescaled))
p <- p + theme_light() + xlim(as.Date(c("2020-01-01", NA)))
print(p)


#
# Compare sources
#

df_data_compare <- df_data_ecdc %>% bind_rows(df_data_jhu_country, df_data_spf, df_data_who)

df_data_ecdc %>% filter(country == "France")

## France

df_data_compare %>% filter(country == "France", date_rep > as.Date("2020-02-20")) %>% 
ggplot(aes(x = date_rep, y = value, colour = source)) + facet_wrap(~time_series, scales = "free_y", ncol = 1) + geom_line() + theme_light() + scale_colour_brewer(palette = "Dark2")
	