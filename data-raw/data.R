if (Sys.info()["nodename"] == "vps709766") {
  is_server <- TRUE
  setwd("/srv/shiny-server/covid19")
  linelist_dir <- "/home/epicentre/covid-linelist-dashboard/data"
} else {
  is_server <- FALSE
  setwd(here::here())
  linelist_dir <- "~/epicentre/covid.linelist.dashboard/data"
}

library(tidyverse)
library(covidutils)

get_who_daily <- function(path = "https://covid19.who.int/WHO-COVID-19-global-data.csv") {
  df_who_raw <- readr::read_csv(path)
  df_who <- df_who_raw %>%
    janitor::clean_names() %>%
    dplyr::transmute(
      date = date_reported,
      continent = countrycode::countrycode(country_code, "iso2c", "continent"),
      country = country,
      region = countrycode::countrycode(country_code, "iso2c", "region23"),
      iso_a3 = countrycode::countrycode(country_code, "iso2c", "iso3c"),
      cases = new_cases,
      deaths = new_deaths
    ) %>%
    dplyr::mutate(
      across(c(cases, deaths), ~dplyr::if_else(.x < 0, 0, .)),
      country = stringr::str_remove(country, "\\[1\\]$"),
      continent = dplyr::case_when(
        country %in% c("Bonaire", "Saba", "Sint Eustatius") ~ "Americas",
        country %in% c("Kosovo") ~ "Europe",
        TRUE ~ continent
      ),
      region = dplyr::case_when(
        country %in% c("Bonaire", "Saba", "Sint Eustatius") ~ "Caribbean",
        country %in% c("Kosovo") ~ "Southern Europe",
        TRUE ~ region
      ),
      iso_a3 = dplyr::case_when(
        country %in% c("Bonaire", "Saba", "Sint Eustatius") ~ "BES",
        country %in% c("Kosovo") ~ "XKX",
        TRUE ~ iso_a3
      )
    )
    return(df_who)
}

df_who <- get_who_daily()
df_trends <- get_country_summaries(df_who)

# no longer being updated
# source(here::here("R", "get_data.R"))
# df_phsm <- get_phsm_data()
# usethis::use_data(df_phsm, overwrite = TRUE)

data_updated <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")

# save as package data
usethis::use_data(df_who, df_trends, data_updated, overwrite = TRUE)

save(df_trends, file = fs::path(linelist_dir, "df_trends.rda"))

if (is_server) {
  # remove old cache files
  file.remove(list.files(file.path("/srv/shiny-server/covid19", ".rcache"), full.names = TRUE))
  # launch new session of app for new connections
  system("touch restart.txt")
} else {
  file.remove(list.files(here::here(".rcache"), full.names = TRUE))
}
