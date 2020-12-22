if (Sys.info()["nodename"] == "vps709766") {
  is_server <- TRUE
  setwd("/srv/shiny-server/covid19")
  linelist_dir <- "/home/epicentre/covid-linelist-dashboard/data"
} else {
  is_server <- FALSE
  setwd(here::here())
}

library(tidyverse)
pkgload::load_all()

df_interventions <- get_interventions_data()

df_ecdc <- get_ecdc_weekly()

df_jhcsse <- get_owid_jhcsse()

df_trends <- get_trends_data(df_jhcsse)

data_updated <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")

# save as package data
usethis::use_data(df_interventions, df_ecdc, df_trends, df_jhcsse, data_updated, overwrite = TRUE)

if (is_server) {
  # remove old cache files
  file.remove(list.files(file.path("/srv/shiny-server/covid19", ".rcache"), full.names = TRUE))
  # launch new session of app for new connections
  system("touch restart.txt")

  # also save trends data into linelist dashboard package
  save(df_trends, file = fs::path(linelist_dir, "df_trends.rda"))
} else {
  file.remove(list.files(here::here(".rcache"), full.names = TRUE))
}