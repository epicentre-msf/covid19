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

#TODO remove from dashboard as no longer being updated
# df_interventions <- get_interventions_data()

# not working when run on server
# running locally and sending to server for now
# df_ecdc <- get_ecdc_weekly()

df_jhcsse <- get_owid_jhcsse()
df_trends <- get_country_summaries(df_jhcsse)

data_updated <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")

# save as package data
usethis::use_data(df_jhcsse, df_trends, data_updated, overwrite = TRUE)

save(df_trends, file = fs::path(linelist_dir, "df_trends.rda"))

if (is_server) {
  # remove old cache files
  file.remove(list.files(file.path("/srv/shiny-server/covid19", ".rcache"), full.names = TRUE))
  # launch new session of app for new connections
  system("touch restart.txt")
} else {
  file.remove(list.files(here::here(".rcache"), full.names = TRUE))
}