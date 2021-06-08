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
pkgload::load_all()

#TODO remove from dashboard as no longer being updated
# df_interventions <- get_interventions_data()

# not working when run on server
# running locally and sending to server for now
# df_ecdc <- get_ecdc_weekly()

# 
df_jhcsse <- get_owid_jhcsse()
df_trends <- get_country_summaries(df_jhcsse)

# old trends
# df_trends <- get_trends_data(df_jhcsse)
# new trends
# df_trends_new <- get_trends_data_new(df_jhcsse)

data_updated <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")

# save as package data
usethis::use_data(df_jhcsse, df_trends, data_updated, overwrite = TRUE)

save(df_trends, file = fs::path(linelist_dir, "df_trends.rda"))
# save(df_trends_new, file = fs::path(linelist_dir, "df_trends_new.rda"))

save(df_trends, file = fs::path(linelist_dir, "df_trends.rda"))
save(df_trends_new, file = fs::path(linelist_dir, "df_trends_new.rda"))

if (is_server) {
  # remove old cache files
  file.remove(list.files(file.path("/srv/shiny-server/covid19", ".rcache"), full.names = TRUE))
  # launch new session of app for new connections
  system("touch restart.txt")
  # also save trends data into linelist dashboard package
  # save(df_trends, file = fs::path(linelist_dir, "df_trends.rda"))
  
  # make static map =============================================
  # load(here::here("data", "sf_world.rda"))
  # source(here::here("R", "make_map.R"))
  # source(here::here("R", "utils_leaflet.R"))
  # Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
  # library(leaflet)
  # # df_trends <- get_trends_data_new(df_jhcsse)
  # latest_date <- max(df_jhcsse$date, na.rm = TRUE)
  # leafmap <- make_map(df_trends, sf_world, latest_date)
  # leafmap <- make_map(df_trends, sf_world, latest_date)
  # #local output folder
  # local_output <- "/home/epicentre/static_reports/covid_map"
  # fname <- fs::path(local_output, "index", ext = "html")
  # htmlwidgets::saveWidget(
  #   leafmap, 
  #   file = fname, 
  #   libdir = fs::path(local_output, "libs"),
  #   title = paste("Epicentre COVID-19 Case Trends"),
  #   background = "#FFFAFA",
  #   selfcontained = TRUE
  # )
  # # copy to nginx folder to be served by webserver
  # file.copy(fname, "/usr/share/nginx/html/covid-map/", overwrite = TRUE)
} else {
  file.remove(list.files(here::here(".rcache"), full.names = TRUE))
}