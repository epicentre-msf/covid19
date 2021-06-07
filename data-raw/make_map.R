setwd("/srv/shiny-server/covid19")
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")
library(tidyverse)
library(leaflet)
pkgload::load_all()

latest_date <- max(df_jhcsse$date, na.rm = TRUE)
leafmap <- make_map(df_trends, sf_world, latest_date)
# local output folder
local_output <- "/home/epicentre/static_reports/covid_map"
fname <- fs::path(local_output, "index", ext = "html")
htmlwidgets::saveWidget(
  leafmap, 
  file = fname, 
  libdir = fs::path(local_output, "libs"),
  title = paste("Epicentre COVID-19 Case Trends"),
  background = "#FFFAFA",
  selfcontained = TRUE
)
# copy to nginx folder to be served by webserver
file.copy(fname, "/usr/share/nginx/html/covid-map/", overwrite = TRUE)
