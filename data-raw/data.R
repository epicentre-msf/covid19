if (Sys.info()["nodename"] == "vps709766") {
  setwd("/srv/shiny-server/covid19")
  linelist_dir <- "/home/epicentre/covid-linelist-dashboard/data"
} else {
  setwd(here::here())
  linelist_dir <- "~/epicentre/covid.linelist.dashboard/data"
}

library(tidyverse)
pkgload::load_all()

NCoVUtils::reset_cache(refresh_data = FALSE)

df_interventions <- get_interventions_data()

df_ecdc <- get_ecdc_data()

df_trends <- get_trends_data(df_ecdc)

#this is no longer being updated
#df_who <- get_who_data()

df_jhcsse <- get_jhcsse_data()

data_updated <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")

# save as package data
usethis::use_data(df_interventions, df_ecdc, df_trends, df_jhcsse, data_updated, overwrite = TRUE)

# launch new session of app for new connections
system("touch restart.txt")

# also save trends data into linelist dashboard package
save(df_trends, file = fs::path(linelist_dir, "df_trends.rda"))

