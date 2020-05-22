if (Sys.info()["nodename"] == "vps709766") {
  setwd("/srv/shiny-server/covid19")
} else {
  setwd(here::here())
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

usethis::use_data(df_interventions, df_ecdc, df_trends, df_jhcsse, data_updated, overwrite = TRUE)

system("touch restart.txt")
