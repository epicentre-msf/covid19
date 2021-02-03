# running locally on Paul's laptop
setwd("~/epicentre/covid19")
library(tidyverse)
pkgload::load_all()
df_ecdc <- get_ecdc_weekly()
usethis::use_data(df_ecdc, overwrite = TRUE)
system("rsync ~/epicentre/covid19/data/df_ecdc.rda shinyproxy:/srv/shiny-server/covid19/data/")
