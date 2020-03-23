library(cronR)

r <- cron_rscript(here::here("data-raw/data.R"))

cron_add(r, frequency = "hourly", description = "update covid data")