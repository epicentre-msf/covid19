library(cronR)

r <- cron_rscript(here::here("data-raw/data.R"))
cron_add(r, frequency = "hourly", description = "update covid data")

# run ecdc script locally
r <- cron_rscript(here::here("data-raw/ecdc.R"))
cron_add(
  r, 
  frequency = "daily", 
  at = "11am", 
  days_of_week = 5, 
  description = "update weekly ecdc data and send to server",
  user = "paul"
)

cron_ls()
