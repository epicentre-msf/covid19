
library(tidyverse)
library(COVID19)
library(geodata)

df_fr <- covid19(country = "France", level = 3)
glimpse(df_fr)

df_fr %>%
  filter(administrative_area_level_3 == "Gard") %>%
  ggplot(aes(date, confirmed)) +
  geom_col()

fr_3 <- gadm(
  country = "FRA",
  level = 2,
  path = here::here("data")
)

fr <- raster::getData(
  "GADM",
  country = "FRA", 
  level = 1, 
  path = tempdir()
)

download.file(
  url = "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_FRA_3_sf.rds", 
  destfile = "~/Downloads/gadm36_FRA_3_sf.rds",
  method = "libcurl", mode = "wb"
)
