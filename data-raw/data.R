## code to prepare `data` dataset goes here

library(rnaturalearth)
library(dplyr)
library(stringr)
library(here)
library(fs)

world_map_raw <- rnaturalearth::ne_countries(scale = "medium", type = "countries", returnclass = "sf")

sf_world <- world_map_raw %>% 
  select(country = name_long, iso_a3, pop_est) %>% 
  filter(str_detect(country, "Antarctic", negate = TRUE))

plot(sf_world$geometry)

# France Regions

fr_path <- here("data-raw/shps/fr_shp")

if (!dir_exists(fr_path)) {
  fr_url <- "https://www.data.gouv.fr/en/datasets/r/aacf9338-8944-4513-a7b9-4cd7c2db2fa9"
  fr_zip <- here("data-raw/shps/fr_shp.zip")
  dir_create(fr_path, recurse = TRUE)
  download.file(fr_url, destfile = fr_zip)
  utils::unzip(fr_zip, exdir = fr_path)
}

sf_france <- sf::st_read(dsn = fr_path, layer = "regions-20180101")

ggplot(sf_france) +
  geom_sf()

plot(sf_france$geometry)


usethis::use_data("data")
