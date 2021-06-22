## code to prepare `data` dataset goes here

library(rnaturalearth)
library(sf)
library(dplyr)
library(stringr)
library(here)
library(fs)

world_map_raw <- rnaturalearth::ne_countries(scale = "small", type = "countries", returnclass = "sf")

sf_world <- world_map_raw %>% 
  select(country = name_long, iso_a3, iso_a2, pop_est) %>% 
  mutate(continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
         region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region")) %>% 
  filter(str_detect(country, "Antarctic", negate = TRUE)) %>% 
  cbind(st_coordinates(st_centroid(., of_largest_polygon = TRUE))) %>% 
  rename(lon = X, lat = Y)

usethis::use_data(sf_world, overwrite = TRUE)

geo_ref <- countrycode::codelist_panel %>% 
  dplyr::select(iso3c, continent, region, country = country.name.en) %>% 
  dplyr::distinct() %>% 
  tibble::as_tibble()

usethis::use_data(geo_ref, overwrite = TRUE)
