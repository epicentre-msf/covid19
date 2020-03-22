## code to prepare `data` dataset goes here

library(rnaturalearth)
library(eurostat)
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

#sf_iran <- rnaturalearth::ne_states(country = "Iran", returnclass = "sf")

sf_europe_1 <- eurostat::get_eurostat_geospatial(output_class = "sf", nuts_level = 1)
sf_europe_2 <- eurostat::get_eurostat_geospatial(output_class = "sf", nuts_level = 2)
#sf_europe_3 <- eurostat::get_eurostat_geospatial(output_class = "sf", nuts_level = 3)


# France ====================================================

# for matching between eurostat ids and region codes in data
fr_match <- tibble::tribble(
  ~id,       ~region_code,   ~region,
  "FRK",     "REG-84",       "Auvergne-Rhône-Alpes",
  "FRC",     "REG-27",    "Bourgogne-Franche-Comté",
  "FRH",     "REG-53",                   "Bretagne",
  "FRB",     "REG-24",        "Centre-Val de Loire",
  "FRM",     "REG-94",                      "Corse",
  "FRF",     "REG-44",                  "Grand Est",
  "FRE",     "REG-32",            "Hauts-de-France",
  "FR1",     "REG-11",              "Ile-de-France",
  "FRD",     "REG-28",                  "Normandie",
  "FRI",     "REG-75",         "Nouvelle-Aquitaine",
  "FRJ",     "REG-76",                  "Occitanie",
  "FRG",     "REG-52",           "Pays de la Loire",
  "FRL",     "REG-93", "Provence-Alpes-Côte d’Azur"
)

sf_fr <- sf_europe_1 %>% 
  filter(CNTR_CODE == "FR", NUTS_ID != "FRY") %>% 
  left_join(select(fr_match, id, region_code), by = "id") %>% 
  cbind(st_coordinates(st_centroid(., of_largest_polygon = TRUE))) %>% 
  rename(lon = X, lat = Y)

# Italy ====================================================

it_match <- tibble::tribble(
     ~id, ~region_code,                 ~region,
  "ITF1",           13,               "Abruzzo",
  "ITF5",           17,            "Basilicata",
  "ITF6",           18,              "Calabria",
  "ITF3",           15,              "Campania",
  "ITH5",            8,        "Emilia Romagna",
  "ITH4",            6, "Friuli Venezia Giulia",
  "ITI4",           12,                 "Lazio",
  "ITC3",            7,               "Liguria",
  "ITC4",            3,             "Lombardia",
  "ITI3",           11,                "Marche",
  "ITF2",           14,                "Molise",
  "ITH1",            4,          "P.A. Bolzano",
  "ITH2",            4,           "P.A. Trento",
  "ITC1",            1,              "Piemonte",
  "ITF4",           16,                "Puglia",
  "ITG2",           20,              "Sardegna",
  "ITG1",           19,               "Sicilia",
  "ITI1",            9,               "Toscana",
  "ITI2",           10,                "Umbria",
  "ITC2",            2,         "Valle d'Aosta",
  "ITH3",            5,                "Veneto"
)

sf_it <- sf_europe_2 %>% 
  filter(CNTR_CODE == "IT") %>% 
  left_join(select(it_match, id, region_code), by = "id") %>% 
  cbind(st_coordinates(st_centroid(., of_largest_polygon = TRUE))) %>% 
  rename(lon = X, lat = Y)

usethis::use_data(sf_world, sf_fr, sf_it, overwrite = TRUE)
