
# dplyr::distinct(countrycode::codelist_panel, continent, region)
# region_selects <- dplyr::distinct(sf_world %>% sf::st_set_geometry(NULL), continent, region)
# region_selects <- c("World" = list("All"), split(region_selects$region, region_selects$continent))

continents <- c("Africa", "Asia", "Americas", "Europe", "Oceania")

world_regions <- c(
  "Caribbean", "Southern Asia", "Southern Europe", "Northern Africa", 
  "Middle Africa", "South America", "Western Asia", "Australia and New Zealand", 
  "Western Europe", "Western Africa", "Eastern Europe", "Central America", 
  "Southern Africa", "South-Eastern Asia", "Eastern Africa", "Northern America", 
  "Eastern Asia", "Northern Europe", "Melanesia", "Micronesia", 
  "Central Asia", "Polynesia"
)

all_continents <- tibble::tibble(continent = continents, region = continents)

country_iso <- dplyr::distinct(countrycode::codelist_panel, country.name.en, iso3c) %>% tidyr::drop_na(iso3c)
country_iso <- purrr::set_names(country_iso$iso3c, country_iso$country.name.en)

