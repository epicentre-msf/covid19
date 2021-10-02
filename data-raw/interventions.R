
library(tidyverse)
library(jsonlite)

phsm_json <- jsonlite::fromJSON("https://extranet.who.int/xmart-api/odata/NCOV_PHM/CLEAN_PHSM")
phsm_json <- jsonlite::fromJSON("https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV_PHM/CLEAN_PHSM")
df_phsm_raw <- readr::read_csv("https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV_PHM/CLEAN_PHSM?$format=csv")

count(df_phsm_raw, COUNTRY_TERRITORY_AREA, sort = TRUE)

write_excel_csv(df_phsm_raw, "~/Downloads/covid_phsm.csv")

tibble::as_tibble(phsm_json$value) %>% 
  janitor::clean_names() %>% 
  dplyr::select(
    iso = country_code,
    country = country_territory_area,
    admin_level,
    category = who_category,
    sub_category = who_subcategory,
    measure = who_measure,
    measure_stage,
    targeted,
    date_start,
    date_end,
    date_entry,
    comments,
    source,
    source_type
  ) %>% 
  dplyr::mutate(
    #country = countrycode::countrycode(iso, origin = "iso3c", destination = "country.name"),
    #country = dplyr::case_when(country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", TRUE ~ country),
    continent = countrycode::countrycode(iso, origin = "iso3c", destination = "continent"),
    region = countrycode::countrycode(iso, origin = "iso3c", destination = "region23"),
    .before = iso
  )