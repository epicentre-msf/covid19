library(tidyverse)
library(jsonlite)
library(readxl)
library(reactable)

df_phsm <- get_phsm_data()
glimpse(df_phsm)

rt_details_row <- function() {
  reactable::JS(
    "function(rowInfo) {
           var cat = rowInfo.row['who_category'];
           var subcat = rowInfo.row['who_subcategory'];
           var measure = rowInfo.row['who_measure'];
           var comments = rowInfo.row['comments'];
           
           if (cat !== null) {
            var cat_html = '<b>Category: </b>' + cat + '</br>'
           } else {
            var cat_html = 'No category info available'
           }

           if (subcat !== null) {
            var subcat_html = '<b>Subcategory: </b>' + subcat + '</br>'
           } else {
            var subcat_html = ''
           }

           if (measure !== null) {
            var measure_html = '<b>Measure: </b>' + measure + '</br>'
           } else {
            var measure_html = ''
           }
           
           if (comments !== null) {
            var comments_html = '<b>Comments: </b>' + comments + '</br>'
           } else {
            var comments_html = ''
           }
           
           return '<div style = \"padding: 10px\">' + cat_html + subcat_html + measure_html + comments_html + '</div>';
         }"
  )
}

df_phsm %>%
  mutate(across(contains("date"), lubridate::as_date)) %>%
  filter(is.na(date_end)) %>% 
  slice_sample(n = 10) %>%
  select(
    iso,
    country_territory_area,
    admin_level,
    area_covered,
    who_category,
    who_subcategory,
    who_measure,
    comments,
    targeted,
    date_start,
    date_end,
    source
  ) %>%
  reactable(
    filterable = TRUE,
    #searchable = TRUE,
    details = reactable::colDef(
      name = "+",
      footer = "",
      details = rt_details_row(),
      html = TRUE
    ),
    columns = list(
      who_category = colDef(show = FALSE),
      who_subcategory = colDef(show = FALSE),
      who_measure = colDef(show = FALSE),
      comments = colDef(show = FALSE)
    )
  )

phsm_json <- jsonlite::fromJSON("https://extranet.who.int/xmart-api/odata/NCOV_PHM/CLEAN_PHSM")
phsm_json <- jsonlite::fromJSON(txt = "https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV_PHM/CLEAN_PHSM")
phsm_json <- jsonlite::fromJSON("~/Downloads/CLEAN_PHSM.json")
obj_lines <- readLines(con = "https://frontdoor-l4uikgap6gz3m.azurefd.net/NCOV_PHM/CLEAN_PHSM?$", encoding = "UTF-8")

xl_path <- "https://orlagh.blob.core.windows.net/release/release_2021_12_14.xlsx"

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