#' Import interventions dataset
#' 
#' @export
get_interventions_data <- function() {
  
  interventions_url <- paste0("https://data.humdata.org/dataset/e1a91ae0-292d-4434-bc75-bf863d4608ba/", 
                              "resource/8a98c9cd-2c49-41fb-9a8e-6c76821c4d72/download/", 
                              "20200317-acaps-covid-19_goverment-measures-dataset.xlsx")
  
  temp <- tempdir()
  filename <- "interventions.xlsx"
  curl::curl_download(interventions_url, destfile = fs::path(temp, filename))

  readxl::read_excel(fs::path(temp, filename), sheet = "Database") %>% 
    janitor::clean_names() %>% 
    dplyr::mutate_if(lubridate::is.POSIXct, lubridate::as_date)
}


#' Import ECDC dataset
#' 
#' @export
get_ecdc_data <- function() {
  
  df_ecdc <- NCoVUtils::get_ecdc_cases() %>% 
    dplyr::mutate(geoid = dplyr::case_when(
      country == "United_Kingdom" ~ "GB",
      country == "Greece" ~ "GR",
      country == "French_Polynesia" ~ "PF",
      TRUE ~ geoid
    )) %>% 
    dplyr::rename(country_ecdc = country) %>% 
    dplyr::mutate(
      country = countrycode::countrycode(geoid, origin = "iso2c", destination = "country.name"),
      continent = countrycode::countrycode(geoid, origin = "iso2c", destination = "continent"),
      region = countrycode::countrycode(geoid, origin = "iso2c", destination = "region"),
      iso_a3 = countrycode::countrycode(geoid, origin = "iso2c", destination = "iso3c"),
      source = "ECDC"
    ) %>% 
    dplyr::select(date, country_ecdc:geoid, country:iso_a3, cases, deaths, source)

}


#' Import WHO dataset
#' 
#' @export
get_who_data <- function() {
  
  who_url <- "https://raw.githubusercontent.com/eebrown/data2019nCoV/master/data/sarscov2_who_2019.rda"
  temp <- tempdir()
  filename <- "sarscov2_who_2019.rda"
  curl::curl_download(who_url, destfile = fs::path(temp, filename))
  load(fs::path(temp, filename))
  
  df_who <- sarscov2_who_2019 %>%
    tibble::as_tibble() %>%
    dplyr::select(date, starts_with("cases"), starts_with("deaths")) %>%
    tidyr::pivot_longer(
      cols = -date,
      names_to = c("ind", "iso_a3", "region"),
      names_sep = "_",
      values_to = "value"
    ) %>%
    dplyr::group_by(ind, iso_a3, region) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(value = c(value[1], diff(value))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(iso_a3, region, date, ind) %>%
    tidyr::pivot_wider(names_from = "ind", values_from = "value") %>% 
    dplyr::filter(is.na(region)) %>% 
    dplyr::select(-region) %>% 
    dplyr::mutate(iso_a3 = stringr::str_to_upper(iso_a3), 
           country = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "country.name"),
           continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
           region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region"),
           source = "WHO") %>% 
    dplyr::filter(!iso_a3 %in% c("GLOBAL", "INTERNATIONALCONVEYANCE")) %>% 
    dplyr::select(date, country:region, dplyr::everything())
}




