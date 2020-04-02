#' Import interventions dataset
#' 
#' @export
get_interventions_data <- function() {
  
  base_url <- "https://data.humdata.org"
  dl_url <- xml2::read_html(paste0(base_url, "/dataset/acaps-covid19-government-measures-dataset#")) %>% 
    rvest::html_node(css = ".ga-download") %>% 
    rvest::html_attr("href") %>% 
    xml2::url_absolute(base_url)
  
  temp <- tempdir()
  filename <- "interventions.xlsx"
  curl::curl_download(dl_url, destfile = fs::path(temp, filename))

  readxl::read_excel(fs::path(temp, filename), sheet = "Database") %>% 
    janitor::clean_names() %>% 
    dplyr::mutate_if(lubridate::is.POSIXct, lubridate::as_date) %>% 
    dplyr::mutate(
      country = countrycode::countrycode(iso, origin = "iso3c", destination = "country.name"),
      # countrycode gives DRC the name of 'Congo - Kinshasa' for some reason? fix this
      country = dplyr::case_when(country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", TRUE ~ country),
      continent = countrycode::countrycode(iso, origin = "iso3c", destination = "continent"),
      region = countrycode::countrycode(iso, origin = "iso3c", destination = "region")
    ) 
}


#' Import ECDC dataset
#' 
#' @export
get_ecdc_data <- function() {
  
  base_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  
  d <- readr::read_csv(base_url) %>%
    dplyr::mutate(date = as.Date(dateRep, format = "%d/%m/%Y")) %>%
    dplyr::rename(geoid = geoId, country_ecdc = countriesAndTerritories, iso_a3 = countryterritoryCode, population_2018 = popData2018) %>%
    dplyr::select(-dateRep) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~ifelse(. < 0, 0L, .)) %>% 
    dplyr::mutate(
      country = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "country.name"),
      # make congo names more PC
      country = dplyr::case_when(
        country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", 
        country == "Congo - Brazzaville" ~ "Republic of Congo",
        TRUE ~ country),
      continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
      region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region"),
      source = "ECDC"
    ) %>% 
    dplyr::select(date, country_ecdc:geoid, country:region, iso_a3, cases, deaths, population_2018, source)
  
  # df_ecdc <- NCoVUtils::get_ecdc_cases() %>% 
  #   dplyr::mutate(geoid = dplyr::case_when(
  #     country == "United_Kingdom" ~ "GB",
  #     country == "Greece" ~ "GR",
  #     country == "French_Polynesia" ~ "PF",
  #     TRUE ~ geoid
  #   )) %>% 
  #   dplyr::rename(country_ecdc = country) %>% 
  #   dplyr::mutate(
  #     country = countrycode::countrycode(geoid, origin = "iso2c", destination = "country.name"),
  #     # countrycode gives DRC the name of 'Congo - Kinshasa' for some reason? fix this
  #     country = dplyr::case_when(country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", TRUE ~ country),
  #     continent = countrycode::countrycode(geoid, origin = "iso2c", destination = "continent"),
  #     region = countrycode::countrycode(geoid, origin = "iso2c", destination = "region"),
  #     iso_a3 = countrycode::countrycode(geoid, origin = "iso2c", destination = "iso3c"),
  #     source = "ECDC"
  #   ) %>% 
  #   dplyr::select(date, country_ecdc:geoid, country:iso_a3, cases, deaths, source)

}


#' Import WHO dataset
#' 
#' @export
get_who_data <- function() {
  
  who_raw <- NCoVUtils::get_who_cases() %>% 
    dplyr::mutate(date = as.Date(Date)) %>% 
    dplyr::select(-SituationReport, -Date, -dplyr::starts_with("RA")) %>% 
    tidyr::pivot_longer(
      cols = -c(date),
      names_to = c("country_who", "indicator", "drop"),
      names_sep = "-",
      values_to = "value"
    )
  
  # who_regions <- who_raw %>% 
  #   filter(country == "Region") %>% 
  #   replace_na(list(indicator = "cases")) %>% 
  #   pivot_wider(names_from = "indicator", values_from = "value")
  
  who_countries <- who_raw %>% 
    dplyr::filter(country_who != "Region", is.na(drop)) %>% 
    dplyr::select(-drop) %>% 
    tidyr::replace_na(list(indicator = "cases")) %>% 
    dplyr::filter(
      indicator %in% c("cases", "deaths"), 
      !country_who %in% c("InternationalConveyance", "HealthCareWorkers")
    ) %>% 
    tidyr::pivot_wider(names_from = "indicator", values_from = "value") %>% 
    dplyr::group_by(country_who) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~c(.[1], diff(.)) %>% as.integer) %>% # convert from cumulative to daily
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~ifelse(. < 0, 0L, .)) %>% 
    dplyr::ungroup() %>%
    dplyr::mutate(iso_a3 = countrycode::countrycode(country_who, "country.name", "iso3c")) %>% 
    dplyr::mutate(iso_a3 = dplyr::case_when(
      country_who == "CentralAfricanRepublic" ~"CAF",
      country_who == "DominicanRepublic"      ~"DOM",
      country_who == "Eswatini"               ~"SWZ",
      country_who == "IsleofMan"              ~"IMN",
      country_who == "Isreal"                 ~"ISR",
      country_who == "Kosovo"                 ~NA_character_,
      country_who == "RepublicofKorea"        ~"KOR",
      country_who == "SaintLucia"             ~"LCA",
      country_who == "SaintMartin"            ~"MAF",
      country_who == "SintMaartin"            ~"MAF",
      country_who == "SouthAfrica"            ~"ZAF",
      country_who == "UnitedStatesofAmerica"  ~"USA",
      TRUE ~ iso_a3
    )) %>% 
    dplyr::mutate(
      country = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "country.name"),
      # countrycode gives DRC the name of 'Congo - Kinshasa' for some reason? fix this
      country = dplyr::case_when(country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", TRUE ~ country),
      continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
      region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region"),
      source = "WHO"
    )
  
  return(who_countries)
  
  # this no longer works as the data format has changed in the github repo below
  
  # who_url <- "https://raw.githubusercontent.com/eebrown/data2019nCoV/master/data/sarscov2_who_2019.rda"
  # who_url <- "https://raw.githubusercontent.com/eebrown/data2019nCoV/master/data/WHO_SR.rda"
  # temp <- tempdir()
  # filename <- "WHO_SR.rda"
  # curl::curl_download(who_url, destfile = fs::path(temp, filename))
  # load(fs::path(temp, filename))
  # 
  # df_who <- WHO_SR %>% 
  #   tibble::as_tibble() %>%
  #   dplyr::select(date, starts_with("cases"), starts_with("deaths")) %>%
  #   tidyr::pivot_longer(
  #     cols = -date,
  #     names_to = c("ind", "iso_a3", "region"),
  #     names_sep = "_",
  #     values_to = "value"
  #   ) %>%
  #   dplyr::filter(is.na(region)) %>% # remove region level data
  #   dplyr::select(-region) %>% 
  #   dplyr::group_by(ind, iso_a3) %>%
  #   dplyr::arrange(date) %>%
  #   dplyr::mutate(value = c(value[1], diff(value))) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::arrange(iso_a3, region, date, ind) %>%
  #   tidyr::pivot_wider(names_from = "ind", values_from = "value") %>% 
  #   dplyr::mutate(
  #     iso_a3 = stringr::str_to_upper(iso_a3), 
  #     country = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "country.name"),
  #     continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
  #     region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region"),
  #     source = "WHO"
  #   ) %>% 
  #   dplyr::filter(!iso_a3 %in% c("GLOBAL", "INTERNATIONALCONVEYANCE")) %>% 
  #   dplyr::select(date, country:region, dplyr::everything())
}




