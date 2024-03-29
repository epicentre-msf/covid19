#' Import WHO PHSM dataset
#'
#' Public health and social measures
#'
#' @export
get_phsm_data <- function() {
  base_url <- "https://www.who.int/emergencies/diseases/novel-coronavirus-2019/phsm"

  get_hrefs <- xml2::read_html(base_url) %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")

  dl_url <- get_hrefs[stringr::str_detect(get_hrefs, "xlsx")] %>%
    xml2::url_absolute(base_url) %>%
    max()

  temp <- tempdir()
  filename <- "phsm.xlsx"
  curl::curl_download(dl_url, destfile = fs::path(temp, filename))

  

  readxl::read_excel(fs::path(temp, filename), sheet = "master") %>%
    mutate(across(contains("date"), lubridate::as_date)) %>%
    left_join(
      select(countrycode::codelist, iso = iso3c, flag = unicode.symbol),
      by = "iso"
    )
}


#' Import interventions dataset
#'
#' @export
get_interventions_data <- function() {
  base_url <- "https://data.humdata.org"

  # dl_url <- xml2::read_html(paste0(base_url, "/dataset/acaps-covid19-government-measures-dataset#")) %>%
  #   rvest::html_node(css = ".ga-download") %>%
  #   rvest::html_attr("href") %>%
  #   xml2::url_absolute(base_url)

  get_hrefs <- xml2::read_html(paste0(base_url, "/dataset/acaps-covid19-government-measures-dataset#")) %>%
    rvest::html_nodes(css = ".ga-download") %>%
    rvest::html_attr("href")

  dl_url <- get_hrefs[stringr::str_detect(get_hrefs, "xlsx")] %>% xml2::url_absolute(base_url)

  temp <- tempdir()
  filename <- "interventions.xlsx"
  curl::curl_download(dl_url, destfile = fs::path(temp, filename))

  readxl::read_excel(fs::path(temp, filename), sheet = "Dataset") %>%
    janitor::clean_names() %>%
    dplyr::mutate_if(lubridate::is.POSIXct, lubridate::as_date) %>%
    # date_implemented is being read as excel numeric now
    # dplyr::mutate(date_implemented = janitor::excel_numeric_to_date(date_implemented)) %>%
    dplyr::mutate(
      measure = stringr::str_to_sentence(measure),
      country = countrycode::countrycode(iso, origin = "iso3c", destination = "country.name"),
      country = dplyr::case_when(country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", TRUE ~ country),
      continent = countrycode::countrycode(iso, origin = "iso3c", destination = "continent"),
      region = countrycode::countrycode(iso, origin = "iso3c", destination = "region23")
    )
}

#' Import weekly cases and deaths data from ECDC
#' @export
get_ecdc_weekly <- function() {
  url_csv <- "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv/data.csv"
  safe_csv <- purrr::safely(readr::read_csv)
  df_raw <- safe_csv(url_csv)

  # if csv url didn't work try the xlsx version
  if (is.null(df_raw$error)) {
    df_raw <- df_raw$result
  } else {
    message("CSV download failed, trying XLSX")
    xlsx_url <- "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/xlsx/data.xlsx"
    httr::GET(xlsx_url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
    df_raw <- readxl::read_excel(tf)
  }

  df_raw %>%
    dplyr::filter(!is.na(country_code)) %>%
    tidyr::pivot_wider(
      names_from = "indicator",
      values_from = c("weekly_count", "cumulative_count", "rate_14_day"),
      names_glue = "{indicator}_{.value}",
    ) %>%
    tidyr::separate(year_week, into = c("year", "week"), sep = "-", remove = FALSE) %>%
    dplyr::mutate(
      date = aweek::week2date(glue::glue("{year}-W{week}"), week_start = "Monday"),
      region = countrycode::countrycode(country_code, origin = "iso3c", destination = "region23"),
      region = dplyr::case_when(country == "Kosovo" ~ "Southern Europe", TRUE ~ region)
    ) %>%
    dplyr::select(
      date,
      year_week,
      continent,
      region,
      country,
      iso_a3 = country_code,
      cases = cases_weekly_count,
      deaths = deaths_weekly_count,
      cases_rate_14_day,
      deaths_rate_14_day
    ) %>%
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~ ifelse(. < 0, 0L, .))
}

#' Import JHU data from Our World in Data
#' @export
get_owid_jhcsse <- function(url = "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/jhu/full_data.csv") {
  df_raw <- readr::read_csv(url, guess_max = 60000)
  df_raw %>%
    dplyr::filter(!location %in% c("International", "World"), !is.na(date)) %>%
    dplyr::mutate(
      iso_a3 = countrycode::countrycode(location, "country.name", "iso3c"),
      iso_a3 = dplyr::case_when(location == "Timor" ~ "TLS", location == "Micronesia (country)" ~ "FSM", TRUE ~ iso_a3),
      continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
      region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region23"),
      continent = dplyr::case_when(location == "Kosovo" ~ "Europe", TRUE ~ continent),
      region = dplyr::case_when(location == "Kosovo" ~ "Southern Europe", TRUE ~ region),
      iso_a3 = dplyr::case_when(location == "Kosovo" ~ "XKX", TRUE ~ iso_a3)
    ) %>%
    dplyr::filter(!is.na(iso_a3)) %>% # filters out continent aggregates
    dplyr::select(date, continent, region, country = location, iso_a3, cases = new_cases, deaths = new_deaths) %>%
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~ ifelse(. < 0, 0L, .))
}

#' Model trends from ECDC Data
#' @export
get_trends_data <- function(df) {
  library(growthrates)
  library(forecast)
  library(sandwich)

  # don't model on the 2 most recent days as data might not be complete due to reporting delays
  use_date <- max(df$date) - 2
  trend_data <- df %>% dplyr::filter(date <= use_date)

  model_cnt_cases_linear <- make_model_cnt_linear(df = trend_data, series = "cases", time_unit_extent = 12, min_sum = 30)
  model_cnt_deaths_linear <- make_model_cnt_linear(df = trend_data, series = "deaths", time_unit_extent = 12, min_sum = 30)

  o_vars <- c("coeff", "lwr", "upr")
  l_cnt_vars <- setNames(as.vector(o_vars), paste0("l_cnt_", o_vars))

  df_cases <- dplyr::rename(model_cnt_cases_linear[[3]], tidyselect::all_of(l_cnt_vars)) %>%
    dplyr::mutate(
      trend_linear = case_when(
        l_cnt_lwr > 0 ~ "increasing",
        l_cnt_upr < 0 ~ "declining",
        l_cnt_upr > 0 & l_cnt_lwr < 0 ~ "stable",
        TRUE ~ NA_character_
      ) %>%
        factor(
          levels = c("increasing", "stable", "declining"),
          labels = c("Increasing", "Stable", "Declining")
        )
    ) %>%
    dplyr::select(iso_a3, trend_cases = trend_linear)

  df_deaths <- dplyr::rename(model_cnt_deaths_linear[[3]], tidyselect::all_of(l_cnt_vars)) %>%
    dplyr::mutate(
      trend_linear = case_when(
        l_cnt_lwr > 0 ~ "increasing",
        l_cnt_upr < 0 ~ "declining",
        l_cnt_upr > 0 & l_cnt_lwr < 0 ~ "stable",
        TRUE ~ NA_character_
      ) %>%
        factor(
          levels = c("increasing", "stable", "declining"),
          labels = c("Increasing", "Stable", "Declining")
        )
    ) %>%
    dplyr::select(iso_a3, trend_deaths = trend_linear)

  df %>%
    dplyr::group_by(continent, region, country, iso_a3) %>%
    dplyr::summarise(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(
      dplyr::full_join(df_cases, df_deaths)
    )

  # dplyr::distinct(df_ecdc, continent, region, country, iso_a3) %>% dplyr::inner_join(dplyr::full_join(df_cases, df_deaths))
}


#' Import ECDC dataset
#'
#' @export
get_ecdc_data <- function() {
  base_url <- "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv"
  xlsx_url <- "https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide.xlsx"

  error <- suppressMessages(
    suppressWarnings(try(readr::read_csv(file = base_url), silent = TRUE))
  )

  if ("try-error" %in% class(error)) {
    message("csv unavailable, trying xlsx")
    httr::GET(xlsx_url, httr::write_disk(tf <- tempfile(fileext = ".xlsx")))
    df <- readxl::read_excel(tf)
  } else {
    df <- readr::read_csv(base_url)
  }


  df %>%
    # Adjust for one-day lag, because ECDC reports at 10am CET the next day
    dplyr::mutate(date = lubridate::make_date(year, month, day) - 1) %>%
    dplyr::select(date, geoid = geoId, country_ecdc = countriesAndTerritories, iso_a3 = countryterritoryCode, population_2019 = popData2019, cases, deaths) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~ ifelse(. < 0, 0L, .)) %>%
    dplyr::mutate(
      geoid = dplyr::case_when(
        country_ecdc == "Namibia" ~ "NA",
        TRUE ~ geoid
      ),
      country = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "country.name"),
      # make congo names more PC
      country = dplyr::case_when(
        country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
        country == "Congo - Brazzaville" ~ "Republic of Congo",
        TRUE ~ country
      ),
      continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
      region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region23"),
      source = "ECDC"
    ) %>%
    dplyr::select(date, country_ecdc:geoid, country:region, iso_a3, cases, deaths, population_2019, source)
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
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~ c(.[1], diff(.)) %>% as.integer()) %>% # convert from cumulative to daily
    dplyr::mutate_at(dplyr::vars(cases, deaths), ~ ifelse(. < 0, 0L, .)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(iso_a3 = countrycode::countrycode(country_who, "country.name", "iso3c")) %>%
    dplyr::mutate(iso_a3 = dplyr::case_when(
      country_who == "CentralAfricanRepublic" ~ "CAF",
      country_who == "DominicanRepublic" ~ "DOM",
      country_who == "Eswatini" ~ "SWZ",
      country_who == "IsleofMan" ~ "IMN",
      country_who == "Isreal" ~ "ISR",
      country_who == "Kosovo" ~ NA_character_,
      country_who == "RepublicofKorea" ~ "KOR",
      country_who == "SaintLucia" ~ "LCA",
      country_who == "SaintMartin" ~ "MAF",
      country_who == "SintMaartin" ~ "MAF",
      country_who == "SouthAfrica" ~ "ZAF",
      country_who == "UnitedStatesofAmerica" ~ "USA",
      TRUE ~ iso_a3
    )) %>%
    dplyr::mutate(
      country = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "country.name"),
      # countrycode gives DRC the name of 'Congo - Kinshasa' for some reason? fix this
      country = dplyr::case_when(country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo", TRUE ~ country),
      continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
      region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region23"),
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

get_jhcsse_data <- function() {
  url_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  url_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

  df_cases <- clean_jhcsse_data(url_cases, type = "cases")
  df_deaths <- clean_jhcsse_data(url_deaths, type = "deaths")

  df_jhcsse <-
    dplyr::full_join(df_cases, df_deaths, by = c("iso_a3", "country_jh", "date")) %>%
    dplyr::mutate(
      country = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "country.name"),
      # make congo names more PC
      country = dplyr::case_when(
        country == "Congo - Kinshasa" ~ "Democratic Republic of the Congo",
        country == "Congo - Brazzaville" ~ "Republic of Congo",
        TRUE ~ country
      ),
      continent = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "continent"),
      region = countrycode::countrycode(iso_a3, origin = "iso3c", destination = "region23"),
      source = "JHCSSE"
    ) %>%
    dplyr::select(date, country_jh, iso_a3, country:region, cases, deaths, source)
}

clean_jhcsse_data <- function(path, type = c("cases", "deaths")) {
  df_raw <- readr::read_csv(path)

  df_china <- df_raw %>%
    dplyr::filter(`Country/Region` == "China", !is.na(`Province/State`)) %>%
    dplyr::select(-Lat, -Long) %>%
    dplyr::group_by(`Country/Region`) %>%
    dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    dplyr::ungroup()

  df_aus <- df_raw %>%
    dplyr::filter(`Country/Region` == "Australia", !is.na(`Province/State`)) %>%
    dplyr::select(-Lat, -Long) %>%
    dplyr::group_by(`Country/Region`) %>%
    dplyr::summarise_if(is.numeric, sum, na.rm = TRUE) %>%
    dplyr::ungroup()

  df_raw %>%
    dplyr::filter(is.na(`Province/State`), `Country/Region` != "China") %>% # filter to only countries
    dplyr::select(-`Province/State`, -Lat, -Long) %>%
    dplyr::bind_rows(df_china) %>%
    dplyr::bind_rows(df_aus) %>%
    dplyr::rename(country_jh = `Country/Region`) %>%
    tidyr::pivot_longer(-country_jh, names_to = "date", values_to = type) %>%
    dplyr::mutate(date = as.Date(date, format = "%m/%d/%y")) %>%
    dplyr::group_by(country_jh) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate_if(is.numeric, ~ c(.[1], diff(.)) %>% as.integer()) %>% # convert from cumulative to daily
    dplyr::mutate_if(is.numeric, ~ ifelse(. < 0, 0L, .)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(iso_a3 = countrycode::countrycode(country_jh, "country.name", "iso3c"))
}