#
# SPF: Santé Publique France

get_spf_cases <- function(level = c("national", "regional")) {
  
  level <- match.arg(level, several.ok = FALSE)
  
  # github api url to list contents of folder (daily data is stored in separate yaml files)
  spf_url <- "https://api.github.com/repos/opencovid19-fr/data/contents/sante-publique-france"
  spf_ls <- httr::GET(spf_url) %>% httr::content() %>% purrr::map_chr("download_url")
  
  #set up cache
  ch <- memoise::cache_filesystem(".cache")
  
  if (level == "national") {
    mem_read_spf <- memoise::memoise(read_spf_yaml, cache = ch)
    
    df_data_spf <- spf_ls %>%
      purrr::map_dfr(mem_read_spf) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate_if(is.numeric, ~c(.[1], diff(.))) #%>%
      # tidyr::pivot_longer(cols = tidyselect::starts_with("n_"), names_to = "time_series", values_to = "value") %>%
      # dplyr::group_by(country, time_series) %>%
      # dplyr::arrange(time_series, date_rep) %>%
      # dplyr::mutate(value_cum = cumsum(value)) %>%
      # dplyr:: ungroup()
  } else if (level == "regional") {
    mem_read_spf_regions <- memoise::memoise(read_spf_regions_yaml, cache = ch)
    
    df_data_spf <- spf_ls %>%
      purrr::map_dfr(mem_read_spf_regions) %>%
      dplyr::arrange(date) %>%
      dplyr::group_by(region) %>% 
      dplyr::mutate(cases = c(cases[1], diff(cases))) %>%
      dplyr::ungroup() #%>% 
      # tidyr::pivot_longer(cols = tidyselect::starts_with("n_"), names_to = "time_series", values_to = "value") %>%
      # dplyr::group_by(region, time_series) %>%
      # dplyr::arrange(time_series, date_rep) %>%
      # dplyr::mutate(value_cum = cumsum(value)) %>%
      # dplyr::ungroup()
  }
  
  return(df_data_spf)
}

read_spf_yaml <- function(path) {
  ydat <- yaml::read_yaml(path)
  tibble::tibble(
    date = as.Date(ydat$date),
    country = "France",
    source = "SPF",
    cases = ydat$donneesNationales$casConfirmes,
    deaths = ydat$donneesNationales$deces
  )
}

read_spf_regions_yaml <- function(path) {
  ydat <- yaml::read_yaml(path)
  regions <- ydat$donneesRegionales
  
  tibble::tibble(
    date = as.Date(ydat$date),
    country = "France",
    source = "SPF",
    region = purrr::map_chr(regions, "nom"),
    region_code = purrr::map_chr(regions, "code"),
    cases = purrr::map_int(regions, "casConfirmes")
  )
}

dat <- get_spf_cases(level = "national")
spf_regions <- get_spf_cases(level = "regional")
  