#' French Regional Case Counts
#'
#' @return A dataframe of French regional case counts.
#' @export
#' @importFrom yaml read_yaml
#' @importFrom httr GET content
#' @importFrom purrr map_dfr map_chr
#' @importFrom dplyr mutate select arrange group_by mutate mutate_if ungroup
#' @importFrom memoise cache_filesystem memoise
#' @examples
#'
#' ## Code
#' get_italy_regional_cases

# SPF: Santé Publique France

# get_spf_cases <- function(level = c("national", "regional")) {
# 
#   level <- match.arg(level, several.ok = FALSE)
# 
#   # github api url to list contents of folder (daily data is stored in separate yaml files)
#   spf_url <- "https://api.github.com/repos/opencovid19-fr/data/contents/sante-publique-france"
#   spf_ls <- httr::GET(spf_url) %>% httr::content() %>% purrr::map_chr("download_url")
# 
#   #set up cache
#   ch <- memoise::cache_filesystem(".cache")
# 
#   if (level == "national") {
#     mem_read_spf <- memoise::memoise(read_spf_yaml, cache = ch)
# 
#     df_data_spf <- spf_ls %>%
#       purrr::map_dfr(mem_read_spf) %>%
#       dplyr::arrange(date) %>%
#       dplyr::mutate_if(is.numeric, ~c(.[1], diff(.))) #%>%
#       # tidyr::pivot_longer(cols = tidyselect::starts_with("n_"), names_to = "time_series", values_to = "value") %>%
#       # dplyr::group_by(country, time_series) %>%
#       # dplyr::arrange(time_series, date_rep) %>%
#       # dplyr::mutate(value_cum = cumsum(value)) %>%
#       # dplyr:: ungroup()
#   } else if (level == "regional") {
#     mem_read_spf_regions <- memoise::memoise(read_spf_regions_yaml, cache = ch)
# 
#     df_data_spf <- spf_ls %>%
#       purrr::map_dfr(mem_read_spf_regions) %>%
#       dplyr::arrange(date) %>%
#       dplyr::group_by(region) %>%
#       dplyr::mutate(cases = c(cases[1], diff(cases))) %>%
#       dplyr::ungroup() #%>%
#       # tidyr::pivot_longer(cols = tidyselect::starts_with("n_"), names_to = "time_series", values_to = "value") %>%
#       # dplyr::group_by(region, time_series) %>%
#       # dplyr::arrange(time_series, date_rep) %>%
#       # dplyr::mutate(value_cum = cumsum(value)) %>%
#       # dplyr::ungroup()
#   }
# 
#   return(df_data_spf)
# }
# 
# read_spf_yaml <- function(path) {
#   ydat <- yaml::read_yaml(path)
#   tibble::tibble(
#     date = as.Date(ydat$date),
#     country = "France",
#     source = "SPF",
#     cases = ydat$donneesNationales$casConfirmes,
#     deaths = ydat$donneesNationales$deces
#   )
# }
# 
# read_spf_regions_yaml <- function(path) {
#   ydat <- yaml::read_yaml(path)
#   regions <- ydat$donneesRegionales
# 
#   tibble::tibble(
#     date = as.Date(ydat$date),
#     country = "France",
#     source = "SPF",
#     region = purrr::map_chr(regions, "nom"),
#     region_code = purrr::map_chr(regions, "code"),
#     cases = purrr::map_int(regions, "casConfirmes")
#   )
# }
# 
# dat <- get_spf_cases(level = "national")
# spf_regions <- get_spf_cases(level = "regional")



# France Regions

# fr_path <- here("data-raw/shps/fr_shp")
# 
# if (!dir_exists(fr_path)) {
#   fr_url <- "https://www.data.gouv.fr/en/datasets/r/aacf9338-8944-4513-a7b9-4cd7c2db2fa9"
#   fr_zip <- here("data-raw/shps/fr_shp.zip")
#   dir_create(fr_path, recurse = TRUE)
#   download.file(fr_url, destfile = fr_zip)
#   utils::unzip(fr_zip, exdir = fr_path)
# }
# 
# sf_france <- sf::st_read(dsn = fr_path, layer = "regions-20180101")
# 
# ggplot(sf_france) +
#   geom_sf()
# 
# plot(sf_france$geometry)
#'   