# Inverted versions of in, is.null and is.na
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

# Removes the null from a vector
drop_nulls <- function(x){
  x[!sapply(x, is.null)]
}

# If x is null, return y, otherwise return x
"%||%" <- function(x, y){
  if (is.null(x)) {
    y
  } else {
    x
  }
}
# If x is NA, return y, otherwise return x
"%|NA|%" <- function(x, y){
  if (is.na(x)) {
    y
  } else {
    x
  }
}

# typing reactiveValues is too long
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

break_text_html <- function(text, width = 25, exdent = 4) {
  stringr::str_wrap(text, width = width, exdent = exdent) %>% 
    stringr::str_replace_all("\\\n", "<br>")
}

filter_geo <- function(df, r_filter, r_type, iso_col = iso_a3) {

  if (r_type == "continent") {
    df <- df %>% dplyr::filter(continent == r_filter)
  } else if (r_type == "region") {
    df <- df %>% dplyr::filter(region == r_filter)
  } else if (r_type == "country") {
    df <- df %>% dplyr::filter({{iso_col}} == r_filter)
  }
  return(df)
}

numeric_to_date <- function(data_row, column) {
  data_row[[column]] <- as.Date(data_row[[column]], origin = "1970-01-01")
  return(data_row)
}

factor_to_char <- function(data, column) {
  data[[column]] <- as.character(data[[column]])
  return(data)
}

date_to_timestamp <- function(data, column) {
  if (lubridate::is.Date(data[[column]])) {
    data[[column]] <- datetime_to_timestamp(data[[column]])
  }
  return(data)
}

#' Custom and faster version of highcharter:::data_to_series
#' 
#' @description 
#' It should be only applied for grouped data containing date column. 
#  Can be extended in the future for general use.
data_to_series <- function(data, type) {
  
  cols_to_use <- setdiff(colnames(data), "group")
  data <- date_to_timestamp(data, "x")
  
  split(data, data$group) %>% 
    purrr::imap(
      ~ list(name = .y, 
             data = purrr::transpose(.x[, cols_to_use]) %>% purrr::modify(numeric_to_date, column = "date"), 
             type = type
      )
    ) %>% unname() 
}
