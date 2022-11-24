
# dplyr::distinct(countrycode::codelist_panel, continent, region)
# region_selects <- dplyr::distinct(sf_world %>% sf::st_set_geometry(NULL), continent, region)
# region_selects <- c("World" = list("All"), split(region_selects$region, region_selects$continent))

continents <- c("Africa", "Asia", "Americas", "Europe", "Oceania")

world_regions <- c(
  "Caribbean", "Southern Asia", "Southern Europe", "Northern Africa", 
  "Middle Africa", "South America", "Western Asia", "Australia and New Zealand", 
  "Western Europe", "Western Africa", "Eastern Europe", "Central America", 
  "Southern Africa", "South-Eastern Asia", "Eastern Africa", "Northern America", 
  "Eastern Asia", "Northern Europe", "Melanesia", "Micronesia", "Central Asia", "Polynesia"
)

all_continents <- tibble::tibble(continent = continents, region = continents)

country_iso <- tidyr::drop_na(dplyr::distinct(countrycode::codelist_panel, country.name.en, iso3c), iso3c)
country_iso <- purrr::set_names(country_iso$iso3c, country_iso$country.name.en)

pal20 <- c("#4E79A7FF", "#A0CBE8FF", "#F28E2BFF", "#FFBE7DFF", "#59A14FFF", 
         "#8CD17DFF", "#B6992DFF", "#F1CE63FF", "#499894FF", "#86BCB6FF", 
         "#E15759FF", "#FF9D9AFF", "#79706EFF", "#BAB0ACFF", "#D37295FF", 
         "#FABFD2FF", "#B07AA1FF", "#D4A6C8FF", "#9D7660FF", "#D7B5A6FF")


hc_opts <- getOption("highcharter.chart")
hc_opts$colors <- pal20

fntfmly <- '"Roboto Condensed",-apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif,"Apple Color Emoji","Segoe UI Emoji","Segoe UI Symbol";'

options(
  highcharter.chart = hc_opts,
  highcharter.theme =
    highcharter::hc_theme_smpl(
      chart = list(
        style = list(
          fontFamily = fntfmly
        )
      ),
      title = list(text = NULL, style = list(fontFamily = fntfmly)),
      subtitle = list(style = list(fontFamily = fntfmly)),
      exporting = list(enabled = FALSE),
      credits = list(enabled = FALSE, style = list(fontFamily = fntfmly, fontSize = "12px", cursor = "default"), href=""),
      plotOptions = list(line = list(marker = list(enabled = FALSE)))
    )
)