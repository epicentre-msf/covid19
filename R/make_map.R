
make_map <- function(df_trends, sf_world, latest_date = Sys.Date()-1) {
  sf_df <- sf_world %>%
    dplyr::select(iso_a3, lon, lat) %>%
    dplyr::inner_join(
      select(df_trends, country, iso_a3, cases, deaths, trend_cases = trend_cases_14d, trend_deaths = trend_deaths_14d),
      by = "iso_a3"
    ) %>%
    tidyr::drop_na(cases)
  
  tooltip <- glue::glue_data(
    sf_df,
    "<b>{country}</b><br>
  Total cases: {scales::number(cases, accuracy = 1)}<br>
  Total deaths: {scales::number(deaths, accuracy = 1)}<br>
  <b>14 day trend</b><br>
  Cases: {trend_cases}<br>
  Deaths: {trend_deaths}<br>"
  ) %>% purrr::map(htmltools::HTML)
  
  lvls <- c("Increasing", "Likely increasing", "Stable", "Likely decreasing", "Decreasing")
  values <- scico::scico(5, palette = "vikO", begin = .2, end = .8, direction = -1) %>% set_names(lvls)
  pal <- leaflet::colorFactor(palette = values, levels = names(values), ordered = TRUE, na.color = NA)
  pal_bw <- leaflet::colorFactor(palette = c("#FFFFFF", rep("#808080", 3), "#FFFFFF"), levels = lvls, ordered = TRUE, na.color = NA)
  
  leaflet(sf_df) %>%
    addMapPane(name = "choropleth", zIndex = 410) %>%
    addMapPane(name = "polygons", zIndex = 420) %>%
    addMapPane(name = "borders", zIndex = 430) %>%
    addMapPane(name = "circles", zIndex = 440) %>%
    addMapPane(name = "place_labels", zIndex = 450) %>%
    addTiles(
      urlTemplate = "https://{s}.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{r}.png",
      attribution = paste0(
        '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> ', 
        'contributors &copy; <a href="https://carto.com/attributions">CARTO</a> ',  
        '| WHO COVID-19 data as of ', latest_date, 
        ' | Trend analysis and map: <a href="https://reports.msf.net/public/covid19/">Epicentre MSF</a>'
      )
    ) %>%
    addTiles(
      urlTemplate = "https://{s}.basemaps.cartocdn.com/light_only_labels/{z}/{x}/{y}{r}.png",
      attribution = "",
      group = "Labels",
      options = leaflet::leafletOptions(pane = "place_labels")
    ) %>%
    setView(0, 40, zoom = 2) %>%
    leaflet.extras::addFullscreenControl(position = "topleft") %>%
    leaflet.extras::addResetMapButton() %>%
    addLayersControl(
      overlayGroups = c("Labels", "Trends", "Cases"),
      position = "topleft",
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    addPolygons(
      stroke = TRUE,
      color = "white",
      weight = 1,
      fillColor = ~pal(trend_cases),
      fillOpacity = 0.6,
      label = tooltip,
      highlightOptions = highlightOptions(bringToFront = TRUE, fillOpacity = .7, weight = 2),
      group = "Trends",
      options = pathOptions(pane = "choropleth")
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = factor(names(values), levels = names(values)),
      title = paste("14 day Cases Trend"),
      layerId = "choro_legend",
      group = "Trends"
    ) %>% 
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = ~calc_radius(cases),
      fill = FALSE,
      weight = 1,
      color = ~pal_bw(trend_cases),
      opacity = 1,
      label = tooltip,
      group = "Cases",
      options = pathOptions(pane = "circles")
    ) %>%
    addCircleLegend(
      title = paste("Confirmed", "Cases"),
      range = sf_df$cases,
      scaling_fun = calc_radius,
      label_accuracy = 1,
      fillColor = "#FFFFFF",
      fillOpacity = 0.6,
      weight = 1,
      color = "#808080",
      position = "topright",
      layerId = "circle_legend",
      group = "Cases"
    ) 
}
