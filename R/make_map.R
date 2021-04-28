
make_map <- function(df_trends, sf_world, latest_date = Sys.Date()-1) {
  sf_df <- sf_world %>% select(iso_a3, lon, lat) %>%  dplyr::inner_join(df_trends, by = "iso_a3")
  
  tooltip <- glue::glue_data(
    sf_df,
    "<b>{country}</b><br>
  Cases: {scales::number(cases, accuracy = 1)}<br>
  Cases trend: {trend_cases}<br>
  Deaths: {scales::number(deaths, accuracy = 1)}<br>
  Deaths trend: {trend_deaths}<br>"
  ) %>% purrr::map(htmltools::HTML)
  
  # values <- c(
  #   "Increasing" = "#e75f00",
  #   "Likely increasing" = "#fd9e49",
  #   "Stable" = "#7b848f",
  #   "Likely decreasing" = "#5fa2ce",
  #   "Decreasing" = "#1170aa"
  # )
  # 
  # values <- c(
  #   "Increasing" = "#D7191C",
  #   "Likely increasing" = "#FDAE61",
  #   "Stable" = "grey60",
  #   "Likely decreasing" = "#A6D96A",
  #   "Decreasing" = "#1A9641"
  # )
  
  lvls <- c("Increasing", "Likely increasing", "Stable", "Likely decreasing", "Decreasing")
  greens <- RColorBrewer::brewer.pal(11, "RdYlGn")[c(7, 9)]
  values <- scico::scico(5, palette = "vikO", begin = .2, end = .8, direction = -1) %>% set_names(lvls)
  values[4:5] <- greens
  pal <- leaflet::colorFactor(palette = values, levels = names(values), ordered = TRUE, na.color = NA)
  
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
        '| JHU CSSE COVID-19 data as of ', latest_date, 
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
      # baseGroups = c("Labels", "No Labels"),
      overlayGroups = c("Labels", "Trends", "Cases"),
      position = "topleft",
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    addScaleBar(position = "bottomleft", options = scaleBarOptions(imperial = FALSE)) %>%
    addPolygons(
      stroke = TRUE,
      color = "white",
      weight = 1,
      fillColor = ~ pal(trend_cases),
      fillOpacity = .6,
      label = tooltip,
      highlightOptions = highlightOptions(bringToFront = TRUE, fillOpacity = .7, weight = 2),
      group = "Trends",
      options = pathOptions(pane = "choropleth")
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = factor(names(values), levels = names(values)),
      title = paste("Cases Trend"),
      layerId = "choro_legend",
      group = "Trends"
    ) %>% 
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = ~calc_radius(cases),
      fillColor = "#57AACB",
      fillOpacity = 0.6,
      weight = 1,
      color = "#FFFFFF",
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
      fillColor = "#57AACB",
      fillOpacity = 0.6,
      weight = 1,
      color = "#FFFFFF",
      position = "topright",
      layerId = "circle_legend",
      group = "Cases"
    )
}
