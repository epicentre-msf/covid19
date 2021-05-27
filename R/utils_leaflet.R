# make a scaling function to convert real numbers to radii appropriate for leaflet
calc_radius <- function(n, scale_factor = 30) {
  sqrt(n)/sqrt(max(n)) * scale_factor
}

addCircleLegend <- function(
  map, title = "", range, scaling_fun, ...,
  color, weight, fillColor, fillOpacity, label_accuracy = 0.1,
  position = c("topright", "bottomright", "bottomleft", "topleft"),
  data = leaflet::getMapData(map), layerId = NULL, group = NULL) {
  
  range <- base::pretty(sort(range), 20)
  range <- range[range != 0]
  min_n <- ceiling(min(range, na.rm = TRUE))
  med_n <- round(median(range, na.rm = TRUE), 0)
  max_n <- round(max(range, na.rm = TRUE), 0)
  n_range <- c(min_n, med_n, max_n)
  radii <- scaling_fun(n_range, ...)
  n_range <- scales::label_number_si(accuracy = label_accuracy)(n_range)
  
  circle_style <- glue::glue(
    "border-radius:50%;
    border: {weight}px solid {color};
    background: {paste0(fillColor, round(fillOpacity*100, 0))};
    position: absolute;
    bottom:1px;
    right:25%;
    left:50%;"
  )
  
  text_style <- glue::glue(
    "text-align: right;
    font-size: 11px;
    position: absolute;
    bottom: 0px;
    right:1px;"
  )
  
  circle_legend <- htmltools::HTML(glue::glue(
    '<div class="bubble-legend">
    <div id="legendTitle" style="text-align: center; font-weight: bold;">{title}</div>
    <div class="symbolsContainer" style="min-width: {radii[3]*2 + 20}px; min-height: {radii[3]*2}px;">
    <div class="legendCircle" style="width: {radii[3] * 2}px; height: {radii[3] * 2}px; margin-left: {-radii[3]}px; {circle_style}"></div>
    <div class="legendCircle" style="width: {radii[2] * 2}px; height: {radii[2] * 2}px; margin-left: {-radii[2]}px; {circle_style}"></div>
    <div class="legendCircle" style="width: {radii[1] * 2}px; height: {radii[1] * 2}px; margin-left: {-radii[1]}px; {circle_style}"></div>
    <div>
    <p class="legendValue" style="margin-bottom: {radii[1] * 2 - 12}px; {text_style}">{n_range[1]}</p>
    </div>
    <div>
    <p class="legendValue" style="margin-bottom: {radii[2] * 2 - 12}px; {text_style}">{n_range[2]}</p>
    </div>
    <div>
    <p class="legendValue" style="margin-bottom: {radii[3] * 2 - 12}px; {text_style}">{n_range[3]}</p>
    </div>
    </div>
    </div>'
  ))
  
  return(
    #addControl(map, html = circle_legend, position = position, layerId = layerId)
    leaflet::addCustomLegend(map, html = circle_legend, position = position, layerId = layerId, group = group)
  )
}
