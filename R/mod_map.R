# Module UI

#' @title   mod_map_ui and mod_map_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_map
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_map_ui <- function(id, df_interventions){
  ns <- NS(id)
  tagList(
    fluidRow(
      col_3(
        selectInput(
          ns("intervention"), 
          "Intervention", 
          choices = unique(df_interventions$MEASURE),
          width = "100%"
        )
      ),
      #col_3(selectInput(ns("param"), "Indicator", choices = c("Cases", "Cumulative incidence per 100,000", "Deaths", "Population"), width = "100%")),
      col_6(
        sliderInput(
          ns("time_period"),
          label = "Time period",
          min = min(df_interventions$DATE_IMPLEMENTED, na.rm = TRUE),
          max = Sys.Date(),
          value = c(min(df_interventions$DATE_IMPLEMENTED, na.rm = TRUE), Sys.Date()),
          timeFormat = "%d/%m/%y",
          step = 7,
          width = "100%"
        )
      )
    ),
    fluidRow(
      col_12(
        leaflet::leafletOutput(ns("map"))
      )
    )
  )
}

# Module Server

#' @rdname mod_map
#' @export
#' @keywords internal
#' @import leaflet
mod_map_server <- function(input, output, session, df_interventions){
  ns <- session$ns
  
  w <- waiter::Waiter$new(
    id = ns("map"),
    html = waiter::spin_3(), 
    color = waiter::transparent(.5)
  )
  
  map_dat <- reactive({
    
    w$show()
    
    #browser()
    
    df <- df_interventions %>% 
      dplyr::filter(MEASURE == input$intervention) %>% 
      dplyr::left_join(sf_world, by = c("ISO" = "iso_a3")) %>% 
      sf::st_as_sf()
    
    return(df)
  })
  
  output$map <- leaflet::renderLeaflet({
    # init_period <- isolate(input$time_period)
    # title <- tags$div(
    #   tag.map.title, # find this in utils.R
    #   HTML(glue::glue("{ISOweek::ISOweek(init_period[1])} - {ISOweek::ISOweek(init_period[2])}"))
    # )
    
    leaflet::leaflet() %>%
      leaflet::addMapPane(name = "choropleth", zIndex = 410) %>%
      leaflet::addMapPane(name = "borders", zIndex = 420) %>%
      leaflet::addMapPane(name = "place_labels", zIndex = 430) %>% 
      leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM") %>%
      leaflet::addProviderTiles("CartoDB.Positron", group = "Basemap") %>%
      leaflet::addProviderTiles("CartoDB.PositronOnlyLabels", group = "Basemap", 
                                options = leaflet::leafletOptions(pane = "place_labels")) %>%
      #leaflet::flyToBounds(lng1 = -5.513454, lng2 = 2.400604, lat1 = 9.411405, lat2 = 15.082890) %>% 
      # addPolylines(
      #   color = "red", weight = 2, group = "Region borders",
      #   options = leaflet::leafletOptions(pane = "borders")
      # ) %>% 
      #addControl(title, position = "topright", className="map-title", layerId = "title") %>% 
      addScaleBar(position = "bottomright") %>% 
      leaflet::addLayersControl(
        baseGroups = c("Basemap", "OSM"),
        overlayGroups = c("Choropleth"),
        position = "topleft"
      )
    
  })
  
  # change epiweek label when slider changes
  # observe({
  #   title <- tags$div(
  #     tag.map.title, # find this in utils.R
  #     HTML(glue::glue("{ISOweek::ISOweek(input$time_period[1])} - {ISOweek::ISOweek(input$time_period[2])}"))
  #   )
  #   leafletProxy("map", session) %>%  
  #     removeControl("title") %>% 
  #     addControl(title, position = "topright", className="map-title", layerId = "title")
  # })
  
  observe({
    
    dat <- map_dat()
    
    # vec <- dat[[input$param]]
    # vec_type <- typeof(vec)
    # 
    # if (vec_type == "integer") {
    #   n <- min(c(max(vec, na.rm = TRUE)-min(vec, na.rm = TRUE), 5))
    # } else if (vec_type == "double") {
    #   n <- dplyr::n_distinct(vec)
    # }
    # 
    # if (n < 5) {
    #   dat$brks <- round(vec, 2)
    #   leaf_pal <- colorFactor("viridis", sort(unique(dat$brks)))
    # } else {
    #   dat$brks <- add_brks(vec)
    #   leaf_pal <- colorFactor("viridis", levels = sort(unique(dat$brks)))
    # }
    # 
    # #leaf_pal <- colorBin("viridis", domain = dat[[input$param]], na.color = "grey", reverse = FALSE)
    # popup_cols <- c("Region", "District", "Population", "Disease", 
    #                 "Cases", "Cumulative incidence per 100,000", "Deaths", "Data completeness")
    # 
    leafletProxy("map", session) %>% 
      clearGroup("Choropleth") %>% 
      #removeControl("legend") %>% 
      leaflet::addPolygons(
        data = dat,
        stroke = TRUE, 
        color = "white",  
        weight = 1,
        fillColor = "red", 
        fillOpacity = .6,
        label = ~country, 
        #popup = leafpop::popupTable(dat, zcol = popup_cols, row.numbers = FALSE, feature.id = FALSE),
        highlightOptions = leaflet::highlightOptions(bringToFront = TRUE, fillOpacity = .7),
        group = "Choropleth", 
        options = leaflet::pathOptions(pane = "choropleth")
      ) #%>% 
      # addLegend(
      #   pal = leaf_pal, 
      #   values = dat$brks, 
      #   layerId = "legend",
      #   title = input$param, 
      #   position = "bottomright"
      # )
    
    w$hide() 
  })
  
}

## To be copied in the UI
# mod_map_ui("map_1")

## To be copied in the server
# callModule(mod_map_server, "map_1")

