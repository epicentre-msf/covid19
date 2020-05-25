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
mod_map_ui <- function(id){
  ns <- NS(id)
  
  region_selects <- dplyr::bind_rows(
    all_continents,
    dplyr::distinct(sf_world %>% sf::st_set_geometry(NULL), continent, region)
  )
  region_selects <- c("Worldwide", split(region_selects$region, region_selects$continent))
  
  tagList(
    fluidRow(
      col_3(
        selectInput(
          ns("region"), 
          "Region focus", 
          choices = region_selects, 
          width = "100%"
        )
      ),
      col_3(
        shinyWidgets::radioGroupButtons(
          inputId = ns("source"), 
          label = "Data source", 
          choices = c("ECDC", "JHU CSSE"), #"WHO",
          justified = TRUE,
          size = "sm"
        )
      ),
      col_3(
        shinyWidgets::radioGroupButtons(
          inputId = ns("indicator"), 
          label = "Indicator", 
          choices = c("Cases" = "cases", "Deaths" = "deaths"),
          justified = TRUE,
          size = "sm"
        )
      ),
      col_3(
        dateRangeInput(
          ns("time_period"),
          label = "Time period",
          min = min(df_ecdc$date, na.rm = TRUE),
          max = Sys.Date(),
          start = min(df_ecdc$date, na.rm = TRUE),
          end = Sys.Date(),
          width = "100%"
        )
      )
      # col_4(
      #   selectInput(
      #     ns("intervention"),
      #     "Interventions",
      #     choices = c("Highlight government interventions" = "", sort(unique(df_interventions$measure))),
      #     width = "100%"
      #   )
      # )
    ),
    
    fluidRow(
      col_2(
        uiOutput(ns("totals"))
      ),
      
      shinydashboard::box(
        width = 6, solidHeader = TRUE,
        leaflet::leafletOutput(ns("map"))
      ),
      
      shinydashboard::box(
        #title = tags$b("Government interventions"), 
        width = 4, solidHeader = TRUE,
        reactable::reactableOutput(ns("table"))
      )
    ),
    
    fluidRow(
      col_6(
        shinydashboard::box(
          width = NULL, solidHeader = TRUE,
          title = tagList(
            tags$div(textOutput(ns("epicurve_title")), style = "display: inline-block; font-weight: bold;")#,
            #tags$div(tags$small("click + drag horizontally to zoom"), style = "display: inline-block;")
          ),
          highcharter::highchartOutput(ns("epicurve"))
        )
      ),
      
      col_6(
        shinydashboard::box(
          width = NULL, solidHeader = TRUE, 
          title = tagList(
            tags$div(textOutput(ns("cumulative_title")), style = "display: inline-block; font-weight: bold;"), 
            tags$div(style = "display: inline-block; padding-right: 10px;"),
            tags$div(
              style = "display: inline-block;",
              shinyWidgets::dropdownButton(
                size = "xs", label = "params",  icon = icon("sliders"), #status = "primary",
                inline = TRUE, width = "50px", circle = FALSE,
                # checkboxGroupInput(ns("c_params"), label = "", inline = FALSE,
                #                    choices = c("log scale" = "log", "set days since" = "days")),
                checkboxInput(ns("log"), label = "log scale", value = TRUE),
                checkboxInput(ns("set_days"), label = "xaxis to days since...", value = FALSE),
                tags$br(),
                numericInput(ns("n_days"), label = paste("n", "cases"), value = 10, min = 1, step = 10)
                #uiOutput(ns("n_days_ui"))
              )
            )
          ),
          highcharter::highchartOutput(ns("cumulative"))
        )
      )
    )
    
  )
}

# Module Server

#' @rdname mod_map
#' @export
#' @keywords internal
#' @import leaflet
#' @import highcharter
mod_map_server <- function(input, output, session){
  ns <- session$ns
  
  w_totals <- waiter::Waiter$new(
    id = c(ns("totals")),
    html = waiter::spin_3(), 
    color = waiter::transparent(alpha = 0)
  )
  
  w_map <- waiter::Waiter$new(
    id = c(ns("map")),
    html = waiter::spin_3(), 
    color = waiter::transparent(alpha = 0)
  )
  
  w_tbl <- waiter::Waiter$new(
    id = c(ns("table")),
    html = waiter::spin_3(), 
    color = waiter::transparent(alpha = 0)
  )
  
  w_charts <- waiter::Waiter$new(
    id = c(ns("epicurve"), ns("cumulative")),
    html = waiter::spin_3(), 
    color = waiter::transparent(alpha = 0)
  )
  
  # Region select observers ====================================================
  
  # selected region is input$region unless user clicks on country on map
  
  # reactive val boolean to indicate if a shape has been selected
  map_click <- reactiveVal(FALSE)
  region_select <- reactiveVal()
  
  # if region is selected from map, update map_click value and drop-down selected value
  observeEvent(input$region, {
    region_select(input$region)
  })
  
  # if region is selected from map, update region_select value
  observeEvent(input$map_shape_click$id, {
    map_click(TRUE)
    region_select(input$map_shape_click$id)
  })
  
  observeEvent(input$map_marker_click$id, {
    map_click(TRUE)
    iso <- stringr::str_remove(input$map_marker_click$id, "_mrkr")
    region_select(iso)
  })
  
  observeEvent(input$map_click, {
    if(map_click()) {
      map_click(FALSE)
    } else {
      updateSelectInput(session, "region", selected = "Worldwide")
      region_select("Worldwide")
    }
  })
  
  region_type <- reactive({
    r <- region_select()
    dplyr::case_when(
      r == "Worldwide" ~ "global",
      r %in% continents ~ "continent",
      r %in% world_regions ~ "region",
      TRUE ~ "country"
    )
  })
  
  # Data ========================================================
  
  # switch data source based on user input
  df_data <- reactive({
    switch(
      input$source, 
      ECDC = df_ecdc,
      WHO = df_who,
      `JHU CSSE` = df_jhcsse
    )
  })
  
  map_indicators <- reactive({
    
    w_map$show()
    
    req(length(input$time_period) == 2)
    
    df <- df_data()
    
    if (input$region != "Worldwide") {
      if (input$region %in% continents) {
        df <- df %>% dplyr::filter(continent == input$region)
      } else {
        df <- df %>% dplyr::filter(region == input$region)
      }
    }
    
    df <- df %>% 
      dplyr::filter(
        date >= input$time_period[1],
        date <= input$time_period[2]
      ) %>% 
      tidyr::drop_na(iso_a3) %>% 
      dplyr::group_by(country, iso_a3) %>% 
      dplyr::summarise(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE)) %>% 
      dplyr::arrange(-cases) %>% 
      dplyr::inner_join(dplyr::select(sf_world, iso_a3, lon, lat), by = c("iso_a3")) %>% 
      sf::st_as_sf()
    
    return(df)
  })
  
  df_epicurve <- reactive({
    
    w_charts$show()
    
    df <- df_data()
    ind <- rlang::sym(input$indicator)
    
    df <- df %>% 
      filter_geo(r_filter = region_select(), r_type = region_type(), iso_col = iso_a3) %>% 
      dplyr::filter(date >= input$time_period[1], date <= input$time_period[2]) %>% 
      tidyr::drop_na(country, {{ind}}) %>% 
      dplyr::mutate(country = forcats::fct_lump(country, n = 9, other_level = "Other", w = {{ind}})) %>% 
      dplyr::group_by(date, country) %>% 
      dplyr::summarise(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE)) %>% 
      dplyr::ungroup()
    
    return(df)
  })
  
  # df_gi <- reactive({
  #   
  #   df <- df_interventions
  #   if (input$intervention != "") df <- df %>% dplyr::filter(measure == input$intervention)
  #   return(df)
  #   
  # })
  
  # Outputs ========================================================
  
  output$totals <- renderUI({
    
    w_totals$show()
    
    df <- df_data() %>% 
      filter_geo(r_filter = region_select(), r_type = region_type(), iso_col = iso_a3) %>% 
      dplyr::summarise(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE))
    
    w_totals$hide()
    
    div(
      class = "text-center",
      h2(region_select(), style = "font-weight: bold;"),
      shinydashboard::valueBox(countup::countup(df$cases), "Confirmed Cases", width = 12, color = "blue"),
      shinydashboard::valueBox(countup::countup(df$deaths), "Confirmed Deaths", width = 12, color = "red")
    )
    
  })
  
  output$table <- reactable::renderReactable({
    
    w_tbl$show()
    
    df <- df_interventions
    
    df <- df %>% 
      filter_geo(region_select(), region_type(), iso_col = iso) %>% 
      dplyr::select(date_implemented, country, measure, comments)
    
    rtbl <- reactable::reactable(
      data = df,
      height = 400, searchable = FALSE, defaultSorted = "date_implemented", 
      compact = TRUE, highlight = TRUE, pagination = TRUE, paginationType = "jump", 
      showSortable = TRUE, filterable = TRUE, 
      columns = list(
        date_implemented = reactable::colDef(name = "Date implemented", 
                                             defaultSortOrder = "desc",
                                             filterable = FALSE),
        country = reactable::colDef(name = "Country"),
        measure = reactable::colDef(name = "Intervention"),
        comments = reactable::colDef(show = FALSE)
      ),
      details = function(index) {
        comment <- df[index,]$comments
        if(!is.na(comment)) {
          htmltools::div(style = "padding: 10px", comment)
        }
      }
    )
    w_tbl$hide()
    return(rtbl)
  })
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      addMapPane(name = "choropleth", zIndex = 410) %>%
      addMapPane(name = "polygons", zIndex = 420) %>%
      addMapPane(name = "borders", zIndex = 430) %>%
      addMapPane(name = "circles", zIndex = 440) %>%
      addMapPane(name = "place_labels", zIndex = 450) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      #addProviderTiles("CartoDB.PositronNoLabels", group = "Labels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels", group = "Place Labels", 
                       options = leafletOptions(pane = "place_labels")) %>%
      setView(0, 40, zoom = 1) %>% 
      #addScaleBar(position = "bottomleft") %>% 
      addControl(tags$div(tag.map.title, HTML("click country on<br>map to filter")), 
                 position = "bottomleft", className="map-title", layerId = "title") %>% 
      leaflet.extras::addFullscreenControl(position = "topleft") %>% 
      leaflet.extras::addResetMapButton() %>% 
      addLayersControl(
        #baseGroups = c("Labels", "No Labels"),
        overlayGroups = c("Place Labels", "Trends", "Cases/Deaths"),
        position = "topleft"
      )
    
  })
  
  # observeEvent(df_gi(), {
  #   
  #   if (input$intervention == "" | nrow(df_gi()) < 1) {
  #     leafletProxy("map", session) %>%
  #       clearGroup("Interventions") %>% 
  #       removeControl(layerId = "choro_legend")
  #   } else {
  #     
  #     df <- df_gi()
  #     
  #     if (input$region != "Worldwide") {
  #       if (input$region %in% continents) {
  #         df <- df %>% dplyr::filter(continent == input$region)
  #       } else {
  #         df <- df %>% dplyr::filter(region == input$region)
  #       }
  #     }
  #     
  #     countries <- unique(df$iso)
  #     
  #     sf_df <- sf_world %>% dplyr::filter(iso_a3 %in% countries)
  #     
  #     #popup_cols <- c("country", "measure", "date_implemented", "comments")
  #     
  #     leafletProxy("map", session) %>%
  #       clearGroup("Interventions") %>%
  #       removeControl(layerId = "choro_legend") %>% 
  #       addPolygons(
  #         data = sf_df,
  #         stroke = TRUE,
  #         color = "white",
  #         weight = 1,
  #         fillColor = "red",
  #         fillOpacity = .4,
  #         label = ~country,
  #         #popup = leafpop::popupTable(dat, zcol = popup_cols, row.numbers = FALSE, feature.id = FALSE),
  #         highlightOptions = highlightOptions(bringToFront = TRUE, fillOpacity = .5),
  #         group = "Interventions",
  #         options = pathOptions(pane = "choropleth")
  #       ) %>% 
  #       addLegend(
  #         position = "bottomright",
  #         title = "Intervention",
  #         colors = "red",
  #         labels = isolate(break_text_html(input$intervention)),
  #         layerId = "choro_legend",
  #         group = "Interventions"
  #       )
  #   }
  #   #w_map$hide()
  # })
  
  observe({
    
    df <- df_trends
    
    if (input$region != "Worldwide") {
      if (input$region %in% continents) {
        df <- df %>% dplyr::filter(continent == input$region)
      } else {
        df <- df %>% dplyr::filter(region == input$region)
      }
    }
    
    ind_lab <- stringr::str_to_title(input$indicator)
    
    sf_df <- sf_world %>% dplyr::inner_join(df, by = "iso_a3")
    sf_df$trend <- sf_df[[paste0("trend_", input$indicator)]]
    
    RdAmGn <- c('#D95F02','#E6AB02','#1B9E77')
    lvls <- c("Increasing", "Stable", "Declining")
    
    pal <- leaflet::colorFactor(palette = RdAmGn, levels = lvls)
    
    #popup_cols <- c("country", "measure", "date_implemented", "comments")
    
    leafletProxy("map", session) %>%
      clearGroup("Trends") %>%
      removeControl(layerId = "choro_legend") %>% 
      addPolygons(
        data = sf_df,
        stroke = TRUE,
        color = "white",
        weight = 1,
        fillColor = ~pal(trend),
        fillOpacity = .4,
        #label = ~iso_a3,
        #popup = leafpop::popupTable(dat, zcol = popup_cols, row.numbers = FALSE, feature.id = FALSE),
        highlightOptions = highlightOptions(bringToFront = TRUE, fillOpacity = .5),
        group = "Trends",
        options = pathOptions(pane = "choropleth")
      ) %>% 
      addLegend(
        position = "bottomright",
        #pal = pal,
        #values = ~trend,
        colors = c(RdAmGn, "#808080"),
        labels = c(lvls, "NA"),
        title = paste(ind_lab, "Trend"),
        layerId = "choro_legend",
        group = "Trends"
      )
  })
  
  observe({
    dat <- map_indicators()
    ind <- dat[[input$indicator]]
    ind_lab <- stringr::str_to_title(input$indicator)
    
    leafletProxy("map", session) %>% 
      clearGroup("Cases/Deaths") %>%
      removeControl(layerId = "circle_legend") %>% 
      addPolygons(
        data = dat,
        stroke = FALSE,
        fillOpacity = 0,
        label = ~glue::glue("<b>{country}</b><br>Cases: {scales::number(cases)}<br>Deaths: {scales::number(deaths)}") %>% purrr::map(htmltools::HTML),
        layerId = ~iso_a3,
        #group = "Indicators",
        options = pathOptions(pane = "polygons")
      ) %>% 
      addCircleMarkers(
        data = dat, 
        lng = ~lon, 
        lat = ~lat, 
        radius = ~calc_radius(ind), 
        fillColor = "#57AACB", 
        fillOpacity = 0.8, 
        weight = 1, 
        color = "#FFFFFF", 
        opacity = 1, 
        label = ~glue::glue("<b>{country}</b><br>Cases: {scales::number(cases)}<br>Deaths: {scales::number(deaths)}") %>% purrr::map(htmltools::HTML),
        #popup = leafpop::popupTable(dat, zcol = c("country", "cases"), row.numbers = FALSE, feature.id = FALSE),
        layerId = ~paste0(iso_a3, "_mrkr"),
        group = "Cases/Deaths",
        options = pathOptions(pane = "circles")
      ) %>% 
      addCircleLegend(
        title = paste("Confirmed", ind_lab),
        range = ind,
        scaling_fun = calc_radius,
        fillColor = "#57AACB", 
        fillOpacity = 0.8, 
        weight = 1, 
        color = "#FFFFFF", 
        position = "topright",
        layerId = "circle_legend",
        group = "Cases/Deaths"
      )
    
    w_map$hide()
  })
  
  observeEvent(input$region, {
    
    r_type <- region_type()
    
    if (r_type == "global") {
      leafletProxy("map", session) %>% 
        clearGroup("Borders") %>% 
        flyTo(0, 40, zoom = 1)
    } else {
      
      if (r_type == "continent") {
        sf_shps <- sf_world %>% dplyr::filter(continent == isolate(input$region))
      } else {
        sf_shps <- sf_world %>% dplyr::filter(region == isolate(input$region))
      }
      
      bbox <- sf::st_bbox(sf_shps)
      
      leafletProxy("map", session) %>% 
        clearGroup("Borders") %>% 
        addPolylines(
          data = sf_shps, 
          group = "Borders", 
          weight = 1, 
          color = "red", 
          opacity = 1,
          options = pathOptions(pane = "borders")
        ) %>% 
        flyToBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]])
    }
    
    #w_map$hide()
  })
  
  region_lab <- reactive({
    ifelse(region_type() == "country", names(country_iso[country_iso == region_select()]), region_select())
  })
  
  output$epicurve_title <- renderText({
    paste(region_lab(), "daily", input$indicator)
  })
  
  output$cumulative_title <- renderText({
    paste(region_lab(), "cumulative", input$indicator)
  })
  
  lockdown_lines <- reactive({
    lockdowns <- df_interventions %>%
      dplyr::filter(
        iso == region_select(),
        measure %in% c("Full lockdown", "Partial lockdown", "State of emergency declared")
      ) %>% 
      dplyr::group_by(measure) %>% 
      dplyr::filter(date_implemented == min(date_implemented, na.rm = TRUE)) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(x = datetime_to_timestamp(date_implemented)) %>% 
      dplyr::select(x, measure) %>% 
      dplyr::distinct() %>% 
      dplyr::group_by(x) %>% 
      dplyr::arrange(x) %>% 
      dplyr::filter(dplyr::row_number() == 1) %>% 
      dplyr::ungroup()
    
    # if ("Full lockdown" %in% lockdowns$measure & "Partial lockdown" %in% lockdowns$measure) {
    #   lockdowns <- lockdowns %>% dplyr::filter(measure != "Partial lockdown")
    # }
    
    lockdowns %>% dplyr::group_split(x, measure)
  })
  
  output$epicurve <- renderHighchart({
    #w$show()
    
    df <- df_epicurve()
    ind <- rlang::sym(input$indicator)
    
    x_min <- datetime_to_timestamp(as.Date(input$time_period[1]))
    y_max <- df %>% dplyr::count(date, wt = {{ind}}) %>% dplyr::pull(n) %>% max
    
    #browser()

    #title <- paste(region_lab(), "daily", input$indicator)
    y_lab <- stringr::str_to_title(input$indicator)
    
    p <- hchart(df, type = "column", hcaes(date, !!ind, group = country)) %>% # name = input$indicator
      hc_chart(zoomType = "x") %>% 
      #hc_title(text = title) %>% 
      #hc_subtitle(text = "click + drag horizontally to zoom") %>% 
      hc_xAxis(
        title = list(text = ""),
        min = datetime_to_timestamp(as.Date(input$time_period[1])),
        max = datetime_to_timestamp(as.Date(input$time_period[2]))
      ) %>% 
      hc_yAxis_multiples(
        list(
          title = list(text = y_lab), 
          #stackLabels = list(enabled = TRUE, align = "center"),
          allowDecimals = FALSE
        ),
        list(
          title = list(text = ""), 
          allowDecimals = FALSE,
          opposite = TRUE,
          linkedTo = 0
        )
      ) %>%
      hc_plotOptions(
        line = list(zIndex = 1, dashStyle = "ShortDash"),
        column = list(zIndex = 2, stacking = "normal", groupPadding = 0.05, pointPadding = 0.05, borderWidth = 0.05)
      ) %>% 
      hc_annotations(
        list(
          draggable = "",
          labels = list(
            list(point = list(x = x_min, y = y_max,  xAxis = 0, yAxis = 0), 
                 text = "click + drag horizontally<br>to zoom charts", style = list(color = "grey"), align = "left", backgroundColor = "transparent", borderWidth = 0)
          )
        )
      ) %>% 
      hc_legend(
        #title = list(text = "Top 9 + other"),
        layout = "vertical",
        align = "right",
        verticalAlign = "top",
        x = -10,
        y = 40
      )
    
    if (region_type() == "country") {
      
      p <- p %>% 
        hc_xAxis(
          plotLines = purrr::map(rev(lockdown_lines()), ~{
            df <- .x
            list(color = "red", zIndex = 1, value = unique(df$x), 
                 label = list(text = unique(df$measure), verticalAlign = "top", textAlign = "left"))
          })
        )
    }

    return(p)
    
    w_charts$hide()
  })
  
  observeEvent(input$indicator, {
    updateNumericInput(session, "n_days", label = paste("n", input$indicator)) 
  })
  
  observeEvent(input$set_days, {
    shinyjs::toggleElement(id = "n_days", condition = input$set_days, anim = TRUE)
  })
  
  output$cumulative <- renderHighchart({
    #w$show()
    
    df <- df_epicurve() %>% 
      dplyr::group_by(country) %>% 
      dplyr::arrange(date) %>% 
      dplyr::mutate_at(dplyr::vars(cases, deaths), cumsum) %>% 
      dplyr::ungroup()
    
    ind <- rlang::sym(input$indicator)
    
    x_min <-  datetime_to_timestamp(as.Date(input$time_period[1]))
    y_max <- df %>% dplyr::count(date, wt = {{ind}}) %>% dplyr::pull(n) %>% max
    
    
    if (input$set_days) {
      req(input$n_days)
      df <- df %>% 
        dplyr::group_by(country) %>% 
        tidyr::drop_na({{ind}}) %>% 
        dplyr::filter({{ind}} >= input$n_days) %>% 
        dplyr::mutate(date = seq_along({{ind}})-1) %>% 
        dplyr::ungroup()
      
      x_min <- min(df$date, na.rm = TRUE)
    }
    
    validate(need(nrow(df) > 0, "No countries meet the criteria..."))
    
    #title <- paste(region_lab(), "cumulative", input$indicator)
    xlab <- ifelse(input$set_days, paste("Days since first", input$n_days, input$indicator), "")
    
    y_lab <- stringr::str_to_title(input$indicator)
    if (input$log) y_lab <- paste(y_lab, "(log scale)")
    y_type <- ifelse(input$log, "logarithmic", "linear")
    y_min <- ifelse(input$log, 1, 0)
    y_min <- ifelse(input$set_days, input$n_days, y_min)
    
    p <- hchart(df, type = "line", hcaes(date, !!ind, group = country)) %>% #, name = input$indicator
      hc_chart(zoomType = "x") %>% 
      #hc_subtitle(text = "click + drag horizontally to zoom") %>% 
      hc_xAxis(
        title = list(text = xlab)
        #min = datetime_to_timestamp(as.Date(input$time_period[1])),
        #max = datetime_to_timestamp(as.Date(input$time_period[2]))
      ) %>% 
      hc_yAxis_multiples(
        list(
          title = list(text = y_lab), 
          allowDecimals = FALSE,
          type = y_type,
          min = y_min
        ),
        list(
          title = list(text = ""), 
          allowDecimals = FALSE,
          type = y_type,
          opposite = TRUE,
          min = y_min,
          linkedTo = 0
        )
      ) %>%
      hc_plotOptions(
        line = list(zIndex = 2)
      ) %>% 
      # hc_annotations(
      #   list(
      #     draggable = "",
      #     labels = list(
      #       list(point = list(x = x_min, y = y_max,  xAxis = 0, yAxis = 0), 
      #            text = "click + drag horizontally to zoom", style = list(color = "grey"), align = "left", backgroundColor = "transparent", borderWidth = 0)
      #     )
      #   )
      # ) %>% 
      hc_legend(
        #title = list(text = "Top 9 + other"), 
        layout = "proximate", 
        align = "right"
      ) 
    
    if (region_type() == "country") {
      p <- p %>% 
        hc_xAxis(
          plotLines = purrr::map(rev(lockdown_lines()), ~{
            df <- .x
            list(color = "red", zIndex = 1, value = unique(df$x), 
                 label = list(text = unique(df$measure), verticalAlign = "top", textAlign = "left"))
          })
        )
    }
    
    return(p)
    
    w_charts$hide()
  })
  
}

