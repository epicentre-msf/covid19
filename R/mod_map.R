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
mod_map_ui <- function(id) {
  ns <- NS(id)

  region_selects <- dplyr::bind_rows(
    all_continents,
    dplyr::distinct(sf_world %>% sf::st_set_geometry(NULL), continent, region)
  )
  region_selects <- c("Worldwide", split(region_selects$region, region_selects$continent))

  tagList(

    # fluidRow(
    #   col_3(
    #     selectInput(
    #       ns("region"),
    #       "Region focus",
    #       choices = region_selects,
    #       width = "100%"
    #     )
    #   ),
    #   col_3(
    #     shinyWidgets::radioGroupButtons(
    #       inputId = ns("source"),
    #       label = "Data source",
    #       choices = c("JHU CSSE (daily)" = "JHU CSSE", "ECDC (weekly)" = "ECDC"), # "WHO",
    #       justified = TRUE,
    #       size = "sm"
    #     )
    #   ),
    #   col_3(
    #     shinyWidgets::radioGroupButtons(
    #       inputId = ns("indicator"),
    #       label = "Indicator",
    #       choices = c("Cases" = "cases", "Deaths" = "deaths"),
    #       justified = TRUE,
    #       size = "sm"
    #     )
    #   ),
    #   col_3(
    #     dateRangeInput(
    #       ns("time_period"),
    #       label = "Time period",
    #       min = min(df_jhcsse$date, na.rm = TRUE),
    #       max = max(df_jhcsse$date, na.rm = TRUE),
    #       start = min(df_jhcsse$date, na.rm = TRUE),
    #       end = max(df_jhcsse$date, na.rm = TRUE),
    #       # value = c(
    #       #   min(df_jhcsse$date, na.rm = TRUE),
    #       #   max(df_jhcsse$date, na.rm = TRUE)
    #       # ),
    #       # step = 1,
    #       width = "100%"
    #     )
    #     # dateRangeInput(
    #     #   ns("time_period"),
    #     #   label = "Time period",
    #     #   min = min(df_ecdc$date, na.rm = TRUE),
    #     #   max = Sys.Date(),
    #     #   start = min(df_ecdc$date, na.rm = TRUE),
    #     #   end = Sys.Date(),
    #     #   width = "100%"
    #     # )
    #   )
    #   # col_4(
    #   #   selectInput(
    #   #     ns("intervention"),
    #   #     "Interventions",
    #   #     choices = c("Highlight government interventions" = "", sort(unique(df_interventions$measure))),
    #   #     width = "100%"
    #   #   )
    #   # )
    # ),

    fluidRow(
      col_2(
        selectInput(
          ns("region"),
          "Region focus",
          choices = region_selects,
          width = "100%"
        ),
        shinyWidgets::radioGroupButtons(
          inputId = ns("source"),
          label = "Data source",
          choices = "WHO", # c("JHU CSSE" = "JHU CSSE", "ECDC" = "ECDC")
          justified = TRUE,
          size = "sm"
        ),
        shinyWidgets::radioGroupButtons(
          inputId = ns("indicator"),
          label = "Indicator",
          choices = c("Cases" = "cases", "Deaths" = "deaths"),
          justified = TRUE,
          size = "sm"
        ),
        dateRangeInput(
          ns("time_period"),
          label = "Time period",
          min = min(df_who$date, na.rm = TRUE),
          max = max(df_who$date, na.rm = TRUE),
          start = min(df_who$date, na.rm = TRUE),
          end = max(df_who$date, na.rm = TRUE),
          width = "100%"
        )
      ),

      col_4(
        uiOutput(ns("totals"))
      ),

      shinydashboard::box(
        width = 6, solidHeader = TRUE,
        leaflet::leafletOutput(ns("map"))
      )

      # shinydashboard::box(
      #   # title = tags$b("Government interventions"),
      #   width = 4, solidHeader = TRUE,
      #   reactable::reactableOutput(ns("table"))
      # )
    ),

    fluidRow(
      col_12(
        box_w_inputs(
          width = 12,
          title = textOutput(ns("epicurve_title")),
          inputs = tagList(
            shinyWidgets::radioGroupButtons(
              inputId = ns("chart_type"),
              label = NULL,
              choices = c(`<i class='fa fa-bar-chart'></i>` = "column", `<i class='fa fa-line-chart'></i>` = "line"),
              size = "sm"
            )
          ),
          highcharter::highchartOutput(ns("epicurve"))
        )
        # shinydashboard::box(
        #   width = NULL, solidHeader = TRUE,
        #   title = tagList(
        #     tags$div(textOutput(ns("epicurve_title")), style = "display: inline-block; font-weight: bold;") # ,
        #     # tags$div(tags$small("click + drag horizontally to zoom"), style = "display: inline-block;")
        #   ),
        #   highcharter::highchartOutput(ns("epicurve"))
        # )
      )

      # col_6(
      #   shinydashboard::box(
      #     width = NULL, solidHeader = TRUE,
      #     title = tagList(
      #       tags$div(textOutput(ns("cumulative_title")), style = "display: inline-block; font-weight: bold;"),
      #       tags$div(style = "display: inline-block; padding-right: 10px;"),
      #       tags$div(
      #         style = "display: inline-block;",
      #         shinyWidgets::dropdownButton(
      #           size = "xs", label = "params", icon = icon("sliders"), # status = "primary",
      #           inline = TRUE, width = "50px", circle = FALSE,
      #           # checkboxGroupInput(ns("c_params"), label = "", inline = FALSE,
      #           #                    choices = c("log scale" = "log", "set days since" = "days")),
      #           checkboxInput(ns("log"), label = "log scale", value = TRUE),
      #           checkboxInput(ns("set_days"), label = "xaxis to days since...", value = FALSE),
      #           tags$br(),
      #           numericInput(ns("n_days"), label = paste("n", "cases"), value = 10, min = 1, step = 10)
      #           # uiOutput(ns("n_days_ui"))
      #         )
      #       )
      #     ),
      #     highcharter::highchartOutput(ns("cumulative"))
      #   )
      # )
    )
  )
}

# Module Server

#' @rdname mod_map
#' @export
#' @keywords internal
#' @import leaflet
#' @import highcharter
mod_map_server <- function(input, output, session) {
  ns <- session$ns

  # render_table <- function(data, selected_region, region_type) {
  #   df <- data %>% tidyr::drop_na(iso, measure)
  #   df <- df %>%
  #     filter_geo(selected_region, region_type, iso_col = iso) %>%
  #     dplyr::select(date_implemented, country, measure, comments)
  # 
  #   rtbl <- reactable::reactable(
  #     data = df,
  #     height = 400, searchable = FALSE, defaultSorted = "date_implemented",
  #     compact = TRUE, highlight = TRUE, pagination = TRUE, paginationType = "jump",
  #     showSortable = TRUE, filterable = TRUE,
  #     columns = list(
  #       date_implemented = reactable::colDef(
  #         name = "Date implemented",
  #         defaultSortOrder = "desc",
  #         filterable = FALSE
  #       ),
  #       country = reactable::colDef(name = "Country"),
  #       measure = reactable::colDef(name = "Intervention"),
  #       comments = reactable::colDef(show = FALSE)
  #     ),
  #     # This is >4 secs faster, but shows expand icon in each row
  #     details = reactable::colDef(
  #       name = "",
  #       details = reactable::JS(
  #         "function(rowInfo) {
  #          if (rowInfo.row['comments'] !== null) {
  #            return '<div style = \"padding: 10px\">' + rowInfo.row['comments'] + '</div>'
  #          }
  #          return '<div style = \"padding: 10px\">No further information available</div>';
  #        }"
  #       ),
  #       html = TRUE,
  #       width = 60
  #     )
  #     # details = function(index) {
  #     #   comment <- df[index,]$comments
  #     #   if(!is.na(comment)) {
  #     #     htmltools::div(style = "padding: 10px", comment)
  #     #   }
  #     # }
  #   )
  # }

  render_epicurve <- function(data, indicator, time_period, region_type, chart_type) {
    df <- data
    ind <- rlang::sym(indicator)

    x_min <- datetime_to_timestamp(as.Date(time_period[1]))
    y_max <- df %>%
      dplyr::count(date, wt = {{ ind }}) %>%
      dplyr::pull(n) %>%
      max()
    # title <- paste(region_lab(), "daily", input$indicator)
    y_lab <- stringr::str_to_title(indicator)

    mapping <- hcaes(date, !!ind, group = country)
    data <- mutate_mapping(df, mapping) %>% factor_to_char(as.character(mapping$group))
    series <- data_to_series(data, type = chart_type)
    opts <- highcharter:::data_to_options(data, chart_type)

    p <- highchart() %>%
      hc_add_series_list(series) %>%
      hc_title(text = NULL) %>%
      hc_caption(enabled = FALSE) %>% 
      hc_xAxis(type = opts$xAxis_type, title = list(text = as.character(mapping$x)), categories = opts$xAxis_categories) %>%
      hc_yAxis(type = opts$yAxis_type, title = list(text = as.character(mapping$y)), categories = opts$yAxis_categories) %>%
      hc_plotOptions(
        series = list(showInLegend = opts$series_plotOptions_showInLegend),
        scatter = list(marker = list(symbol = "circle"))
      ) %>%
      hc_chart(zoomType = "x") %>%
      # hc_title(text = title) %>%
      # hc_subtitle(text = "click + drag horizontally to zoom") %>%
      hc_xAxis(
        title = list(text = ""),
        min = datetime_to_timestamp(as.Date(time_period[1])),
        max = datetime_to_timestamp(as.Date(time_period[2])),
        crosshair = TRUE
      ) %>%
      highcharter::hc_rangeSelector(enabled = TRUE, selected = 4) %>%
      highcharter::hc_navigator(enabled = TRUE) %>%
      hc_yAxis_multiples(
        list(
          title = list(text = y_lab),
          # stackLabels = list(enabled = TRUE, align = "center"),
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
        line = list(zIndex = 1, marker = list(enabled = FALSE), label = list(enabled = TRUE)),
        column = list(zIndex = 2, stacking = "normal", groupPadding = 0.05, pointPadding = 0.05, borderWidth = 0.05)
      ) %>%
      hc_annotations(
        list(
          draggable = "",
          labels = list(
            list(
              point = list(x = x_min, y = y_max, xAxis = 0, yAxis = 0),
              text = "click + drag horizontally<br>to zoom charts", style = list(color = "grey"), align = "left", backgroundColor = "transparent", borderWidth = 0
            )
          )
        )
      ) %>%
      hc_tooltip(shared = TRUE) %>% 
      hc_title(text = NULL) %>% 
      hc_exporting(enabled = FALSE) %>% 
      hc_credits(enabled = FALSE) 
    
    if (chart_type == "column") {
      p <- p %>% 
        hc_legend(
          # title = list(text = "Top 9 + other"),
          layout = "vertical",
          align = "right",
          verticalAlign = "top",
          x = -10,
          y = 40
        )
    } else if (chart_type == "line") {
      p <- p %>% 
        hc_legend(layout = "proximate", align = "right")
    }
    
    # if (region_type == "country") {
    #   p <- p %>%
    #     hc_xAxis(
    #       plotLines = purrr::map(rev(lockdown_lines), ~ {
    #         df <- .x
    #         list(
    #           color = "red", zIndex = 1, value = unique(df$x),
    #           label = list(text = unique(df$measure), verticalAlign = "top", textAlign = "left")
    #         )
    #       })
    #     )
    # }
    return(p)
  }

  # render_cumulative <- function(data, indicator, time_period, set_days, n_days, log, region_type, lockdown_lines) {
  #   df <- data %>%
  #     dplyr::group_by(country) %>%
  #     dplyr::arrange(date) %>%
  #     dplyr::mutate_at(dplyr::vars(cases, deaths), cumsum) %>%
  #     dplyr::ungroup()
  # 
  #   ind <- rlang::sym(indicator)
  # 
  #   x_min <- datetime_to_timestamp(as.Date(time_period[1]))
  #   y_max <- df %>%
  #     dplyr::count(date, wt = {{ ind }}) %>%
  #     dplyr::pull(n) %>%
  #     max()
  # 
  # 
  #   if (set_days) {
  #     req(n_days)
  #     df <- df %>%
  #       dplyr::group_by(country) %>%
  #       tidyr::drop_na({{ ind }}) %>%
  #       dplyr::filter({{ ind }} >= n_days) %>%
  #       dplyr::mutate(date = seq_along({{ ind }}) - 1) %>%
  #       dplyr::ungroup()
  # 
  #     x_min <- min(df$date, na.rm = TRUE)
  #   }
  # 
  #   validate(need(nrow(df) > 0, "No countries meet the criteria..."))
  # 
  #   # title <- paste(region_lab(), "cumulative", input$indicator)
  #   xlab <- ifelse(set_days, paste("Days since first", n_days, indicator), "")
  # 
  #   y_lab <- stringr::str_to_title(indicator)
  #   if (log) y_lab <- paste(y_lab, "(log scale)")
  #   y_type <- ifelse(log, "logarithmic", "linear")
  #   y_min <- ifelse(log, 1, 0)
  #   y_min <- ifelse(set_days, n_days, y_min)
  # 
  #   mapping <- hcaes(date, !!ind, group = country)
  #   data <- mutate_mapping(df, mapping) %>% factor_to_char(as.character(mapping$group))
  #   series <- data_to_series(data, type = "line")
  #   opts <- highcharter:::data_to_options(data, "line")
  # 
  #   p <- highchart() %>%
  #     hc_add_series_list(series) %>%
  #     hc_xAxis(type = opts$xAxis_type, title = list(text = as.character(mapping$x)), categories = opts$xAxis_categories) %>%
  #     hc_yAxis(type = opts$yAxis_type, title = list(text = as.character(mapping$y)), categories = opts$yAxis_categories) %>%
  #     hc_plotOptions(
  #       series = list(showInLegend = opts$series_plotOptions_showInLegend),
  #       scatter = list(marker = list(symbol = "circle"))
  #     ) %>%
  #     hc_chart(zoomType = "x") %>%
  #     # hc_subtitle(text = "click + drag horizontally to zoom") %>%
  #     hc_xAxis(
  #       title = list(text = xlab),
  #       crosshair = TRUE
  #       # min = datetime_to_timestamp(as.Date(input$time_period[1])),
  #       # max = datetime_to_timestamp(as.Date(input$time_period[2]))
  #     ) %>%
  #     hc_yAxis_multiples(
  #       list(
  #         title = list(text = y_lab),
  #         allowDecimals = FALSE,
  #         type = y_type,
  #         min = y_min
  #       ),
  #       list(
  #         title = list(text = ""),
  #         allowDecimals = FALSE,
  #         type = y_type,
  #         opposite = TRUE,
  #         min = y_min,
  #         linkedTo = 0
  #       )
  #     ) %>%
  #     hc_plotOptions(
  #       line = list(zIndex = 2)
  #     ) %>%
  #     # hc_annotations(
  #     #   list(
  #     #     draggable = "",
  #     #     labels = list(
  #     #       list(point = list(x = x_min, y = y_max,  xAxis = 0, yAxis = 0),
  #     #            text = "click + drag horizontally to zoom", style = list(color = "grey"), align = "left", backgroundColor = "transparent", borderWidth = 0)
  #     #     )
  #     #   )
  #     # ) %>%
  #     hc_legend(
  #       # title = list(text = "Top 9 + other"),
  #       layout = "proximate",
  #       align = "right"
  #     ) %>% 
  #     hc_tooltip(shared = TRUE)
  # 
  #   if (region_type == "country") {
  #     p <- p %>%
  #       hc_xAxis(
  #         plotLines = purrr::map(rev(lockdown_lines), ~ {
  #           df <- .x
  #           list(
  #             color = "red", zIndex = 1, value = unique(df$x),
  #             label = list(text = unique(df$measure), verticalAlign = "top", textAlign = "left")
  #           )
  #         })
  #       )
  #   }
  #   return(p)
  # }

  if (getOption("use.cache", default = TRUE)) {
    cache_storage <- memoise::cache_filesystem(getOption("cache.path", default = "./.rcache"))
    # render_table_active <- memoise::memoise(render_table, cache = cache_storage)
    render_epicurve_active <- memoise::memoise(render_epicurve, cache = cache_storage)
    # render_cumulative_active <- memoise::memoise(render_cumulative, cache = cache_storage)
  } else {
    # render_table_active <- render_table
    render_epicurve_active <- render_epicurve
    # render_cumulative_active <- render_cumulative
  }

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
    if (map_click()) {
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
      WHO = df_who
      # ECDC = df_ecdc,
      # `JHU CSSE` = df_jhcsse
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
      # dplyr::left_join(dplyr::select(df_trends_new, iso_a3, trend_cases, trend_deaths), by = "iso_a3") %>% 
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
      tidyr::drop_na(country, {{ ind }}) %>%
      dplyr::mutate(
        date = lubridate::floor_date(date, "week", week_start = 1),
        country = forcats::fct_lump(country, n = 9, other_level = "Other", w = {{ ind }})
      ) %>%
      dplyr::group_by(date, country) %>%
      dplyr::summarise(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      tidyr::drop_na()

    return(df)
  })

  # Outputs ========================================================

  # output$totals <- renderUI({
  #   w_totals$show()
  #   df <- df_data() %>%
  #     filter_geo(r_filter = region_select(), r_type = region_type(), iso_col = iso_a3) %>%
  #     dplyr::filter(date >= input$time_period[1], date <= input$time_period[2]) %>%
  #     dplyr::summarise(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE))

  #   w_totals$hide()

  #   div(
  #     class = "text-center",
  #     h2(region_select(), style = "font-weight: bold;"),
  #     shinydashboard::valueBox(countup::countup(df$cases), "Confirmed Cases", width = 12, color = "blue"),
  #     shinydashboard::valueBox(countup::countup(df$deaths), "Confirmed Deaths", width = 12, color = "red")
  #   )
  # })

  output$totals <- renderUI({
  
    df_ts <- df_data() %>%
      filter_geo(r_filter = region_select(), r_type = region_type(), iso_col = iso_a3) %>%
      dplyr::filter(
        date >= input$time_period[1],
        date <= input$time_period[2]
      ) %>%
      dplyr::mutate(date = lubridate::floor_date(date, "week", week_start = 1),) %>% 
      dplyr::group_by(date) %>%
      dplyr::summarise(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE)) %>%
      dplyr::ungroup()
      # dplyr::arrange(date) %>%
      # dplyr::mutate(
      #   cases_7d = zoo::rollmean(cases, k = 7, fill = NA),
      #   deaths_7d = zoo::rollmean(deaths, k = 7, fill = NA)
      # ) %>%
      # tidyr::drop_na()
  
    hc_cases <- hchart(df_ts, "spline", hcaes(date, cases), name = "weekly cases") %>%
      hc_size(height = 150) %>%
      hc_title(text = NULL) %>% 
      hc_exporting(enabled = FALSE) %>% 
      hc_credits(enabled = FALSE) %>%
      hc_add_theme(hc_theme_sparkline_vb())

    hc_deaths <- hchart(df_ts, "spline", hcaes(date, deaths), name = "weekly deaths") %>%
      hc_size(height = 150) %>%
      hc_title(text = NULL) %>% 
      hc_exporting(enabled = FALSE) %>% 
      hc_credits(enabled = FALSE) %>%
      hc_add_theme(hc_theme_sparkline_vb())
  
    #hc_deaths <- hchart(df_ts, "column", hcaes(date, deaths), name = "Daily deaths")  %>%
      #hc_elementId(id = "hc-deaths-mini") %>%
      #hc_chart(className = "hc-hide") %>%
      #hc_size(height = 100) 
  
    df_total <- df_ts %>% dplyr::summarise(cases = sum(cases), deaths = sum(deaths))
  
    tagList(
      col_12(class = "text-center", h2(region_select(), style = "font-weight: bold;")),
      valueBoxSpark(
        value = countup::countup(df_total$cases), 
        title = "Confirmed Cases", 
        sparkobj = hc_cases, 
        width = 6, 
        color = "blue"
        # info = "line shows 7 day rolling avergae"
      ),
      valueBoxSpark(
        value = countup::countup(df_total$deaths), 
        title = "Confirmed Deaths", 
        sparkobj = hc_deaths, 
        width = 6, 
        color = "red"
        # info = "line shows 7 day rolling avergae"
      )
    )
  
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapPane(name = "choropleth", zIndex = 410) %>%
      addMapPane(name = "polygons", zIndex = 420) %>%
      addMapPane(name = "borders", zIndex = 430) %>%
      addMapPane(name = "circles", zIndex = 440) %>%
      addMapPane(name = "place_labels", zIndex = 450) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      # addProviderTiles("CartoDB.PositronNoLabels", group = "Labels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels",
        group = "Place Labels",
        options = leafletOptions(pane = "place_labels")
      ) %>%
      setView(0, 40, zoom = 1) %>%
      # addScaleBar(position = "bottomleft") %>%
      addControl(tags$div(tag.map.title, HTML("click country on<br>map to filter")),
        position = "bottomleft", className = "map-title", layerId = "title"
      ) %>%
      leaflet.extras::addFullscreenControl(position = "topleft") %>%
      leaflet.extras::addResetMapButton() %>%
      addLayersControl(
        # baseGroups = c("Labels", "No Labels"),
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

    sf_df <- sf_world %>% dplyr::inner_join(dplyr::select(df, iso_a3, trend_cases_14d, trend_deaths_14d), by = "iso_a3")
    sf_df$trend <- sf_df[[paste0("trend_", input$indicator, "_14d")]]

    # RdAmGn <- c("#D95F02", "#E6AB02", "#1B9E77")
    # lvls <- c("Increasing", "Stable", "Declining")
    # pal <- leaflet::colorFactor(palette = RdAmGn, levels = lvls)
    
    lvls <- c("Increasing", "Likely increasing", "Stable", "Likely decreasing", "Decreasing")
    values <- scico::scico(5, palette = "vikO", begin = .2, end = .8, direction = -1) %>% purrr::set_names(lvls)
    pal <- leaflet::colorFactor(palette = values, levels = names(values), ordered = TRUE, na.color = NA)

    leafletProxy("map", session) %>%
      clearGroup("Trends") %>%
      removeControl(layerId = "choro_legend") %>%
      addPolygons(
        data = sf_df,
        stroke = TRUE,
        color = "white",
        weight = 1,
        fillColor = ~pal(trend),
        fillOpacity = .6,
        highlightOptions = highlightOptions(bringToFront = TRUE, fillOpacity = .5),
        group = "Trends",
        options = pathOptions(pane = "choropleth")
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = factor(names(values), levels = names(values)),
        title = paste("14 day", ind_lab, "Trend"),
        layerId = "choro_legend",
        group = "Trends"
      )
      # addLegend(
      #   position = "bottomright",
      #   # pal = pal,
      #   # values = ~trend,
      #   colors = c(RdAmGn, "#808080"),
      #   labels = c(lvls, "NA"),
      #   title = paste(ind_lab, "Trend"),
      #   layerId = "choro_legend",
      #   group = "Trends"
      # )
  })

  observe({
    dat <- map_indicators()
    ind <- dat[[input$indicator]]
    ind_lab <- stringr::str_to_title(input$indicator)
    # trend <- dat[[paste0("trend_", input$indicator, "_14d")]]
    
    # lvls <- c("Increasing", "Likely increasing", "Stable", "Likely decreasing", "Decreasing")
    # pal_bw <- leaflet::colorFactor(palette = c("#FFFFFF", rep("#808080", 3), "#FFFFFF"), levels = lvls, ordered = TRUE, na.color = NA)

    leafletProxy("map", session) %>%
      clearGroup("Cases/Deaths") %>%
      removeControl(layerId = "circle_legend") %>%
      addPolygons(
        data = dat,
        stroke = FALSE,
        fillOpacity = 0,
        label = ~ glue::glue("<b>{country}</b><br>Cases: {scales::number(cases, accuracy = 1)}<br>Deaths: {scales::number(deaths, accuracy = 1)}") %>% purrr::map(htmltools::HTML),
        layerId = ~iso_a3,
        # group = "Indicators",
        options = pathOptions(pane = "polygons")
      ) %>%
      addCircleMarkers(
        data = dat,
        lng = ~lon,
        lat = ~lat,
        radius = ~calc_radius(ind),
        weight = 1,
        color = "#808080",
        fill = FALSE,
        opacity = 1,
        # fillColor = "#57AACB",
        # fillOpacity = 0.8,
        label = ~ glue::glue("<b>{country}</b><br>Cases: {scales::number(cases, accuracy = 1)}<br>Deaths: {scales::number(deaths, accuracy = 1)}") %>% purrr::map(htmltools::HTML),
        layerId = ~ paste0(iso_a3, "_mrkr"),
        group = "Cases/Deaths",
        options = pathOptions(pane = "circles")
      ) %>%
      addCircleLegend(
        title = paste("Confirmed", ind_lab),
        range = ind,
        scaling_fun = calc_radius,
        fillColor = "#FFFFFF",
        fillOpacity = 0.6,
        weight = 1,
        color = "#808080",
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

    # w_map$hide()
  })

  region_lab <- reactive({
    ifelse(region_type() == "country", names(country_iso[country_iso == region_select()]), region_select())
  })

  output$epicurve_title <- renderText({
    paste(region_lab(), "weekly", input$indicator)
  })

  # output$cumulative_title <- renderText({
  #   paste(region_lab(), "cumulative", input$indicator)
  # })

  # lockdown_lines <- reactive({
  #   lockdowns <- df_interventions %>%
  #     dplyr::filter(
  #       iso == region_select(),
  #       measure %in% c("Full lockdown", "Partial lockdown", "State of emergency declared")
  #     ) %>%
  #     dplyr::group_by(measure) %>%
  #     dplyr::filter(date_implemented == min(date_implemented, na.rm = TRUE)) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::mutate(x = datetime_to_timestamp(date_implemented)) %>%
  #     dplyr::select(x, measure) %>%
  #     dplyr::distinct() %>%
  #     dplyr::group_by(x) %>%
  #     dplyr::arrange(x) %>%
  #     dplyr::filter(dplyr::row_number() == 1) %>%
  #     dplyr::ungroup()
  # 
  #   # if ("Full lockdown" %in% lockdowns$measure & "Partial lockdown" %in% lockdowns$measure) {
  #   #   lockdowns <- lockdowns %>% dplyr::filter(measure != "Partial lockdown")
  #   # }
  # 
  #   lockdowns %>% dplyr::group_split(x, measure)
  # })

  output$epicurve <- renderHighchart({
    on.exit(w_charts$hide())
    data_value <- df_epicurve()
    indicator_value <- input$indicator
    time_period_value <- input$time_period
    region_type_value <- region_type()
    # lockdown_lines_value <- lockdown_lines()
    chart_type <- input$chart_type
    render_epicurve_active(data_value, indicator_value, time_period_value, region_type_value, chart_type)
  })
  
  observe({
    if (input$chart_type == "bar") {
      highcharter::highchartProxy(ns("epicurve")) %>%
        highcharter::hcpxy_set_data(
          type = "column",
          data = df_epicurve(),
          mapping = hcaes(date, !!rlang::sym(input$indicator), group = country),
          redraw = TRUE
        ) %>% 
        highcharter::hcpxy_update(
          # chart = list(type = "column"),
          legend = list(
            layout = "vertical",
            align = "right",
            verticalAlign = "top",
            x = -10,
            y = 40
          )
        )
    } else if (input$chart_type == "line") {
      highcharter::highchartProxy(ns("epicurve")) %>%
        highcharter::hcpxy_set_data(
          type = "line",
          data = df_epicurve(),
          mapping = hcaes(date, !!rlang::sym(input$indicator), group = country),
          redraw = TRUE
        ) %>% 
        highcharter::hcpxy_update(
          # chart = list(type = "line"),
          legend = list(layout = "proximate", align = "right")
        )
        
    }
  }) %>% bindEvent(NULL, ignoreInit = TRUE)

  # observeEvent(input$indicator, {
  #   updateNumericInput(session, "n_days", label = paste("n", input$indicator))
  # })
  # 
  # observeEvent(input$set_days, {
  #   shinyjs::toggleElement(id = "n_days", condition = input$set_days, anim = TRUE)
  # })
  # 
  # output$cumulative <- renderHighchart({
  #   # w$show()
  #   data_value <- df_epicurve()
  #   indicator_value <- input$indicator
  #   time_period_value <- input$time_period
  #   set_days_value <- input$set_days
  #   n_days_value <- input$n_days
  #   log_value <- input$log
  #   region_type_value <- region_type()
  #   lockdown_lines_value <- lockdown_lines()
  # 
  #   p <- render_cumulative_active(
  #     data_value, indicator_value, time_period_value, set_days_value,
  #     n_days_value, log_value, region_type_value, lockdown_lines_value
  #   )
  # 
  #   return(p)
  # 
  #   w_charts$hide()
  # })
}