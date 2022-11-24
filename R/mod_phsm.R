#' phsm UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_phsm_ui <- function(id){
  ns <- NS(id)

  geo_choices <- df_phsm %>%
    dplyr::distinct(iso, flag, country_territory_area) %>%
    tidyr::replace_na(list(flag = "")) %>%
    dplyr::transmute(iso, country_lab = stringr::str_trim(paste(flag, country_territory_area)))
  geo_choices <- setNames(geo_choices$iso, geo_choices$country_lab)

  tagList(
    shinydashboard::box(
      solidHeader = TRUE, width = 12, collapsible = TRUE, 
      title = tags$b("Filters"),
      div_inline(shinyWidgets::pickerInput(
        inputId = ns("geo"),
        label = "Country/Territory/Area",
        choices = geo_choices,
        options = picker_opts(actions = FALSE),
        multiple = TRUE,
        width = 200
      ), margin_right = TRUE),
      div_inline(shinyWidgets::pickerInput(
        inputId = ns("category"),
        label = "Type of measure",
        choices = dplyr::count(df_phsm, who_category, sort = TRUE) %>% dplyr::pull(who_category),
        options = picker_opts(actions = FALSE),
        multiple = TRUE,
        width = 200
      ), margin_right = TRUE),
      div_inline(shiny::checkboxInput(ns("active"), "Active measures only?", value = TRUE))
    ),
    shinydashboard::box(
      solidHeader = TRUE, width = 12,
      title = tagList(tags$b("Public Health & Safety Measures"), tags$small("click rows for more information")),
      reactable::reactableOutput(ns("tbl"))
    )
  )
}
    
#' phsm Server Function
#'
#' @noRd 
mod_phsm_server <- function(input, output, session){
  ns <- session$ns

  rt_details_row <- function() {
    reactable::JS(
      "function(rowInfo) {
           var subcat = rowInfo.row['who_subcategory'];
           var measure = rowInfo.row['who_measure'];
           var comments = rowInfo.row['comments'];
           var link = rowInfo.row['link'];

           if (subcat !== null) {
            var subcat_html = '<b>Subcategory: </b>' + subcat + '</br>'
           } else {
            var subcat_html = ''
           }

           if (measure !== null) {
            var measure_html = '<b>Measure: </b>' + measure + '</br>'
           } else {
            var measure_html = ''
           }
           
           if (comments !== null) {
            var comments_html = '<b>Comments: </b>' + comments + '</br>'
           } else {
            var comments_html = ''
           }

           if (link !== null) {
            var link_html = '<b><a href=\"' + link + '\" target=\"_blank\">Link to data source</a></b></br>'
           } else {
            var link_html = ''
           }
           
           return '<div style = \"padding: 10px\">' + subcat_html + measure_html + comments_html + link_html + '</div>';
         }"
    )
  }

  df_tbl <- reactive({
    df_tbl <- df_phsm
    if (input$active) df_tbl %<>% dplyr::filter(is.na(date_end))
    if (length(input$geo)) df_tbl %<>% dplyr::filter(iso %in% input$geo)
    if (length(input$category)) df_tbl %<>% dplyr::filter(who_category %in% input$category)

    df_tbl %>%
      dplyr::select(
        flag,
        country_territory_area,
        admin_level,
        area_covered,
        who_category,
        who_subcategory,
        who_measure,
        comments,
        targeted,
        date_start,
        date_end,
        link
      ) %>%
      dplyr::arrange(desc(date_start))
  })

  output$tbl <- reactable::renderReactable({
    reactable::reactable(
      data = isolate(df_tbl()),
      highlight = TRUE, compact = TRUE, showSortable = TRUE,
      defaultSorted = "date_start", showSortIcon = TRUE,
      onClick = "expand", rowStyle = list(cursor = "pointer"),
      details = reactable::colDef(
        name = "+",
        footer = "",
        details = rt_details_row(),
        html = TRUE
      ),
      columns = list(
        flag = reactable::colDef(name = "", width = 50),
        country_territory_area = reactable::colDef("Country/Territory/Area"),
        admin_level = reactable::colDef("Level"),
        area_covered = reactable::colDef("Area Covered"),
        who_category = reactable::colDef("Category"),
        targeted = reactable::colDef("Targeted"),
        date_start = reactable::colDef(
          "Started", 
          defaultSortOrder = "desc",
          format = reactable::colFormat(date = TRUE, locales = c("fr-FR", "en-GB"))
        ),
        date_end = reactable::colDef(
          "Ended", 
          format = reactable::colFormat(date = TRUE, locales = c("fr-FR", "en-GB")),
          na = "Active"
        ),
        who_subcategory = reactable::colDef(show = FALSE),
        who_measure = reactable::colDef(show = FALSE),
        comments = reactable::colDef(show = FALSE),
        link = reactable::colDef(show = FALSE)
      )
    )
  })

  observeEvent(df_tbl(), ignoreInit = TRUE, {
    reactable::updateReactable("tbl", data = df_tbl())
  })
}
    
## To be copied in the UI
# mod_phsm_ui("phsm")
    
## To be copied in the server
# callModule(mod_phsm_server, "phsm")
 
