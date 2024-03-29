#' @import shiny
app_ui <- function() {
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage(
      title = tagList("COVID-19 Epi Dashboard", tags$small(paste("data updated", data_updated))),
      windowTitle = "COVID-19 Epi Dashboard",
      position = "fixed-top",
      collapsible = TRUE,
      
      # tabs 
      tabPanel(title = icon("globe-africa"), mod_map_ui("map_1")), #"Global overview", 

      tabPanel(title = "PHSM", icon = icon("table"), mod_phsm_ui("phsm")),
      
      tabPanel(
        title = "About",
        icon = icon("info"), 
        fluidRow(
          column(
            width = 8, offset = 2,
            tabsetPanel(
              tabPanel("About", includeMarkdown(system.file('app/www/about.md', package = 'covid19'))),
              tabPanel("Trends", includeMarkdown(system.file('app/www/trends.md', package = 'covid19')))
            )
            
          )
        )
      )
      
    ),
    waiter::waiter_preloader(html = waiter::spin_3())
    #waiter::waiter_show_on_load(html = waiter::spin_3()),
    #waiter::waiter_hide_on_render("map_1-cumulative")
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'covid19')
  )
 
  tags$head(
    metathis::meta() %>%
      metathis::meta_social(
        title = "COVID-19 Epi Dashboard",
        description = "Developed by Epicentre - MSF",
        url = "https://reports.msf.net/public/covid19/",
        image = "https://reports.msf.net/img/covid19.png",
        image_alt = "covid-19 dashboard screenshot",
        twitter_card_type = "summary_large_image"
      ),
    shiny::includeHTML(system.file('app/www/ga.html', package = 'covid19')),
    golem::activate_js(),
    golem::favicon("www/favicon.png"),
    #tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js", type="text/javascript"),
    tags$link(href = "https://fonts.googleapis.com/css?family=Roboto+Condensed:400,700&display=swap", rel = "stylesheet"),
    waiter::use_waiter(),
    shinyjs::useShinyjs(),
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css"),
    tags$script(src="www/addNavLink.js"),
    shinyWidgets::useShinydashboard()
  )
}
