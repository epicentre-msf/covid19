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
      
      tabPanel(
        #title = "About",
        title = icon("info"), 
        fluidRow(
          column(
            width = 8, offset = 2,
            includeMarkdown(system.file('app/www/about.md', package = 'covid19'))
          )
        )
      )
      
    ),
    HTML('<div data-iframe-height></div>'),
    waiter::waiter_show_on_load(html = waiter::spin_3())
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
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/4.2.10/iframeResizer.contentWindow.min.js", type="text/javascript"),
    tags$link(href = "https://fonts.googleapis.com/css?family=Roboto+Condensed:400,700&display=swap", rel = "stylesheet"),
    waiter::use_waiter(include_js = FALSE),
    shinyjs::useShinyjs(),
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css"),
    tags$script(src="www/addNavLink.js"),
    shinyWidgets::useShinydashboard()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
