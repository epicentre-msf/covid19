#' @import shiny
app_ui <- function() {
  # load data files
  data_dir <- golem::get_golem_options("data_dir")
  df_interventions <- readxl::read_excel(
    fs::path(data_dir, "interventions", "interventions.xlsx"),
    sheet = "Database"
  ) %>% 
    dplyr::mutate_if(lubridate::is.POSIXct, lubridate::as_date)
  
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage(
      title = tagList("NCovid19 Epi Dashboard"),
      windowTitle = "NCovid19 Epi Dashboard",
      position = "fixed-top",
      collapsible = TRUE,
      
      # tabs 
      tabPanel("Map", mod_map_ui("map_1", df_interventions = df_interventions))
      #tabPanel("Epicurves", mod_facet_ui("facet_1", surveillance_df = surveillance_df)),
      
      # footer = tagList(
      #   tags$div(
      #     class = "text-right", style = "padding-top: 20px; padding-right: 20px; color: grey;",
      #     tags$p(
      #       "Source: ",
      #       tags$a(glue::glue("TLOH {latest_week}"), href = "https://data.humdata.org/dataset/0f9ae779-2095-4e17-a63f-26f1966369cb", target = "_blank"),
      #       " | Visualisation: ", 
      #       tags$a("Epicentre MSF", href = "https://epicentre.msf.org/", target = "_blank")
      #     )
      #   )
      # )
    ),
    waiter::waiter_show_on_load(html = waiter::spin_3())
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  addResourcePath(
    'www', system.file('app/www', package = 'covid19')
  )
 
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    waiter::use_waiter(include_js = FALSE),
    tags$link(rel="stylesheet", type="text/css", href="www/styles.css")
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
