#' @import shiny
app_server <- function(input, output,session) {
  # load data files
  data_dir <- golem::get_golem_options("data_dir")
  df_interventions <- readxl::read_excel(fs::path(data_dir, "interventions", "interventions.xlsx"),
                                         sheet = "Database") %>% 
    dplyr::mutate_if(lubridate::is.POSIXct, lubridate::as_date)
  
  # List the first level callModules here
  callModule(mod_map_server, "map_1", df_interventions = df_interventions)
  
  # hide initial loading screen
  waiter::waiter_hide()
}
