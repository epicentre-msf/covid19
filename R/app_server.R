#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(mod_map_server, "map_1")
  
  # hide initial loading screen
  # waiter::waiter_hide()
}
