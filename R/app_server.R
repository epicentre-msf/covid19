#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(mod_map_server, "map_1")
  callModule(mod_phsm_server, "phsm")
  # hide initial loading screen
  # waiter::waiter_hide()
}
