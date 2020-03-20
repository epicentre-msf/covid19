#' Run the Shiny Application
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(..., lb = FALSE) {
  with_golem_options(
    app = shinyApp(ui = app_ui, server = app_server, options = list("launch.browser" = lb)), 
    golem_opts = list(...)
  )
}
