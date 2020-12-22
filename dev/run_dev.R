
# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# library(reactlog)
# tell shiny to log all reactivity
# options(shiny.reactlog = TRUE)

# Run the application
covid19::run_app(lb = FALSE)