# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

data_dir <- here::here("data-raw")

# Run the application
covid19::run_app("data_dir" = data_dir, lb = TRUE)
