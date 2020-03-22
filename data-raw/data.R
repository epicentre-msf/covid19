pkgload::load_all()

NCoVUtils::reset_cache(refresh_data = FALSE)

df_interventions <- get_interventions_data()

df_ecdc <- get_ecdc_data()

df_who <- get_who_data()

usethis::use_data(df_interventions, df_ecdc, df_who, overwrite = TRUE)
