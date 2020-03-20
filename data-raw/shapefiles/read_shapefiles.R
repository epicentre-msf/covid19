# Code to import relavant shapefiles

library(sf)


# France ------------------------------------------------------------------

france_regions <- st_read(dsn = file.path("data-raw", "shapefiles", "france", "ADMIN-EXPRESS_2-2__SHP__FRA_2020-02-24", "ADMIN-EXPRESS", "1_DONNEES_LIVRAISON_2020-02-24", "ADE_2-2_SHP_WGS84G_FRA", "REGION.shp"))

