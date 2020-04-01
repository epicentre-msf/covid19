# Code to import relavant shapefiles

library(sf)

# Syria ------------------------------------------------------------------

# https://data.humdata.org/dataset/syrian-arab-republic-administrative-boundaries-populated-places

syria_adm2 <- st_read(here("data-raw/shapefiles/syria/syr_admin_shp_utf8_19321/syr_admin2.shp"))
syria_adm3 <- st_read(here("data-raw/shapefiles/syria/syr_admin_shp_utf8_19321/syr_admin3.shp"))
# point data
syria_adm4 <- st_read(here("data-raw/shapefiles/syria/syr_admin_shp_utf8_19321/syr_pplp_adm4_unocha.shp"))


# France ------------------------------------------------------------------

# ADMIN-EXPRESS édition Janvier 2020 par territoire : ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/ADMIN-EXPRESS_2-2__SHP__FRA_L93_2020-01-16.7z.001


france_adm1 <- sf::st_read(dsn = file.path("data-raw", "shapefiles", "france", "ADMIN-EXPRESS_2-2__SHP__FRA_2020-02-24", "ADMIN-EXPRESS", "1_DONNEES_LIVRAISON_2020-02-24", "ADE_2-2_SHP_WGS84G_FRA", "REGION.shp"))

# Note: department levels are also in the folder if needed


# Italy -------------------------------------------------------------------

# https://github.com/sramazzina/italian-maps-shapefiles
italy_provinces <- sf::st_read(file.path("data-raw", "shapefiles", "italy", "geojson-italy-master", "geojson", "limits_IT_provinces.geojson"))

italy_regions <- sf::st_read(file.path("data-raw", "shapefiles", "italy", "geojson-italy-master", "geojson", "limits_IT_regions.geojson"))



# Iran --------------------------------------------------------------------

# https://data.humdata.org/dataset/administrative-boundaries-1-2
iran_adm0 <- st_read(dsn = "./data-raw/shapefiles/iran/irn_adm_unhcr_20190514_shp/irn_admbnda_adm0_unhcr_20190514.shp")

iran_adm1 <- st_read(dsn = "./data-raw/shapefiles/iran/irn_adm_unhcr_20190514_shp/irn_admbnda_adm1_unhcr_20190514.shp")

# Note : the next lower level is also there (irn_admbnda_adm2_unhcr_20190514.shp) 


# Afghanistan -------------------------------------------------------------

# https://www.diva-gis.org/gdata
# If does not work well, we can try this link as well : https://data.humdata.org/dataset/afg-admin-boundaries

afghanistan_adm0 <- st_read(dsn = "./data-raw/shapefiles/afghanistan/AFG_adm/AFG_adm0.shp")

afghanistan_adm1 <- st_read(dsn = "./data-raw/shapefiles/afghanistan/AFG_adm/AFG_adm1.shp")

# The lower level is also in the file
