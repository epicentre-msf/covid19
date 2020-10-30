library(tidyverse)
library(sf)

font <- "IBM Plex Sans"
theme_set(theme_minimal(base_size = 14, base_family = font))

#drc_adm1 <- geoboundaries("COD", adm_lvl = "adm1") %>% janitor::clean_names()

drc_adm1 <- read_rds("~/MSF/GRP-EPI-COVID-19 - NCoVEpi/data/shapefiles/COD/sf/COD_adm1.rds")

drc_cas <- readxl::read_excel("~/Downloads/cas_par_province_17aout.xlsx") %>% 
  janitor::clean_names() %>% 
  mutate(pcode = hmatch::string_std(province)) %>% 
  mutate(pcode = recode(pcode, "h_katanga" = "haut_katanga"))

breaks <- c(1, 10, 50, 100, 500, 1000, Inf)
brk_labs <- epivis::label_breaks(breaks)

df_sf <- drc_adm1 %>% 
  left_join(drc_cas, by = "pcode") %>% 
  mutate(brks = cut(cumul_de_cas, breaks = breaks, labels = brk_labs, include.lowest = TRUE, right = FALSE))

ggplot(df_sf) +
  geom_sf(aes(fill = brks), colour = "white", size = .2) +
  geom_text(
    data = filter(df_sf, cumul_de_cas < 5000),
    aes(lon, lat, label = cumul_de_cas, hjust = ifelse(cumul_de_cas == 94, 2, 0.5)), 
    fontface = "bold", 
    family = font
  ) + 
  ggrepel::geom_text_repel(
    data = filter(df_sf, cumul_de_cas > 5000), 
    aes(lon, lat, label = cumul_de_cas), 
    fontface = "bold", 
    family = font, 
    vjust = -5, 
    hjust = 1
  ) +
  coord_sf(datum = NA) +
  scale_fill_brewer(
    name = "Nombre des cas", 
    palette = "Reds", 
    na.value = "grey", 
    breaks = c(NA, brk_labs), 
    labels = c("0", brk_labs), 
    drop = FALSE
    #na.translate=FALSE
  ) + 
  scale_colour_identity() +
  theme(legend.position = c(.2, .8)) +
  labs(x = NULL, y = NULL, caption = "Données: MSP | Graphique: Epicentre")

ggsave("~/Downloads/drc_case_map.png", width = 10, height = 10, dpi = 320)
