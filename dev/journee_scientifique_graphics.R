library(tidyverse)
library(gt)
library(sf)
pkgload::load_all()

# setup =========================================================================================
Sys.setlocale("LC_TIME", "fr_FR.UTF-8")

font <- "IBM Plex Sans" # change this to a font you have on your system 

caption <- "Data source: European Centre for Disease Prevention and Control | Graphic: Epicentre"

ggplot2::theme_set(theme_light(base_size = 21, base_family = font))

export_dir <- "~/Downloads"

frmt_num <- function(x, accuracy = 1) {
  scales::label_number_si(accuracy = accuracy)(x)
}

# data =========================================================================================
df_covid <- df_ecdc %>% 
  filter(date <= epivis::get_prev_sunday(Sys.Date()-1)) %>% 
  filter(country %in% c("Burkina Faso", "Niger", "Mauritania", "Chad", "Mali")) %>% 
  mutate(country = recode(country, "Mauritania" = "Mauritanie", "Chad" = "Tchad"))

df_events <- df_interventions %>% filter(country %in% c("Burkina Faso", "Niger", "Mauritania", "Chad", "Mali"))

sf_sahel <- sf_world %>% 
  filter(country %in% c("Burkina Faso", "Niger", "Mauritania", "Chad", "Mali")) %>% 
  mutate(country = recode(country, "Mauritania" = "Mauritanie", "Chad" = "Tchad"))

# Table ==============================================================================
df_covid %>% 
  group_by(country) %>% 
  summarise(cases = sum(cases), deaths = sum(deaths)) %>% 
  mutate(cfr = deaths / cases) %>% 
  left_join(st_drop_geometry(sf_sahel) %>% select(country, pop_est)) %>% 
  mutate(cas_100k = (cases / pop_est) * 100000) %>% 
  select(-pop_est) %>% 
  gather(key = "key", value = "value", cases:cas_100k) %>% 
  spread(country, value) %>% 
  mutate(key = factor(key, levels = c("cases", "deaths", "cfr", "cas_100k"), labels = c("Cas", "Décès", "Mortalité", "Cas par 100 000"))) %>% 
  arrange(key) %>% 
  gt::gt(rowname_col = "key") %>% 
  fmt_number(columns = 2:6, rows = c(1, 2), sep_mark = " ", decimals = 0) %>% 
  fmt_number(columns = 2:6, rows = c(4), sep_mark = " ", decimals = 1) %>% 
  fmt_percent(columns = 2:6, rows = c(3), decimals = 1) 

# Map ==============================================================================
df_map <- df_covid %>% 
  group_by(country) %>% 
  summarise(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(sf_sahel) %>% 
  sf::st_as_sf()

p_map_cases <- ggplot(df_map) +
  geom_sf(fill = "#BED2C0", alpha = 0.7) +
  geom_point(aes(lon, lat, size = cases), fill = "steelblue", alpha = 0.8, shape = 21, colour = "black") +
  scale_size_area(max_size = 50) +
  geom_sf_label(aes(label = country), nudge_y = 2, fontface = "bold", family = font) +
  geom_text(aes(lon, lat, label = frmt_num(cases, .1)), colour = "white", fontface = "bold", family = font, size = 7) +
  coord_sf(datum = NA) +
  theme_minimal(base_size = 16, base_family = font) +
  labs(x = NULL, y = NULL, size = "Cas") + #, caption = caption
  theme(legend.position = "none", plot.caption = element_text(face = "italic", size = 10)) +
  #theme(legend.position = c(0.55, .9)) +
  guides(
    size = guide_legend(nrow = 1, label.position = "bottom", title.position = "left")
  )

ggsave(fs::path(export_dir, "map_cases", ext = "png"),
       plot = p_map_cases, 
       width = 10, 
       height = 6, 
       dpi = 320)

p_map_deaths <- ggplot(df_map) +
  geom_sf(fill = "#DFA599", alpha = 0.7) +
  geom_point(aes(lon, lat, size = deaths), fill = "steelblue", alpha = 0.8, shape = 21, colour = "black") +
  scale_size_area(max_size = 30) +
  geom_sf_label(aes(label = country), nudge_y = 2, fontface = "bold", family = font) +
  geom_text(aes(lon, lat, label = frmt_num(deaths)), colour = "white", fontface = "bold", family = font, size = 8) +
  coord_sf(datum = NA) +
  theme_minimal(base_size = 16, base_family = font) +
  labs(x = NULL, y = NULL, size = "Décès", title = "") + #caption = caption,
  theme(legend.position = "none", plot.caption = element_text(face = "italic", size = 10), ) +
  #theme(legend.position = c(0.55, .9)) +
  guides(
    size = guide_legend(nrow = 1, label.position = "bottom", title.position = "left")
  )

ggsave(fs::path(export_dir, "map_deaths", ext = "png"),
       plot = p_map_deaths, 
       width = 10, 
       height = 6, 
       dpi = 320)

# Epicurves ==============================================================================

df_weekly <- df_covid %>% 
  mutate(week_date = lubridate::floor_date(date, "week", week_start = 1)) %>% 
  group_by(week_date, country) %>% 
  summarise(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE)) %>% 
  ungroup()

df_totals <- df_covid %>% 
  summarise(cases = sum(cases, na.rm = TRUE), deaths = sum(deaths, na.rm = TRUE))

date_weeks <- epivis::floor_week(df_covid$date)
date_breaks <- seq.Date(range(df_covid$date)[1], range(date_weeks)[2], length.out = 5)
  
p_weekly_cases <- ggplot(df_weekly, aes(week_date, cases, group = country)) +
  geom_col(aes(fill = country), colour = "white", size = 0.3) +
  annotate(
    "text", 
    x = min(df_weekly$week_date) + 32, 
    y = 1175, 
    label = glue::glue("Cas Total: {scales::number(df_totals$cases)}"),
    fontface = "bold", family = font, size = 7
  ) +
  ggthemes::scale_fill_tableau(name = NULL) +
  scale_x_date("Semaine", date_breaks = "2 weeks", date_labels = "%V", 
               sec.axis = sec_axis(trans = ~ ., breaks = date_breaks, labels = scales::date_format("%d-%b")), 
               expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous("Cas", labels = scales::number_format(accuracy = 1), expand = expansion(mult = c(0, 0.02))) +
  #labs(title = "Cas") +
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    strip.text = element_text(colour = "black", face = "bold"),
    plot.caption = element_text(face = "italic", size = 10), 
    legend.position = "top",
    axis.text = element_text(size = 18, face = "bold", family = font),
    legend.text = element_text(size = 25, face = "bold", family = font)
  ) 

p_weekly_deaths <- ggplot(df_weekly, aes(week_date, deaths, group = country)) +
  geom_col(aes(fill = country), colour = "white", size = 0.3) +
  annotate(
    "text", 
    x = min(df_weekly$week_date) + 32, 
    y = 53, 
    label = glue::glue("Décès Total: {scales::number(df_totals$deaths)}"),
    fontface = "bold", family = font, size = 7
  ) +
  ggthemes::scale_fill_tableau(name = NULL) +
  scale_x_date("Semaine", date_breaks = "2 weeks", date_labels = "%V", 
               sec.axis = sec_axis(trans = ~ ., breaks = date_breaks,  labels = scales::date_format("%d-%b")), 
               expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous("Décès", labels = scales::number_format(accuracy = 1), expand = expansion(mult = c(0, 0.05))) +
  #labs(title = "Décès") +
  theme(
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(), 
    strip.text = element_text(colour = "black", face = "bold"),
    plot.caption = element_text(face = "italic", size = 10), 
    legend.position = "top",
    axis.text = element_text(size = 18, face = "bold", family = font),
    legend.text = element_text(size = 25, face = "bold", family = font)
  )

library(patchwork)

combined_curves <- p_weekly_cases + p_weekly_deaths + plot_layout(guides = 'collect', ncol = 2)

combined_all <- p_weekly_cases + 
  p_weekly_deaths + 
  p_map_cases + 
  p_map_deaths + 
  plot_layout(guides = 'collect', ncol = 2, heights = c(.4, .6)) +
  plot_annotation(theme = theme(legend.position = "top", legend.direction = "horizontal"))

ggsave(fs::path(export_dir, "epicurves", ext = "svg"),
       plot = combined_curves, 
       width = 18, 
       height = 5, 
       scale = 1.1,
       dpi = 320)

ggsave(fs::path(export_dir, "weekly_cases", ext = "png"),
       plot = p_weekly_cases, 
       width = 10, 
       height = 6, 
       scale = 1.1,
       dpi = 320)

ggsave(fs::path(export_dir, "weekly_deaths", ext = "png"),
       plot = p_weekly_deaths, 
       width = 10, 
       height = 6, 
       scale = 1.1,
       dpi = 320)

# Cumulative  ==============================================================================

df_cumulative <- df_covid %>% 
  dplyr::group_by(country) %>% 
  dplyr::arrange(date) %>% 
  dplyr::mutate_at(dplyr::vars(cases, deaths), cumsum) %>% 
  dplyr::ungroup()

df_cumulative_cases <- df_cumulative %>% 
  select(country, cases) %>% 
  dplyr::group_by(country) %>% 
  tidyr::drop_na(cases) %>% 
  dplyr::filter(cases >= 100) %>% 
  dplyr::mutate(days_100_cases = seq_along(cases)-1) %>% 
  mutate(label = if_else(days_100_cases == max(days_100_cases), country, NA_character_)) %>% 
  dplyr::ungroup()

p_cumulative_cases <- ggplot(df_cumulative_cases, aes(days_100_cases, cases, group = country, colour = country)) +
  geom_line(aes(colour = country), size = .8, show.legend = FALSE) +
  ggthemes::scale_colour_tableau() + 
  scale_y_continuous("Cas (log scale)", trans = "log10") +
  ggrepel::geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE) +
  theme(legend.position = "none", plot.caption = element_text(face = "italic", size = 10)) +
  labs(x = "Jours depuis les 100 premiers cas", caption = caption)

ggsave(fs::path(export_dir, "cumulative_cases", ext = "png"),
       plot = p_cumulative_cases, 
       width = 10, 
       height = 6, 
       scale = 1.1,
       dpi = 320)

df_cumulative_deaths <- df_cumulative %>% 
  select(country, deaths) %>% 
  dplyr::group_by(country) %>% 
  tidyr::drop_na(deaths) %>% 
  dplyr::filter(deaths >= 10) %>% 
  dplyr::mutate(days_10_death = seq_along(deaths)-1) %>% 
  mutate(label = if_else(days_10_death == max(days_10_death), country, NA_character_)) %>% 
  dplyr::ungroup()

p_cumulative_deaths <- ggplot(df_cumulative_deaths, aes(days_10_death, deaths, group = country, colour = country)) +
  geom_line(aes(colour = country), size = .8, show.legend = FALSE) +
  ggthemes::scale_colour_tableau() + 
  scale_y_continuous("Décès (log scale)", trans = "log10") +
  ggrepel::geom_label_repel(aes(label = label), nudge_x = 1, na.rm = TRUE, ) +
  theme(legend.position = "none", plot.caption = element_text(face = "italic", size = 10)) +
  labs(x = "Jours depuis les 10 premiers décès", caption = caption)

ggsave(fs::path(export_dir, "cumulative_deaths", ext = "png"),
       plot = p_cumulative_deaths, 
       width = 10, 
       height = 6, 
       scale = 1.1,
       dpi = 320)

# Experimental cumulative cases chart with events  ===========================================

df_measures <- df_events %>% 
  select(date = date_implemented, country, category, measure) %>% 
  left_join(select(df_cumulative, date, country, cases)) %>% 
  replace_na(list(cases = 0)) 

ggplot(df_cumulative, aes(date, cases, group = country)) + #
  geom_line(aes(colour = country)) +
  geom_point(data = df_measures, aes(date, cases, shape = category))
   #scale_y_continuous("Cas (log scale)", trans = "log10")
  #ggrepel::geom_text_repel(data = df_measures %>% filter(country == "Mali"), aes(label = measure))


# facet plots - not being used ================================================

ggplot(df_covid, aes(week_date, cases)) +
  facet_wrap(~country, scales = "free_y", ncol = 1, strip.position = "right") +
  geom_col(aes(fill = country), show.legend = FALSE) +
  ggthemes::scale_fill_tableau() +
  scale_x_date("Semaine", date_breaks = "2 weeks", date_labels = "%V", sec.axis = sec_axis(trans = ~ .)) +
  scale_y_continuous("Deaths", labels = scales::number_format(accuracy = 1)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), strip.text = element_text(colour = "black", face = "bold")) +
  labs(caption = "Note free y scales")

ggplot(df_covid, aes(week_date, deaths)) +
  facet_wrap(~country, scales = "free_y", ncol = 1, strip.position = "right") +
  geom_col(aes(fill = country), show.legend = FALSE) +
  ggthemes::scale_fill_tableau() +
  scale_x_date("Semaine", date_breaks = "2 weeks", date_labels = "%V", sec.axis = sec_axis(trans = ~ .)) +
  scale_y_continuous("Deaths", labels = scales::number_format(accuracy = 1)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text = element_text(colour = "black", face = "bold")) +
  labs(caption = "Note free y scales")
