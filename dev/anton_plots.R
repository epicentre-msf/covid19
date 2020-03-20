# ==========================================================================
# Some random plots
# ==========================================================================

#
# Compare countries
#

lag_FR <- 8
lag_IT <- 0
lag_UK <- 14

df_data_ecdc %>%
  count(country) %>%
  as.data.frame()

df_data_ecdc %>%
  mutate(
    date_rep = case_when(
      country == "France" ~ date_rep - lag_FR,
      country == "Italy" ~ date_rep - lag_IT,
      country == "United Kingdom" ~ date_rep - lag_UK,
      TRUE ~ date_rep
    )
  ) %>%
  filter(country %in% c("France", "Italy", "United Kingdom"), date_rep > as.Date("2020-02-15")) %>%
  ggplot(aes(x = date_rep, y = value, colour = country)) +
  facet_wrap(~time_series, scales = "free_y", ncol = 1) +
  geom_line() +
  geom_point(aes(shape = country)) +
  theme_light() +
  scale_y_log10()

#
# Compare lockdown countries
#

date_lockdown_china <- df_data_intervention %>%
  filter(country == "China", intervention == "Lockdown") %>%
  .$date_start
df_data_inter_plot <- df_data_intervention %>%
  filter(intervention == "Lockdown", !region %in% c("Lombardie")) %>%
  mutate(lag = date_start - date_lockdown_china, date_start_rescaled = date_start - lag)

lag_FR <- df_data_inter_plot %>%
  filter(country == "France") %>%
  .$lag
lag_IT <- df_data_inter_plot %>%
  filter(country == "Italy") %>%
  .$lag

df_data_plot <- df_data_who %>%
  filter(country %in% c("Italy", "France", "China") & is.na(region), value < 10000, date_rep > as.Date("2020-01-01")) %>%
  mutate(
    date_rep = case_when(
      country == "France" ~ date_rep - lag_FR,
      country == "Italy" ~ date_rep - lag_IT,
      country == "China" ~ date_rep,
      TRUE ~ date_rep
    )
  )

p <- ggplot(df_data_plot) + facet_wrap(~time_series, scales = "free_y", ncol = 1)
p <- p + geom_line(data = df_data_plot, aes(x = date_rep, y = value, colour = country))
p <- p + geom_vline(data = df_data_inter_plot, aes(xintercept = date_start_rescaled))
p <- p + theme_light() + xlim(as.Date(c("2020-01-01", NA)))
print(p)


#
# Compare sources
#

df_data_compare <- df_data_ecdc %>% bind_rows(df_data_jhu_country, df_data_spf, df_data_who)

df_data_ecdc %>%
  filter(country == "France") %>%
  View()

## France

df_data_compare %>%
  filter(country == "France", date_rep > as.Date("2020-02-20")) %>%
  ggplot(aes(x = date_rep, y = value, colour = source)) + 
  facet_wrap(~time_series, scales = "free_y", ncol = 1) + 
  geom_line() + theme_light() + 
  scale_colour_brewer(palette = "Dark2")
