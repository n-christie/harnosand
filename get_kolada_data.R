library(tidyverse)
library(rKolada)


# Get kpi descriptions  from Kolada

kpi_df <- get_kpi()

kpi_filter <- kpi_df %>% kpi_search("Brukarbedömning hemtjänst äldre", column = c("description", "title"))

# Find kpis


kpis <- c("N21700","N21704","N20891","U21468")

# municipalities

munic_grp_filter <- get_municipality_groups() %>% 
  municipality_grp_search("Liknande kommuner socioekonomi, Härnösand")


# Get data
grp_data <- get_values(
  kpi = kpis,
  municipality = c(
    municipality_grp_extract_ids(munic_grp_filter)
  )
)

# Get KPIs describing Gross Regional Product of municipalities
kpi_filter <- get_kpi() %>% 
  kpi_search(c("N21700","N21704","N20891","U21468")) %>%
  kpi_search("K", column = "municipality_type")

munic_grp_filter <- get_municipality_groups() %>% 
  municipality_grp_search("Liknande kommuner socioekonomi, Härnösand")

Härnösand <- get_municipality() %>% municipality_search("Härnösand")

kom_list <- get_municipality() %>% municipality_search(c("Avesta", "Ludvika", "Ronneby" , "Karlskoga","Härnösand"))
# Get data
grp_data <- get_values(
  kpi = kpi_extract_ids(kpi_filter),
  municipality = c(

    municipality_extract_ids(kom_list)
  )
)


library("ggplot2")

grp_data %>% 
  filter(gender == "K",
         year >= 2015) %>% 

ggplot( aes(year, value, color = municipality)) +
  geom_line(aes(color = municipality), size = 0.8) +
  facet_grid(kpi ~ ., scales = "free") +
  labs(
    title = "Hemtjänst metrics",
    subtitle = "Swedish municipalities similar to Härnösand",
    caption = values_legend(grp_data, kpi_filter)
  ) +
  scale_color_brewer(palette="Dark2") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(n.breaks = 7, limits = c(2015,2022))+
  theme_light()
