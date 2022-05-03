
# Code Tidy Tuesday - 5 May 2022

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(ggtext)
library(glue)
library(here)

raw_data <- tidytuesdayR::tt_load('2022-05-03')

wind <- raw_data$wind
solar <- raw_data$solar

power_combined <- solar %>%
  full_join(wind, by = "date") %>%
  pivot_longer(cols = c(solar_mwh:wind_capacity)) %>%
  mutate(energy_type = case_when(str_detect(name, "wind") ~ "wind",
                                 TRUE ~ "solar"),
         metric = case_when(str_detect(name, "mwh") ~ "mwh",
                            TRUE ~ "capacity")) %>%
  select(-name) %>%
  pivot_wider(names_from = "metric") %>%
  unnest(cols = c(mwh, capacity))

chart_colours <- c("solar" = "#ff6f61", "wind" = "#92a8d1")

title_html <- glue(
  "<span style='font-size:16pt'> The price of
    <span style='color:{chart_colours[1]};'>solar</span> and
    <span style='color:{chart_colours[2]};'>wind</span> power has fallen from
    2009 to 2021 </span>")

p <- power_combined %>%
  ggplot(aes(x = date, y = mwh, colour = energy_type, size = capacity,
             fill = energy_type)) +
  geom_point(colour =  "#3A3B3C", shape = 21, stroke = 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "#EADDDD"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12, margin = margin(15, 0, 0, 0)),
        axis.title.y = element_text(size = 12, margin = margin(0, 15, 0, 0)),
        plot.title = element_markdown(margin = margin(15, 0, 20, 0),
                                      face = "bold"),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        legend.position = c(0.678, 1),
        legend.direction = "horizontal") +
  scale_fill_manual(values = chart_colours, guide = "none") +
  scale_size(name = "Capacity (gigawatts)", breaks = c(100, 500)) +
  labs(y = "$ per megawatt hour", x = "", title = title_html)

p

ggsave(glue("{here()}/figures/tidy-tuesday_2022-05-03.png"), width = 7.65,
       height = 6.375, dpi = 550)
