
# TidyTuesday - Eurovision

# Setup --------------------------------------------------------------------------------------------

library(tidytuesdayR)
library(dplyr)
library(ggbump)
library(ggplot2)
library(glue)
library(here)
library(stringr)
library(ggtext)

path <- glue("{here::here()}/2022W20_eurovision/")

raw_data <- tidytuesdayR::tt_load('2022-05-17')

# Wrangle data -------------------------------------------------------------------------------------

eurovision <- raw_data$eurovision

eurovision_points <- raw_data$`eurovision-votes` |>
  filter(semi_final == "f") |>
  rename(vote = jury_or_televoting, country = to_country) |>
  mutate(vote = case_when(vote == "J" ~ "Jury",
                          vote == "T" ~ "Public")) |>
  group_by(year, vote, country) |>
  summarise(points = sum(points)) |>
  ungroup()

# Create a total points version and combine into same tibble

eurovision_points <- eurovision_points |>
  group_by(year, country) |>
  summarise(points = sum(points)) |>
  ungroup() |>
  mutate(vote = "Total") |>
  bind_rows(eurovision_votes)

eurovision_ranks <- eurovision_points |>
  group_by(year, vote) |>
  mutate(points_rank = rank(-points, ties.method = "min"),
         vote = factor(vote, levels = c("Jury", "Public", "Total"))) |>
  ungroup() |>
  filter(country == "Ukraine", !(year < 2016 & vote == "Jury")) |>
  mutate(plot_text = if_else(points_rank == 1 & vote == "Total",
                             glue("Win\n'{str_sub(year, 3, 4)}"),
                             ""))

# Plot ---------------------------------------------------------------------------------------------

plot_colours <- c("Jury" = "#005BBB", "Public" = "#FFD500", "Total" = "#FFFFFF")
background_colour <- '#140018'

subtitle_html <- glue("<span style='font-size:13pt'> Separate
                       <span style='color:{plot_colours[1]};'>jury</span> and
                       <span style='color:{plot_colours[2]};'>public</span>
                       voting began in 2016</span>")

p <- eurovision_ranks |>
  ggplot(aes(year, points, colour = vote, fill = vote)) +
  geom_point(size = 2) +
  geom_bump() +
  geom_text(aes(label = plot_text), nudge_x = 0.7, nudge_y = 10) +
  theme_minimal() +
  labs(title = "Ukraine at Eurovision",
       subtitle = subtitle_html,
       y = "Points") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12, colour = "white"),
        panel.grid.major.y = element_line(colour = "#EADDDD"),
        axis.title.x = element_text(size = 12, margin = margin(15, 0, 0, 0)),
        axis.title.y = element_text(size = 12, margin = margin(0, 15, 0, 0), colour = "white"),
        plot.title = element_text(hjust = 0.06, margin = margin(15, 0, 20, 0), face = "bold",
                                  colour = "white", size = 16),
        panel.border = element_blank(),
        plot.subtitle = element_markdown(hjust = 0.085, colour = "white"),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        legend.position = "none",
        plot.background = element_rect(fill = background_colour),
        panel.background = element_rect(fill = background_colour))  +
  scale_colour_manual(values = plot_colours) +
  scale_fill_manual(values = plot_colours) +
  geom_segment(aes(x = 2020, y = 0, xend = 2020, yend = 620, colour = "#EADDDD"), linetype = 2) +
  annotate("label", x = 2020, y = 650, label = "Cancelled due\nto Covid-19", colour = "white",
           size = 2.5, fill = background_colour, label.size = NA)

p

ggsave(glue("{path}chart.png"), width = 7.65, height = 6.375, dpi = 550)
