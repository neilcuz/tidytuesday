
library(dplyr)
library(ggplot2)
library(ggbeeswarm)
library(stringr)
library(ggtext)
library(ggrepel)
library(glue)
library(snakecase)
library(readr)
library(here)

snakecase::to_title_case("HELLO MATE")

nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')

min_weeks <- 20

nyt_titles <- nyt_titles |>
  dplyr::filter(total_weeks >= min_weeks) |>
  dplyr::mutate(decade = glue::glue("{str_sub(year, 1, 3)}0s"),
                decade = factor(decade, levels = unique(sort(decade, decreasing = TRUE)))) |>
  dplyr::group_by(decade) |>
  mutate(max_weeks = max(total_weeks)) |>
  dplyr::ungroup() |>
  mutate(chart_label = if_else(max_weeks == total_weeks,
                               glue("{author}\n {snakecase::to_title_case(title)} \n {total_weeks} weeks"), ""),
         chart_label = str_replace(chart_label, "you Ll", "You'll"))

p <- nyt_titles |>
  ggplot(aes(x = decade, y = total_weeks)) +
  ggbeeswarm::geom_beeswarm(cex = 0.75, shape = 21, col = "white", alpha = 0.95, show.legend = FALSE,
                            fill = "#ec9c2c", size = 3) +
  geom_text_repel(data = filter(nyt_titles, chart_label != ""),
                   mapping = aes(x = decade, y = total_weeks, label = chart_label),
                   size = 3,
                   color = "#ee9b2d",
                   lineheight = 0.8,
                   min.segment.length = unit(0.1, "mm"),
                   segment.size = 0.2,
                   segment.color = "grey50",
                   direction = "y",
                   hjust = 0,
                   nudge_y = 12) +
  coord_flip() +
  theme_bw() +
  labs(title = "Staying in the charts",
       subtitle = glue("New York Times bestsellers that stayed in the charts for at least {min_weeks} weeks"),
       y = "Total weeks in the chart",
       x = "") +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 12, colour = "white"),
        axis.title.x = element_text(size = 12, margin = margin(15, 0, 0, 0)),
        axis.title.y = element_text(size = 12, margin = margin(0, 15, 0, 0), colour = "white"),
        plot.title = element_text(hjust = 0.075, size = 16, margin = margin(15, 0, 20, 0), face = "bold", colour = "white"),
        panel.border = element_blank(),
        plot.subtitle = element_text(hjust = 0.19, size = 12, colour = "white"),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black")) +
  scale_y_continuous(breaks = c(20, 60, 100, 140, 180), labels = c(20, 60, 100, 140, 180), limits = c(20, 240))


p

ggsave(glue("{here()}/figures/tidy-tuesday_2022-05-10.png"), width = 7.65, height = 6.375, dpi = 550)



