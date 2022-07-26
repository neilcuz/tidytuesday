
# Code Tidy Tuesday - 19 April 2022

library(tidytuesdayR)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(glue)
library(tidyr)
library(ggtext)
library(here)

path <- glue("{here::here()}/2022W16_crossword-puzzles/")

raw_data <- tidytuesdayR::tt_load('2022-04-19')

big_dave <- raw_data$big_dave
times <- raw_data$times

big_dave_monthly <- big_dave %>%
  mutate(answer_length = nchar(answer),
         clue = str_replace(clue, " \\(.\\)", ""),
         clue_length = nchar(clue),
         .year = year(puzzle_date),
         .month = month(puzzle_date)) %>%
  group_by(.year, .month) %>%
  summarise(big_dave_clue_length = mean(clue_length, .na.rm = TRUE), .groups = "drop") %>%
  mutate(first_of_the_month = ymd(glue("{.year}/{.month}/1"))) %>%
  select(first_of_the_month, big_dave_clue_length)

times_monthly <- times %>%
  mutate(answer_length = nchar(answer),
         clue = str_replace(clue, " \\(.\\)", ""),
         clue_length = nchar(clue),
         .year = year(puzzle_date),
         .month = month(puzzle_date)) %>%
  group_by(.year, .month) %>%
  summarise(times_clue_length = mean(clue_length, na.rm = TRUE), .groups = "drop") %>%
  mutate(first_of_the_month = ymd(glue("{.year}/{.month}/1"))) %>%
  select(first_of_the_month, times_clue_length)

combined_monthly <- full_join(big_dave_monthly, times_monthly, by = "first_of_the_month") %>%
  filter(!is.na(first_of_the_month))

chart_colours <- c("big_dave" = "#ff6f61", "times" = "#92a8d1")

# You can use the ggtext package to colour titles. Add element_markdown into themes for that particular item.

subtitle_html <- glue(
    "<span style='font-size:12pt'> Average number of characters in clues by month, Jan 2016 to Dec 2019, for
    <span style='color:{chart_colours[1]};'>Big Dave</span> and the
    <span style='color:{chart_colours[2]};'>Times </span>
    </span>")

p <- combined_monthly %>%
  pivot_longer(big_dave_clue_length:times_clue_length, names_to = "crossword", values_to = "clue_length") %>%
  mutate(crossword = str_replace(crossword, "_clue_length", "")) %>%
  filter(first_of_the_month >= ymd("2015-01-01"),
         first_of_the_month <= ymd("2019-12-30")) %>%
  ggplot(aes(x = first_of_the_month, y = clue_length, colour = crossword)) +
  scale_color_manual(values = chart_colours) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(y = "Average number of characters", x = "", title = "Tidy Tuesday Crosswords", subtitle = subtitle_html) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "#EADDDD"),
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 12, margin = margin(15, 0, 0, 0)),
        axis.title.y = element_text(size = 12, margin = margin(0, 15, 0, 0)),
        plot.title = element_text(size = 16, margin = margin(15, 0, 0, 0), face = "bold"),
        plot.subtitle = element_markdown(margin = margin(10, 0, 5, 0)),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        legend.position = "none")

p

ggsave(glue("{path}chart.png"), width = 7.65, height = 6.375, dpi = 550)
