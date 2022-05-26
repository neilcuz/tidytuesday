
# TidyTuesday - Eurovision

# Setup --------------------------------------------------------------------------------------------

library(glue)
library(here)
library(tidytuesdayR)
library(dplyr)
library(ggplot2)
library(purrr)
library(magrittr)
library(zoo)
library(ggforce)
library(countrycode)
library(grid)
library(png)
library(ggtext)
library(gtable)

raw_data <- tidytuesdayR::tt_load('2022-05-24')

# Wrangle data -----------------------------------------------------------------

fifteens_raw <- raw_data$fifteens

## A game could be played home or away, we arent bothered where the team is
## playing so combine home and away games

fifteens_home <- fifteens_raw |>
  rename(team = team_1, opponent = team_2) |>
  select(date, team, opponent, margin_of_victory)

fifteens_away <- fifteens_raw |>
  mutate(margin_of_victory = -margin_of_victory) |>
  rename(team = team_2, opponent = team_1) |>
  select(date, team, opponent, margin_of_victory)

fifteens <- bind_rows(fifteens_home, fifteens_away) |> arrange(date)

## Filter down the teams and dates we want and final formatting

fifteens <- fifteens |>
  count(team) |>
  arrange(desc(n)) |>

  # Just want the 12 teams with the most matches, a bit of trial and error found
  # 80 matches gave the top 12 teams

  filter(n >= 80) |>
  select(team) |>
  semi_join(fifteens, y = _, by = "team")

# Only start after the team which appears last in the data, of the teams we are
# interest in, appears

start_date <- fifteens |>
  group_split(team) |>
  map(1) |>
  map(1) |>
  map(as.character) |>
  unlist() |>
  as.Date() |>
  sort() |>
  rev() |>
  extract(1)

fifteens <- fifteens |>
  filter(date >= start_date) |>
  group_by(team) |>
  mutate(ma_margin = zoo::rollmean(margin_of_victory, 20, na.pad = TRUE),
         ma_status = case_when(ma_margin > 0 ~ "pos",
                               ma_margin < 0 ~ "neg",
                               ma_margin == 0 ~ "0")) |>
  filter(!is.na(ma_margin))

# Order from strongest to weakest teams

team_levels <- fifteens |>
  mutate(mean_margin = mean(margin_of_victory)) |>
  distinct(team, mean_margin) |>
  arrange(desc(mean_margin)) |>
  pull(team)

fifteens <- fifteens |>
  mutate(team = factor(team, levels = team_levels)) |>
  arrange(team, date) |>
  group_by(team) |>
  mutate(row_num = 1:n()) |>
  ungroup()

# Plot -------------------------------------------------------------------------

## These functions are used for adding in the flags

# create_grob: create a grob for the flags

create_grob <- function (filename, width = 0.125, height = 0.125, x = 0.5,
                         y = 0.945) {

  filename |>
    readPNG() |>
    rasterGrob(width = width, height = height, x = x, y = y)

}

# modify_chart: edit the ggplot chart to add in the grobs

modify_chart <- function(p, grob_list) {

  p_gtable <- ggplot_gtable(ggplot_build(p))
  facets <- grep("panel", p_gtable$layout$name)

  p_gtable <- with(
    p_gtable$layout[facets,],
    gtable_add_grob(p_gtable, grob_list, t = t, l = l, b = b, r = r, name = "x")
    )

  return(p_gtable)
}

chart_colours <- c("pos" = "#006B38FF", "neg" = "#E94B3CFF")


title_html <-  glue(
  "<span style='font-size:16pt;font-family:'Gill Sans;align:left'>New Zealand and
  England are great at rugby</span><br><br>
  <span style='font-size:10pt;line-height:50%;align:left'>
  The chart shows the 20-match rolling winning average for 12 of the best teams in
  women's international<br>rugby.The green lines indicate that a team had a
  <span style='color:{chart_colours[1]};font-size:10pt;line-height:50%'>positive
  score differential</span> over the previous 20 matches<br>while the red lines
  indicate a team had a <span style='color:{chart_colours[2]};font-size:10pt;line-height:50%'>negative
  score differential.</span> New Zealand and England appear to <br> have been the
  best teams over the period with most of their time spent with a positive
  rolling score <br> differential. No adjustment has been made for strength of
  opposition.
  </span>"
  )

p <- fifteens |>
  ggplot(aes(x = date, y = ma_margin, colour = ma_status)) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_link2(aes(colour = after_stat(if_else(y > 0, "pos", "neg")))) +
  scale_colour_manual(values = chart_colours, guide = "none") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.grid.major.y = element_line(colour = "#EADDDD"),
        axis.text.x = element_text(size = 9),
        plot.title = element_markdown(margin = margin(15, 0, 20, 13),
                                      hjust = 0),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text = element_text(size = 10, family = "Gill Sans"),
        plot.caption = element_text(size = 8)) +
  ylim(-30, 30) +
  labs(title = title_html,
       x = "",
       y = "",
       caption = "Source: Neil Currie (Twitter: @neilgcurrie) data from ScrumQueens") +
  facet_wrap(~team)

#  Add in the flags

flag_filenames <- fifteens |>

  # Expect a warning - this is the UK countries not matching

  mutate(country_code = countrycode(team, "country.name", "iso2c"),
         country_code = case_when(team == "Scotland" ~ "gb-sct",
                                  team == "England" ~ "gb-eng",
                                  team == "Wales" ~ "gb-wls",
                                  TRUE ~ country_code)) |>
  distinct(country_code) |>
  unlist() |>
  paste0(".png") |>
  tolower() |>

  # This sorts the order of the flags. We will have 12 facets. The flags are
  # appear to be in the correct order. The facets are:
  # A, B, C, D
  # E, F, G, H
  # I, J, K, L
  # But the method for drawing the flags allocates them down columns so:
  # A, E, I, B, F...etc
  # So will be in the wrong order. By putting in a matrix we fix it.

  matrix(nrow = 3, byrow = TRUE) |>
  as.vector() |>

  # New pipe syntax

  {\(x) glue("{here()}/flags/{x}")}()

final_plot_gtable <- flag_filenames |>
  map(create_grob) |>
  modify_chart(p, grob_list = _)

# Save

png(glue("{here()}/figures/tidy-tuesday_2022-05-26.png"), units = "in",
    width = 7.65, height = 6.375, res = 550 )

grid.draw(final_plot_gtable)

dev.off()
