
# Setup and grab Tidy Tuesday data ---------------------------------------------

library(tidytuesdayR)
library(janitor)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ggtext)
library(glue)
library(here)
library(geofacet)

path <- glue("{here::here()}2022W24_us-drought/")

raw_data <- tt_load('2022-06-14')

drought <- raw_data$drought
drought_fips <- raw_data$`drought-fips`

# Prepare data -----------------------------------------------------------------

plot_data <- drought |>
  clean_names() |>
  mutate(date = str_replace(date, "d_", ""),
         date = ymd(date),
         decade = year(date) - year(date) %% 10,
         decade = paste0(decade, "s"),
         decade = factor(decade, levels = rev(unique(decade))),
         state = str_replace(state, "-", " "),
         state = str_to_title(state))  |>
  filter(decade != "1890s") |>
  group_by(decade, state) |>
  summarise(d = mean(d2), w = mean(w2)) |>
  ungroup() |>
  select(decade, state, d, w) |>
  pivot_longer(cols = d:w) |>
  mutate(value = if_else(name == "w", -1 * value, value),
         decade_lab = if_else(decade %in% c("1900s", "1960s", "2020s"),
                              as.character(decade),
                              ""),
         state_code = state.abb[match(state,state.name)])

# Prepare plot -----------------------------------------------------------------

# Sort out labels - only want to plot a few decades of labels or plot becomes
# crowded

decade_labels <- plot_data$decade_lab
names(decade_labels) <- plot_data$decade

# Custom colours for dry and wet

chart_colours <- c("d" = "#DB5425", "w" = "#545D75")

# Use html in the plot subtitle using ggtext package and element_markdown
# function in pipe below

subtitle_html <- glue(
  "The chart shows the proportion of each state experiencing severe or worse
   <span style='color:{chart_colours[1]};font-size:11.5pt'>Dry</span> and
   <span style='color:{chart_colours[2]};font-size:11.5pt'>Wet</span> conditions. <br>
   Aggregated by decade, 1900s - 2020s."
)

# Define font for plot - you will need to install this font if you dont have it

font <- "Roboto"

# Prepare grid to plot in shape of US

state_grid <- filter(us_state_grid1, !code %in% c("DC","HI","AK"))


# Plot -------------------------------------------------------------------------

plot_data |>
  ggplot(aes(x = decade, y = value, fill = name)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme_bw() +
  scale_fill_manual(values = chart_colours) +
  theme(
    text = element_text(family = font),
    panel.border = element_blank(),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    #panel.grid.major.y = element_line(colour = "#EADDDD"),
    axis.text.x = element_text(size = 7),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 12, margin = margin(15, 0, 0, 0)),
    axis.title.y = element_text(size = 12, margin = margin(0, 15, 0, 0)),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 18,
                              margin = margin(15, 0, 10, 20),
                              face = "bold",
                              hjust = 0.05),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(size = 11, margin = margin(0, 0, 10, 20)),
    plot.caption = element_markdown(margin = margin(0, 0, 0, 5), size = 9),
    strip.background = element_rect(colour = "white", fill = "white"),
    strip.text = element_text(size = 7)
    ) +
  scale_x_discrete(labels = decade_labels) +
  labs(
    x = "",
    y = "",
    title = "Drought conditions in the USA",
    subtitle = subtitle_html,
    caption = "Source: Neil Currie @neilgcurrie, data from the US Drought Monitor"
    ) +
  facet_geo(~state_code, grid = state_grid, label = "name")

ggsave(glue("{path}chart.png"),
       width = 9,
       height = 6.375,
       dpi = 550)
