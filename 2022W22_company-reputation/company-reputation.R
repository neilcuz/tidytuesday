
library(tidytuesdayR)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggbeeswarm)
library(ggtext)
library(showtext)
library(glue)
library(here)

path <- glue("{here::here()}2022W22_company-reputation/")

raw_data <- tidytuesdayR::tt_load('2022-05-31')

reputation <- raw_data$reputation

top_3 <- c("Retail", "Tech", "Food & Beverage")

# Chart will focus on the top 3 industries so we want to set aesthetics for them
# separately from the rest of the industries.

fill_industry2 <-c("#F0A862", "#90D3A9", "#DE5B4F", '#8e8e8e')
names(fill_industry2) <- c(top_3, "Other")

alpha_industry2 <- c(1, 1, 1, 0.4)
names(alpha_industry2) <- names(fill_industry2)

colour_industry2 <- c("black", "black", "black", '#8e8e8e')
names(colour_industry2) <- names(fill_industry2)

size_industry2 <- c(3, 3, 3, 2)
names(size_industry2) <- names(fill_industry2)

# Used in conjunction with element_markdown below (ggtext package)

subtitle_html <- glue(
  "The top 100 most visible companies were dominated by the
   <span style='color:{fill_industry2[1]}'>Retail</span>,
   <span style='color:{fill_industry2[2]}'>Tech</span>, and, <br>
   <span style='color:{fill_industry2[3]}'>Food & Beverage</span>
   industries. Results were derived using a national survey of<br> Americans with
   companies ranked across seven key measures of reputation. <br>
   The Products* dimension below is Products & Services."
)

# Plot

p <- reputation |>
  mutate(industry2 = if_else(industry %in% top_3, industry, "Other"),
         dimension = if_else(name == "P&S",
                             "Products*",
                             name),
         dimension = str_to_title(dimension),
         dimension = factor(dimension,
                            levels = sort(unique(dimension),
                                          decreasing = TRUE))) |>
  ggplot(aes(x = dimension, y = score, fill = industry2, colour = industry2,
             size = industry2, alpha = industry2)) +
  geom_beeswarm(cex = 1.3, shape = 21,
                show.legend = FALSE) +
  scale_fill_manual(values = fill_industry2) +
  scale_colour_manual(values = colour_industry2) +
  scale_size_manual(values = size_industry2) +
  scale_alpha_manual(values = alpha_industry2) +
  coord_flip() +
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text = element_text(size = 12,  family = "Roboto"),
        axis.title.x = element_text(size = 12,
                                    margin = margin(15, 0, 0, 0),
                                    family = "Roboto"),
        axis.title.y = element_text(size = 12,
                                    margin = margin(0, 15, 0, 0),
                                    family = "Roboto"),
        plot.title = element_text(size = 18,
                                  family = "Roboto",
                                  hjust = 0.04,
                                  margin = margin(15, 0, 10, 0),
                                  face = "bold"),
        panel.border = element_blank(),
        plot.subtitle = element_markdown(size = 11,
                                         family = "Roboto",
                                         margin = margin(0, 0, 10, 20)),
        plot.caption = element_text(margin = margin(12, 0, 5, 0),
                                    size = 8,
                                    hjust = -0.3),
        axis.ticks = element_blank(),
        plot.title.position = "plot") +
  labs(title = "2022 Axios-Harris Poll",
       subtitle = subtitle_html,
       x = "",
       y = "Score",
       caption = "Source: Neil Currie (Twitter: @neilgcurrie) data from the Axios-Harris Poll")

ggsave(glue("{path}chart.png"),
       width = 7.65,
       height = 6.375,
       dpi = 550)
