###########
# Example Tetris chart
###########

library(dplyr)
library(ggplot2)
library(gameR) #dev version for "blocks" palette: remotes::install_github("nathansam/gameR")
library(showtext)

# import font from Google
font_add_google("Press Start 2P", "Press Start 2P")
showtext_auto()

# load
source("~/tetris_plot/scripts/tetris_plot.R")

# load data
games <- read.csv("~/tetris_plot/data/Games_img.csv", stringsAsFactors=FALSE)

# tidy games data
games <- games %>% 
  arrange(Initial.release.date) %>%
  rename(Manufacturer = Firm)

tetris_plot(games, "Manufacturer", bgcolor = "#0f0f0f", blockcolor = "#3b3b3b") +
  ggtitle("BEST-SELLING\nHOME VIDEO GAMES\nOF THE 80'S & 90'S\nBY RELEASE YEAR", subtitle = "Each Tetris block (4 squares) represents one\ntitle with over 5 million units sold.") +
  scale_fill_manual(values = gameR_cols("blocks"), na.value = "#0f0f0f") +
  theme(text = element_text(family = "Press Start 2P", color = "white"),
        plot.title = element_text(hjust= 0.5, margin = margin(t = 10, b = 10, unit = "pt")),
        plot.subtitle = element_text(hjust = 0.5, size = 6),
        plot.title.position = "plot")
