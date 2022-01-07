###########
# Create a chart that looks like a game of Tetris
# using 80s and 90s game sales data
###########

library(ggplot2)
library(dplyr)
library(tidyr)
library(extrafont)
library(gganimate)

# load data
games <- read.csv("~/TetrisChart/Games.csv", stringsAsFactors=FALSE)
turns <- read.csv("~/TetrisChart/Turns.csv")
turns_sep <- read.csv("~/TetrisChart/Turns_separated.csv")
data <- data_full %>% filter(Units.sold >= 10) %>% mutate(Units.sold = Units.sold / 10)

# tidy games data
games <- games %>% arrange(Initial.release.date)
games$Platform <- gsub("Mega Drive/Genesis", "Mega Drive", games$Platform)
games$count <- 1

# counting the rows of data to match against the "turns" pattern
n <- ceiling(sum(games$count) / 30)

# prepare turns file for the dataset
turns_rep <- turns_sep %>% filter(Turn <= n * 30)
turns_long_rep <- turns_rep %>% gather(column, blocks, col_1:col_10, factor_key=TRUE) %>% arrange(Turn)

# need to account for the extra loops on turns with additional rows

tetris_data <- function(data) {
  
  number_df <- data.frame(Block = as.numeric(),
                          Turn = as.numeric(),
                          Platform = as.character(),
                          Firm = as.character()
  )
  
  block <- 0
  turn <- 0
  
  for (i in 1:nrow(data)) {
    
    for (j in 1:(round(data$count[i]) * (sum(unique(turns_long_rep$Block) > 0) / sum(unique(turns_long_rep$Turn) > 0))) ) {
    turn <- turn + 1
      for (k in 1:sum(turns_sep$Turn == turn)) {
      block <- block + 1
      
      c_turn <- data.frame(Block = c(block),
                         Turn = c(turn),
                           Platform = c(data$Platform[turn]),
                           Firm = c(data$Firm[turn]),
                           Release = c(data$Initial.release.date[turn]),
                           Genre = c(data$Genre[turn]))
      
      number_df <- rbind(number_df, c_turn)
      }
    }
  }
  
  return(number_df)

  }

# format data for plot
tdf <- tetris_data(games)

test <- merge(turns_long_rep, tdf, by = c("Turn", "Block"), all.x = TRUE)
test <- test %>% arrange(column) %>% arrange(Block)

test %>% group_by(column) %>% summarise(count = sum(Turn > 0))
test$Platform <- toupper(test$Platform)
test$Firm <- toupper(test$Firm)

# manual colour palette
tetris_pal <- c("#6bd12c", "#cc4c2c", "#8f1b75", "#1a78da", "#ccae2c", "#1ad169", "#2b188f", "#cc203e", "#1ad1c2", "#afd12c", "#38d12c", "#1a3dda", "#cc312c", "#cb20da", "#daa71e", "#1ad1ac", "#cc7e2c")

# create charts
manufacturer <- ggplot(test, aes(fill = as.factor(Turn), y=-blocks, x=as.factor(column))) + 
  geom_col(aes(fill = Firm, group = Block), 
           position="stack", 
           color = "#394d6e", 
           width = 1, 
           size = 1) +
  scale_fill_manual(values = tetris_pal,na.value="#141822") +
  ggtitle("BY PLATFORM MANUFACTURER", subtitle = "Each Tetris block (4 squares) represents one title with over 5 million units sold.") +
  guides(group = "none", color = "none", fill=guide_legend(title="MANUFACTURER", ncol=2, byrow=TRUE)) +
  theme(panel.spacing = unit(3, "cm"),
        legend.position = "right",
        plot.margin = margin(t = 0, r = 40, b = 0, l = 40, unit = "pt"),
        panel.border = element_rect(linetype = "solid", fill = NA, color = NA, size = 3),
        plot.title = element_text(family = "Press Start K", color = "#bcc2f4", hjust = 0.5, size = 10),
        plot.title.position = "panel",
        plot.subtitle = element_text(size = 5, color = "#bcc2f4", hjust = 0.5),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "#141822"),
        panel.background = element_rect(fill = "#141822"),
        legend.key = element_rect(fill = "#141822"),
        legend.background = element_rect(fill = "#141822"),
        legend.text = element_text(color = "#bcc2f4", family = "Press Start", size = 5),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

platform <- ggplot(test, aes(fill = as.factor(Turn), y=-blocks, x=as.factor(column))) + 
  geom_col(aes(fill = Platform, group = Block), 
           position="stack", 
           color = "#394d6e", 
           width = 1, 
           size = 0.4) +
  scale_fill_manual(values = tetris_pal,na.value="#141822") +
  ggtitle("BEST-SELLING\nHOME VIDEO GAMES\nOF THE 80'S & 90'S", subtitle = "Each Tetris block (4 squares) represents one title with over 5 million units sold.") +
  guides(group = "none", color = "none", fill=guide_legend(title="BY PLATFORM", ncol=1, byrow=TRUE)) +
  theme(panel.spacing = unit(3, "cm"),
        legend.position = "right",
        plot.margin = margin(t = 0, r = 40, b = 0, l = 40, unit = "pt"),
        panel.border = element_rect(linetype = "solid", fill = NA, color = NA, size = 3),
        plot.title = element_text(family = "Press Start K", color = "#bcc2f4", hjust = 0.5, size = 10, margin = margin(t = 20, unit = "pt")),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 5, color = "#bcc2f4", hjust = 0.5),
        legend.title = element_text(family = "Press Start", color = "#bcc2f4", size = 10),
        plot.background = element_rect(fill = "#141822"),
        panel.background = element_rect(fill = "#141822"),
        legend.key = element_rect(fill = "#141822"),
        legend.background = element_rect(fill = "#141822"),
        legend.text = element_text(color = "#bcc2f4", family = "Press Start", size = 5),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

release <- ggplot(test, aes(fill = as.factor(Turn), y=-blocks, x=as.factor(column))) + 
  geom_col(aes(fill = as.factor(Release), group = Block), 
           position="stack", 
           color = "#394d6e", 
           width = 1, 
           size = 1) +
  scale_fill_manual(values = tetris_pal,na.value="#141822") +
  ggtitle("BEST-SELLING\nHOME VIDEO GAMES\nOF THE 80'S & 90'S\nBY RELEASE YEAR", subtitle = "Each Tetris block (4 squares) represents one title with over 5 million units sold.") +
  guides(group = "none", color = "none", fill=guide_legend(title="PLATFORM", ncol=3, byrow=TRUE)) +
  theme(panel.spacing = unit(3, "cm"),
        legend.position = "right",
        plot.margin = margin(t = 0, r = 40, b = 0, l = 40, unit = "pt"),
        panel.border = element_rect(linetype = "solid", fill = NA, color = NA, size = 3),
        plot.title = element_text(family = "Press Start K", color = "#bcc2f4", hjust = 0.5, size = 10, margin = margin(t = 20, unit = "pt")),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 5, color = "#bcc2f4", hjust = 0.5),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "#141822"),
        panel.background = element_rect(fill = "#141822"),
        legend.key = element_rect(fill = "#141822"),
        legend.background = element_rect(fill = "#141822"),
        legend.text = element_text(color = "#bcc2f4", family = "Press Start", size = 5),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

# view plots
release
platform
manufacturer

# animated chart 

# create full dataset of NAs 
test_blank <- test %>% mutate(Platform = NA, Firm = NA, Release = NA, Genre = NA)

# create first turn
test_cpy <- merge(test_blank[,1:4], test %>% filter(Turn <= 1), by = c("Turn", "Block", "column", "blocks"), all.x = TRUE)
test_cpy$loop <- 1

# loop through the rest of the turns
for (i in 2:max(test_blank$Turn)) {
  
  test_cpy_x <- merge(test_blank[,1:4], test %>% filter(Turn <= i), by = c("Turn", "Block", "column", "blocks"), all.x = TRUE)
  test_cpy_x$loop <- i
  test_cpy <- rbind(test_cpy, test_cpy_x)
}

p <- ggplot(test_cpy, aes(fill = as.factor(Turn), y=-blocks, x=as.factor(column))) + 
  geom_col(aes(fill = as.factor(Release), group = Block), 
           position="stack", 
           color = "#394d6e", 
           width = 1, 
           size = 1) +
  scale_fill_manual(values = tetris_pal,na.value="#141822") +
  ggtitle("BEST-SELLING\nHOME VIDEO GAMES\nOF THE 80'S & 90'S\nBY RELEASE YEAR", subtitle = "Each Tetris block (4 squares) represents one title with over 5 million units sold.") +
  guides(group = "none", color = "none", fill=guide_legend(title="PLATFORM", ncol=3, byrow=TRUE)) +
  transition_states(loop, wrap = TRUE) +
  labs(caption = "DATA: WIKIPEDIA", color = "#bcc2f4") +
  enter_fly(y_loc = 0) +
  theme(panel.spacing = unit(3, "cm"),
        legend.position = "right",
        plot.margin = margin(t = 0, r = 200, b = 0, l = 200, unit = "pt"),
        panel.border = element_rect(linetype = "solid", fill = NA, color = NA, size = 3),
        plot.title = element_text(family = "Press Start K", color = "#bcc2f4", hjust = 0.5, size = 30, margin = margin(t = 20, b = 10, unit = "pt")),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = 16, color = "#bcc2f4", hjust = 0.5),
        plot.caption = element_text(size = 8, color = "#bcc2f4"),
        legend.title = element_blank(),
        plot.background = element_rect(fill = "#141822"),
        panel.background = element_rect(fill = "#141822"),
        legend.key = element_rect(fill = "#141822"),
        legend.background = element_rect(fill = "#141822"),
        legend.text = element_text(color = "#bcc2f4", family = "Press Start", size = 15),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

animate(p, height = 1000, width = 1000, end_pause = 3, detail = 1)
