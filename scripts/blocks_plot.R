#########
# Generalised code for blocks_plot
#########

library(ggplot2)
library(dplyr)
library(tidyr)

blocks_turns <- function(data) {
  data$blocks_chart_count <- 1
  n <- ceiling(sum(data$blocks_chart_count) / 30)
  
  turns_rep <- turns_sep %>% filter(Turn <= n * 30)
  turns_long_rep <- turns_rep %>% gather(column, blocks, col_1:col_10, factor_key=TRUE) %>% arrange(Turn)
  
  return(turns_long_rep)
}

blocks_data <- function(data, x) {
  data$blocks_chart_count <- 1
  turns_long_rep <- blocks_turns(data)
  number_df <- data.frame()
  
  block <- 0
  turn <- 0
  
  for (i in 1:nrow(data)) {
    
    for (j in 1:(round(data$blocks_chart_count[i]) * (sum(unique(turns_long_rep$Block) > 0) / sum(unique(turns_long_rep$Turn) > 0))) ) {
      turn <- turn + 1
      for (k in 1:sum(turns_sep$Turn == turn)) {
        block <- block + 1
        
        c_turn <- data.frame(Block = c(block),
                             Turn = c(turn),
                             x = data[[x]][turn])
        
        if (nrow(number_df) == 0) {
          number_df <- c_turn
        } else {
          number_df <- rbind(number_df, c_turn)
        }
      }
    }
  }
  
  tet_df <- merge(turns_long_rep, number_df, by = c("Turn", "Block"), all.x = TRUE)
  tet_df <- tet_df %>% arrange(column) %>% arrange(Block)
  
  return(tet_df)
  
}

blocks_plot <- function(data, count_col, bgcolor = "#ffffff", blockcolor = "#394d6e") {
  
  tdf <- blocks_data(data, count_col)
  
  chart <- ggplot(tdf, aes(fill = as.factor(Turn), y=-blocks, x=as.factor(column))) + 
    geom_col(aes(fill = x, group = Block), 
             position="stack", 
             color = blockcolor, 
             width = 1, 
             size = 1) +
    guides(fill=guide_legend(title=count_col)) +
    coord_fixed() +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          plot.margin = margin(t = 0, r = 40, b = 0, l = 40, unit = "pt"),
          panel.border = element_rect(linetype = "solid", fill = NA, color = NA, size = 3),
          plot.background = element_rect(fill = bgcolor, color = bgcolor),
          panel.background = element_rect(fill = bgcolor, color = bgcolor))
  
  return(chart)
}
