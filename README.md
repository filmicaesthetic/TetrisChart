tetris_plot
================

## Tetris Chart

A function to create a Tetris-themed plot using ggplot2’s geom_col.

![Example of Tetris plot, showing count of video games with over 5
million sales by manufacturer](outputs/tetrisplotexample.png)

## Data

-   Turns_separated.csv : template used to place Tetris-themed blocks
    into the chart

## Use

This plot format is very limited for practical use, and is best used to
count occurrences of factors in very small datasets (\<100 rows).

## Issues

-   Currently has trouble outputting with ggsave
