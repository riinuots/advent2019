---
title: "Diary: Advent of Code 2019"
output:
  html_document:
    keep_md: yes
    toc: yes
---

Notes about functions/arguments I'd not come across before. And any other thoughts. Full solutions can be found in, e.g., `01.R`, `02.R`, etc.

# Day 3

Learning from the time I spent in vain the previous day trying to make the exercise conform to R (they're not data science challenges...), I decided to do things my way.

My solution was super fast an fun - plot it and pick the answers from the plot:

![](https://github.com/riinuots/advent2019/blob/master/03-path.gif?raw=true)

Dummy code to show that sometimes gganimate is just a single line (`transition_reveal(var)`):


```r
library(tidyverse)
library(gganimate)

data %>% 
  ggplot(aes(x = x, y = y, group = something)) +
  geom_path(alpa = 0.5) +
  transition_reveal(index_var)
```

This creates a bunch of png files, I then used imagemagick to convert:

`convert *.png output.gif` (or `Sys("convert *.png output.gif")` in an R script)

After meditationg over the gif a bit, I used `plotly::ggplotly()` to interact with the plot (zoom + hover over pooints for intersection coordinates).  
Simple, but works. `¯\_(ツ)_/¯`

# Day 2

`melt_csv()` - input was in rows, but this "melts" them into columns. Sweet!

Tried really hard to make the solution look like proper R code, with maps rather than for loops etc. Gave up and bashed out a nested for loop with heavy use of `[` `]` for indexing.

Added +1 to every coordinate as instructions start counting from 0, R (and Fortran, for that matter) starts from 1.

# Day 1

new R function (new for me...): `scan("input_file")` - reads into a simple vector rather than a proper table/data frame/tibble




