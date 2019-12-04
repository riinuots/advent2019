#' Code at [https://github.com/riinuots/advent2019](https://github.com/riinuots/advent2019)
#+ message = FALSE, echo = FALSE
library(tidyverse)
library(DT)
theme_set(theme_bw())

# Part I

tibble(code = 254032:789860) %>% 
  extract(code, into  = paste0("c", 1:6),
          rep("([:digit:])", 6) %>% paste(collapse = ""),
          remove = FALSE) %>% 
  filter(
      c2 >= c1 &
      c3 >= c2 &
      c4 >= c3 &
      c5 >= c4 &
      c6 >= c5
    ) %>% 
  filter(
      c2 == c1 |
      c3 == c2 |
      c4 == c3 |
      c5 == c4 |
      c6 == c5
  ) %>% 
  nrow()

# Part II

part2 = tibble(code = 254032:789860) %>% 
  extract(code, into  = paste0("c", 1:6),
          rep("([:digit:])", 6) %>% paste(collapse = ""),
          remove = FALSE) %>% 
  filter(
      c2 >= c1 &
      c3 >= c2 &
      c4 >= c3 &
      c5 >= c4 &
      c6 >= c5
  ) %>%
  filter(
      (c2 == c1 & c2 != c3)|
      (c3 == c2 & (c3 != c4 & c2 != c1))|
      (c4 == c3 & (c4 != c5 & c3 != c2))|
      (c5 == c4 & (c5 != c6 & c4 != c3))|
      (c6 == c5 & (c5 != c4 & c5 != c4))
  )

#' ## Using datatable for debugging
datatable(part2, options = list(pageLength = 100), rownames = FALSE) %>% 
  formatStyle(
    paste0("c", 1:6),
    backgroundColor = styleEqual(0:9, RColorBrewer::brewer.pal(10, "Paired"))
  )

part2 %>% nrow()




