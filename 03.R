library(tidyverse)
library(gganimate)

starting_location = tibble(wire = c(1, 2),
                           direction = "R",
                           steps = 0,
                           order = 0,
                           x = 0,
                           y = 0)

wire_locations = melt_csv("03-input") %>% 
  select(wire = row, value) %>% 
  extract(value,
          into = c("direction", "steps"),
                 "([:upper:])([:digit:]+)",
          convert = TRUE) %>% 
  mutate(x = 0, y = 0) %>% 
  group_by(wire) %>% 
  mutate(order = seq_along(wire)) %>% 
  mutate(x = case_when(
    direction == "R" ~ x + steps,
    direction == "L" ~ x - steps,
    TRUE           ~ x) %>%
      cumsum(),
    y = case_when(
      direction == "U" ~ y + steps,
      direction == "D" ~ y - steps,
      TRUE           ~ y) %>%
      cumsum()
    ) %>% 
  ungroup() %>% 
  bind_rows(starting_location) %>% 
  arrange(wire, order) %>% 
  select(order, everything()) %>% 
  mutate(wire = fct_recode(factor(wire), "Wire 1" = "1", "Wire 2" = "2"))

paths_plot = wire_locations %>% 
  ggplot(aes(x = x, y = y, group = wire, colour = wire)) +
  geom_path(alpha = 0.5)  +
  geom_point()


# Part I
# picked coordinates off plotly:
paths_plot +
  coord_cartesian(xlim = c(-600, 0), ylim = c(-200, 0))
plotly::ggplotly()

571+150

# optional gganimate
if (FALSE){
wire_locations %>% 
  ggplot(aes(x = x, y = y, group = wire, colour = wire)) +
  geom_path(alpha = 0.5)  +
  geom_point() +
  transition_reveal(order)

system("mv gganim* 03path")
system("convert 03path/* 03-path.gif")
}

# Part II

paths_labels = wire_locations %>% 
  filter(order < 15) %>% 
  ggplot(aes(x = x, y = y, group = wire, colour = wire)) +
  geom_path(alpha = 0.5)  +
  geom_point() +
  geom_label(aes(label = paste(steps, order)))

# 4 full steps for Wire 2
# 7 full steps for Wire 1
paths_labels +
  coord_cartesian(xlim = c(-1500, -1000), ylim = c(-250, 1000))

plotly::ggplotly()

y = 354
x = -1322

wire1 = wire_locations %>% 
  filter(wire == "Wire 1" & order < 8)

wire2 = wire_locations %>% 
  filter(wire == "Wire 2" & order < 5)

wire1$steps %>% sum() + y + abs(-150) +
wire2$steps %>% sum() + abs(-1322 - -1288)






