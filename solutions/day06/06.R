library(tidyverse)
library(igraph)

# Part I 
orbit_input = read_delim("solutions/day06/06-input", delim = ")", col_names = c("from", "to"))

g = orbit_input %>% 
  graph_from_data_frame(directed = TRUE)

g %>% 
  distances(to = "COM") %>%
  sum()


# Part II

YOU_orbits = orbit_input %>% 
  filter(to == "YOU") %>% 
  pull(from)

SAN_orbits = orbit_input %>% 
  filter(to == "SAN") %>% 
  pull(from)

distances(g, v = YOU_orbits, to = SAN_orbits)



