library(tidyverse)
library(igraph)

input = read_delim("solutions/day14/14-input-test1", delim = "=") 
reactions = input %>% 
  mutate(output = str_remove(output, "> "),
         input = str_trim(input))

g = reactions %>% 
  select(to = output, from = input, everything()) %>% 
  graph_from_data_frame()

plot(g)

all_simple_paths(g, from = "FUEL", to = "ORE")
