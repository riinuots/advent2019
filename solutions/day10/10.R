library(raster)
library(tidyverse)
library(gghighlight)
library(matlib)
theme_set(theme_bw())

# Read in and reshape into long
satellites = read_table("solutions/day10/10-input", col_names = "sat") %>% 
  mutate(y0 = row_number()) %>% 
  mutate(sat = str_replace_all(sat, c("\\." = "0", "#" = "1")) %>% 
           strsplit("")) %>% 
  unnest(sat) %>% 
  mutate(sat = as.numeric(sat)) %>% 
  group_by(y0) %>% 
  mutate(x0 = seq_along(y0)) %>% 
  ungroup() %>% 
  filter(sat == 1) %>% 
  select(-sat) %>% 
  mutate(sat_id0 = seq_along(y0))

# Cross the same data 3 times,
# all combinations of from (sat_id0), to (sat_id1), and possible intercepter (sat_id2) 
sat_duplicate  = satellites %>% rename(sat_id1 = sat_id0, x1 = x0, y1 = y0)
sat_duplicate2 = satellites %>% rename(sat_id2 = sat_id0, x2 = x0, y2 = y0)

sat_matrix = crossing(satellites, sat_duplicate) %>% 
  filter(sat_id0 != sat_id1) %>% 
  crossing(sat_duplicate2) %>% 
  select(sat_id0, x0, y0, sat_id1, x1, y1, sat_id2, x2, y2) %>% 
  filter(sat_id2 != sat_id0, sat_id2 != sat_id1)

# calculate distances, interception happens if
# dist02 + dist12 = dist01 (means that interceptor must be on the shortest path)
sat_matrix_dist = sat_matrix %>% 
  mutate(dist02 = pointDistance(as.matrix(sat_matrix[, c("x0", "y0")]),
                                 as.matrix(sat_matrix[, c("x2", "y2")]),
                                 lonlat = FALSE),
         dist01 = pointDistance(as.matrix(sat_matrix[, c("x0", "y0")]),
                                 as.matrix(sat_matrix[, c("x1", "y1")]),
                                 lonlat = FALSE),
         dist12 = pointDistance(as.matrix(sat_matrix[, c("x1", "y1")]),
                                 as.matrix(sat_matrix[, c("x2", "y2")]),
                                 lonlat = FALSE)) %>% 
  mutate(intercepts = near(dist01, dist02 + dist12))

save(sat_matrix_dist, file = "satellites_distances.rda")

# summarise to see if any other sattelites are intercepting between id0/id1 pairs
sat_destinations_intercepted = sat_matrix_dist %>% 
  group_by(sat_id0, sat_id2) %>% 
  summarise(any_intercept = any(intercepts))

sat_destinations_intercepted %>%
  count(sat_id0, any_intercept, sort = TRUE) %>% 
  ungroup() %>% 
  slice(1)

satellites %>% 
  ggplot(aes(x = x0, y = -y0, label = sat_id0)) +
  #geom_label() +
  geom_point() +
  gghighlight(sat_id0 == 312)

# Part II
straight_up = c(21, 0) - c(21, -19)

laser = sat_matrix_dist %>% 
  select(-sat_id2, -x2, -y2, -dist02, -dist12, -intercepts) %>% 
  distinct() %>% 
  filter(sat_id0 == 312)

laser = laser %>% 
  mutate(y0 = -y0, y1 = -y1) %>% 
  arrange(sat_id1)

save(laser, file = "laser.rda")

laser %>% 
  ggplot(aes(x = x1, y = y1)) +
  #geom_label() +
  geom_point() +
  #gghighlight(sat_id1 %in% c(11, 13, 331) ) +
  geom_point(data = tibble(x1 = 21, y1 = -19), colour = "red")



laser$angle = NA
for (i in 1:nrow(laser)){
  #print(i)
  angle = angle(straight_up,
                as.vector(as.matrix(laser[i, c("x1", "y1")])) - as.vector(as.matrix(laser[i, c("x0", "y0")]))
                )
  if (near(angle, 0) & laser$y1[i] > 19){
    angle = 180
  }
  if (near(angle, 180) & laser$y1[i] < 19){
    angle = 0
  }
    
  if(laser$x1[i]  > laser$x0[i]){
   angle = abs(180 - angle)
  }
  if(laser$x1[i]  < laser$x0[i]){
    angle = 180 + angle
  }
  
  laser$angle[i] = angle
}


shooting_order = laser %>% 
  group_by(angle) %>% 
  arrange(dist01) %>% 
  mutate(n_row = seq_along(angle)) %>% 
  arrange(n_row, angle) %>% 
  ungroup()

laser %>% 
  ggplot(aes(x = x1, y = y1)) +
  geom_point() +
  geom_point(data = tibble(x1 = 21, y1 = -19), colour = "red") +
  geom_segment(data = slice(shooting_order, 1:20),
               aes(x = 21, y = -19, xend = x1, yend = y1), colour = "red")


#807
#706


