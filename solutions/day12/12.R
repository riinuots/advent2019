library(tidyverse)
library(schoolmath)

#source("solutions/day12/12-test1.R")
source("solutions/day12/12-input.R")
locations_orig = locations
velocities_orig = velocities

result = NULL
for (mycoord in c("x", "y", "z")){
locations = locations_orig %>% 
  filter(coord == mycoord)

velocities = velocities_orig %>% 
  filter(coord == mycoord)


first_state = paste(c(locations$value, velocities$velocity), collapse = "/")

time = Sys.time()


for (i in 1:max_iter){

  if((i %% 500) == 0){
    print(i)
    print(Sys.time() - time)
  }
  locations2 = locations %>% set_names(paste0, "_2")
  
  moons_crossed = crossing(locations, locations2) %>% 
    filter(moon != moon_2 & coord == coord_2) %>% 
    mutate(velocity_change = case_when(
      value_2 > value  ~ 1,
      value_2 < value  ~ -1,
      value_2 == value ~ 0,
      TRUE             ~ NA_real_
    )) %>% 
    group_by(moon, coord) %>% 
    summarise(velocity_change = sum(velocity_change))
  
  velocities = velocities %>% 
    left_join(moons_crossed, by = c("moon", "coord")) %>% 
    mutate(velocity = velocity + velocity_change) %>% 
    select(-velocity_change)
  
  # velocities %>% 
  #   spread(coord, velocity)
  
  locations = locations %>% 
    left_join(velocities, by = c("moon", "coord")) %>% 
    mutate(value = value + velocity) %>% 
    select(-velocity)
  
  
  if (paste(c(locations$value, velocities$velocity), collapse = "/") == first_state){
    result = c(result, i)
    paste("Initial state reached at:", i) %>% print()
    break()
  }
  
  if (i == max_iter){
    pots = locations %>%
      mutate(value = abs(value)) %>% 
      group_by(moon) %>% 
      summarise(pot = sum(value))
    kins = velocities %>% 
      mutate(velocity = abs(velocity)) %>% 
      group_by(moon) %>% 
      summarise(kin = sum(velocity))
    
    energy = full_join(pots, kins, by = "moon") %>% 
      mutate(total = pot*kin)
    
    energy$total %>% sum() %>% print()
  }
  
}}


print(Sys.time() - time)

x = result[1]
y = result[2]
z = result[3]
solution = scm(x,scm(y,z))
options(scipen = 999)
solution
# all_locations  = all_locations %>% 
#   enframe()
# 
# 
# all_locations %>%
#   get_dupes(value)

