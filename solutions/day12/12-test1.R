locations = tibble(moon = 1:4,
                   x = c(-1, 2, 4, 3),
                   y = c(0, -10, -8, 5),
                   z = c(2, -7, 8, -1)) %>% 
  gather(coord, value, - moon)

velocities = tibble(moon = 1:4,
                    x = rep(0, 4),
                    y = rep(0, 4),
                    z = rep(0, 4)) %>% 
  gather(coord, velocity, -moon)

max_iter = 2772