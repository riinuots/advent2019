locations = tibble(moon = 1:4,
                   x = c(-8, 5, 2, 9),
                   y = c(-10, 5, -7, -8),
                   z = c(0, 10, 3, -3)) %>% 
  gather(coord, value, - moon)

velocities = tibble(moon = 1:4,
                    x = rep(0, 4),
                    y = rep(0, 4),
                    z = rep(0, 4)) %>% 
  gather(coord, velocity, -moon)

max_iter = 100