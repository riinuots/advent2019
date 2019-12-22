locations = tibble(moon = 1:4,
                   x = c(19, 1, 14, 8),
                   y = c(-10, 2, -4, 7),
                   z = c(7, -3, 1, -6)) %>% 
  gather(coord, value, - moon)

velocities = tibble(moon = 1:4,
                    x = rep(0, 4),
                    y = rep(0, 4),
                    z = rep(0, 4)) %>% 
  gather(coord, velocity, -moon)

max_iter = 500000