library(tidyverse)
theme_set(theme_bw())

space_image = read_file("solutions/day08/08-input") %>% 
  strsplit("") %>% 
  unlist()

# reshape input into a table:
mysize = 25*6
n_layers = length(space_image)/mysize

mydata  = tibble(value  = space_image,
                 layer  = rep(1:n_layers, each = mysize),
                 row    = rep(1:25, n_layers*6),
                 col    = rep(1:6,  each = 25) %>% rep(n_layers)
                 )

# Part I
mydata %>% 
  count(layer, value) %>% 
  arrange(value, n) %>% 
  filter(layer == 6)


# Part II
mydata %>% 
  arrange(-layer) %>% 
  ggplot(aes(x = row, y = -col, fill = value)) +
  geom_tile() +
  coord_fixed(1) +
  scale_fill_manual(values = c("white", "black", NA)) +
  theme_void() +
  theme(legend.position = "none")

ggsave("08-bios.png", width = 3, height = 1)

