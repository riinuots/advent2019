library(tidyverse)
library(igraph)

input = read_delim("solutions/day14/14-input-test2", delim = "=") 
#input = read_delim("solutions/day14/14-input", delim = "=") 

reactions = input %>% 
  separate(input, into = paste0("input", 1:11), sep = ",") %>% 
  gather(input_id, input, -output) %>% 
  drop_na() %>% 
  select(-input_id) %>% 
  mutate(output = str_remove(output, "> "),
         input = str_trim(input)) %>%
  select(input, output) %>%
  separate(input, into = c("input_n", "input"), convert = TRUE) %>%
  separate(output, into = c("output_n", "output"), convert = TRUE) %>% 
  mutate(input_n = input_n/output_n,
         output_n = 1)

#chemical = "FUEL"
ore_needed = 0
needed = tibble(needed_n = numeric(), needed = character())

list_reqs = function(chemical, needed_n = 1){
  #chemical = "FUEL"
  #needed_n = 1
  reqs = reactions %>% 
    filter(output == chemical) %>% 
    mutate(needed_n = input_n*needed_n) %>% 
    mutate(needed = paste(input, needed_n, sep = "-")) %>% 
    select(needed_n, needed, output)
  
  return(reqs)
}

#list_reqs("A")

needed = list(bind_rows(needed, list_reqs("FUEL")))
step = 1

while (TRUE){
  print(step)
  for (myneeded in needed[[step]]$needed){
    #myneeded = "ABS-123"
    chemical = str_extract(myneeded, "\\w+")
    needed_n = str_extract(myneeded, "\\d+") %>% as.numeric()
    print(chemical)
    needed = c(needed, list(list_reqs(chemical, needed_n)))
  }
  if (step == length(needed)){
    #browser()
    break
    }else{
    step = length(needed)
    }


}

# needed[[1]]
# needed[[2]]
# needed[[3]]
# needed[[4]]
# needed[[5]]
# needed[[6]]

needed = do.call(rbind, needed)

#needed %>% spread()

# reactions %>% 
#   rename(from  = input, to = output) %>% 
#   graph_from_data_frame() %>% 
#   plot()




