library(tidyverse)


# Part 1: ---

# Testing:
example_input   = c(12, 14, 1969, 100756)
example_results = floor(example_input/3) - 2
correct_results = c(2, 2, 654, 33583)

example_results == correct_results

# Solution:
sum(floor(scan("01-input")/3)-2)

#' # Part 2: ----

# Solution:
fuel_req_fn = function(module_req){
  req = sum(floor(module_req/3)-2)
  total = req
  while(req > 0){
    req = sum(floor(req/3)-2)
    if(req < 1){break}
    total = c(total, req)
  }
  return(total)
}

map(scan("01-input"), fuel_req_fn) %>% unlist() %>% sum() 

# Testing:
correct_results2 = c(2, 2, 966, 50346) %>% sum()
map(example_input, fuel_req_fn)
test_results2 = map(example_input, fuel_req_fn) %>% unlist() %>% sum()
correct_results2 == test_results2











