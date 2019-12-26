library(tidyverse)

deck = 0:10006


deal_stack = function(deck){
  deck = rev(deck)
  return(deck)
}

cut_cards = function(deck, n){
  # to make sure we end up with the same lenght we started with:
  len = length(deck)
  if (n > 0){
    first  = deck[1:n]
    second = deck[(n+1):length(deck)]
    deck = c(second, first)
  } else{
    first  = deck[1:(length(deck) + n)]
    second = deck[(length(deck) + n + 1):length(deck)]
    deck = c(second, first)
  }
  
  stopifnot(len == length(deck))
  return(deck)
}

#deal_stack(deck)
#cut_cards(deck, -4)
deal_increment = function(deck, n){
  #n = 7
  n_slots   = length(deck) - 1
  fill_remainder = 0
  all_slots = tibble(slot = 0:n_slots)
  
  filled = all_slots %>% 
    filter(slot %% n == fill_remainder)
  max_filled = max(filled$slot)
  while (nrow(filled) <= n_slots){
    # fill_remainder finds where to fold
    fill_remainder = n - 1 -  (n_slots - max_filled)
    add_filled = all_slots %>% 
      filter(slot %% n == fill_remainder)
    filled = bind_rows(filled, add_filled)
    max_filled = tail(add_filled$slot, 1)
  }
  filled = filled %>% 
    mutate(cards = deck) %>% 
    arrange(slot)
  
  return(filled$cards)
}

# deck = 0:9
# deal_increment(deck, 7)
num_regexp = "-?\\d+"


instructions = read_csv("solutions/day22/22-input") %>% 
  mutate(n = str_extract(instruction, num_regexp) %>% parse_number(),
         instruction = str_remove(instruction, num_regexp) %>% str_trim())

instructions$instruction %>% unique()

space_shuffle = function(deck, instructions){
  for (i in 1:nrow(instructions)){
    #i = 1
    myrow = instructions %>% 
      slice(i)
    
    instruction = myrow$instruction
    n = myrow$n
    
    if (instruction == "cut"){
      deck = cut_cards(deck, n)
    } else if (instruction == "deal with increment"){
      deck = deal_increment(deck, n)
    } else if (instruction == "deal into new stack"){
      deck = deal_stack(deck)
    } else {
      print(i)
      stop("Unknown instruction")
    }
    
  }
  return(deck)
}


which(deck_shuffled == 2019) - 1
