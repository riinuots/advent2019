library(tidyverse)



# Part 1: ----

input_orig = melt_csv("02-input") %>% 
  mutate(value = parse_number(value)) %>% pull(value)

# testing = c(2,4,4,5,99,0)
# c(1,1,1,4,99,5,6,0,99)
# input_changed = testing

run_program = function(input_changed){
  for (opcode_loc in seq(1, length(input_orig), 4)){
    opcode = input_changed[opcode_loc]
    if (opcode == 99){
      break()
    }
    
    n1_location     = input_changed[opcode_loc + 1] + 1
    n2_location     = input_changed[opcode_loc + 2] + 1
    update_location = input_changed[opcode_loc + 3] + 1
    
    n1 = input_changed[n1_location]
    n2 = input_changed[n2_location]
    
    if (opcode == 1){
      input_changed[update_location] = n1 + n2
    }
    if (opcode == 2){
      input_changed[update_location] = n1*n2
    }
  }
  result = input_changed[1]
  return(result)
}

change_input = function(input_orig, noun, verb){
  input_changed = input_orig
  input_changed[1+1] = noun
  input_changed[2+1] = verb
  return(input_changed)
}

input_changed = input_orig
input_changed[1+1] = 12
input_changed[2+1] = 2

run_program(input_changed)

#change_input(input_orig, 12, 2) %>% run_program()

# Part 2: ----

stop = FALSE
for (noun in 0:99){
  for (verb in 0:99){
    #print(noun)
    #print(verb)
    #print("result:")
    result = change_input(input_orig, noun, verb) %>% run_program()
    #print(result)
    if (near(result, 19690720)){
      answer = 100*noun + verb
      print(answer)
      stop = TRUE
      break
    }
  }
  if (stop){break}
}






