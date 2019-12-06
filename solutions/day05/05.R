library(tidyverse)

input_orig = melt_csv("05-input") %>% 
  mutate(value = parse_number(value)) %>% pull(value)

input_changed = input_orig


input_val = 1
opcode_loc = 1

# test_orig = c(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
#             1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
#             999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
#input_changed = test_orig
# test_investigate = input_changed %>% enframe()

input_val = 5
# while loop ----
while (opcode_loc < length(input_changed)){
  #paste("opcode_loc:", opcode_loc) %>% print()
  instr = input_changed[opcode_loc] %>% 
    # it can be up to 5 digits, e.g. 11111, missing is 0 so padding as such
    str_pad(5, pad = "0")
  
  opcode = instr %>% 
    str_sub(4, 5) %>% 
    parse_number()
  
  if (opcode == 99){
    break()
  }
  
  # get values, locations ----
  p1 = str_sub(instr, 3, 3)
  p2 = str_sub(instr, 2, 2)
  p3 = str_sub(instr, 1, 1)
  
  
  
  n1_location     = if_else(p1 == 0,
                            input_changed[opcode_loc + 1] + 1,
                            opcode_loc + 1)
  n2_location     = if_else(p2 == 0,
                            input_changed[opcode_loc + 2] + 1,
                            opcode_loc + 2)
  if (! opcode %in% c(3, 4)){
    update_location = if_else(p3 == 0,
                              input_changed[opcode_loc + 3] + 1,
                              opcode_loc + 3)
  }else{
    update_location = if_else(p1 == 0,
                              input_changed[opcode_loc + 1] + 1,
                              opcode_loc + 1)
  }
  
  n1 = input_changed[n1_location]
  n2 = input_changed[n2_location]
  
  # opcodes action ----
  if (opcode == 1){
    #paste("input_changed[update_location]:", input_changed[update_location]) %>% print()
    #print(n1)
    #print(n2)
    #if (length(n2)> 1){break}
    input_changed[update_location] = n1 + n2
  }
  if (opcode == 2){
    input_changed[update_location] = n1*n2
  }
  if (opcode == 3){
    input_changed[update_location] = input_val
  }
  if (opcode == 4){
    #paste("opcode_loc:", opcode_loc) %>% print()
    print(input_changed[update_location])
  }
  if (opcode == 5 & n1 != 0){
    opcode_loc = n2 + 1
    next
  }
  if (opcode == 6 & n1 == 0){
    opcode_loc = n2 + 1
    next
  }
  if (opcode == 7){
    if (n1 < n2){
      input_changed[update_location] = 1
    }else{
      input_changed[update_location] = 0
    }
  }
  if (opcode == 8){
    if (n1 == n2){
      input_changed[update_location] = 1
    }else{
      input_changed[update_location] = 0
    }
  }
  
  # shift location ----
  # if opcode is 1,2, move on by 4, if 3,4, move on by 2
  #print("increasing opcode loc")
  opcode_loc = opcode_loc + case_when(
    opcode %in% c(1, 2, 7, 8) ~ 4,
    opcode %in% c(3, 4) ~ 2,
    opcode %in% c(5, 6) ~ 3,
    TRUE ~ NA_real_
  )
  test_investigate = input_changed %>% enframe()
}

# result = input_changed[1]

# run_program = function(input_changed){
# 
#   return(result)
# }




