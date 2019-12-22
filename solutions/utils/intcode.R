library(tidyverse)

intcode = function(input_changed,
                   opcode_loc = 1,
                   input_val1,
                   input_val2 = NA,
                   use_val1 = TRUE,
                   rel_base = 0,
                   # 1: up, 2: right, 3: down, 4: left
                   robot_facing = 1){
  if (is.na(input_val2)){
    input_val2 = input_val1
  }

  while (opcode_loc <= length(input_changed)){
    #browser()
    # paste("opcode_loc:", opcode_loc) %>% print()
    #opcode_loc = 517
    # it can be up to 5 digits, e.g. 11111, missing is 0 so padding as such
    instr = input_changed[opcode_loc] %>% 
      str_pad(5, pad = "0")
    
    opcode = instr %>% 
      str_sub(4, 5) %>% 
      parse_number()
    
    #if (opcode_loc == 517){browser()}
    
    #if (opcode == 3){browser()}
    if (opcode == 99){
      print("found opcode 99")
      #browser()
      return(list(value = input_val2, program = "END"))
    }
    
    # get values, locations ----
    p1 = str_sub(instr, 3, 3)
    p2 = str_sub(instr, 2, 2)
    p3 = str_sub(instr, 1, 1)
    
    # get values from params
    n1_location = case_when(
      p1 == 0 ~ input_changed[opcode_loc + 1] + 1,
      p1 == 1 ~ opcode_loc + 1,
      p1 == 2 ~ input_changed[opcode_loc + 1] + rel_base + 1,
      TRUE    ~ NA_real_)
    
    n2_location     = case_when(
      p2 == 0 ~ input_changed[opcode_loc + 2] + 1,
      p2 == 1 ~ opcode_loc + 2,
      p2 == 2 ~ input_changed[opcode_loc + 2] + rel_base + 1,
      TRUE    ~ NA_real_)
    
    if (! opcode %in% c(3, 4, 9)){
      update_location = case_when(
        p3 == 0 ~ input_changed[opcode_loc + 3] + 1,
        p3 == 1 ~ opcode_loc + 3,
        p3 == 2 ~ input_changed[opcode_loc + 3]  + rel_base + 1,
        TRUE    ~ NA_real_)
    }else{
      update_location = case_when(
        p1 == 0 ~ input_changed[opcode_loc + 1] + 1,
        p1 == 1 ~ opcode_loc + 1,
        p1 == 2 ~ input_changed[opcode_loc + 1] + rel_base + 1,
        TRUE    ~ NA_real_)
    }
    
    n1 = input_changed[n1_location]
    n2 = input_changed[n2_location]
    
    # opcodes action ----
    ### 1 ###
    if (opcode == 1){
      #paste("input_changed[update_location]:", input_changed[update_location]) %>% print()
      #print(n1)
      #print(n2)
      #if (length(n2)> 1){break}
      input_changed[update_location] = n1 + n2
      
      ### 2 ###
    } else if (opcode == 2){
      input_changed[update_location] = n1*n2
      
      ### 3 ###
    } else if (opcode == 3){
      if (use_val1){
        input_changed[update_location] = input_val1
        use_val1 = FALSE
      } else{
        input_changed[update_location] = input_val2
      }
      
      ### 4 ###
    } else if (opcode == 4){
      #paste("opcode_loc:", opcode_loc) %>% print()
      return(list(value = input_changed[update_location],
                  program = input_changed,
                  loc = opcode_loc + case_when(
                    opcode %in% c(1, 2, 7, 8) ~ 4,
                    opcode %in% c(3, 4, 9)    ~ 2,
                    opcode %in% c(5, 6)       ~ 3,
                    TRUE ~ NA_real_
                  )
                  )
             )
      
      ### 5 ###
    } else if (opcode == 5){
      if (n1 != 0){
        opcode_loc = n2 + 1
        next
      }
      
      ### 6 ###
    } else if (opcode == 6){
      if (n1 == 0){
        opcode_loc = n2 + 1
        next        
      }
      
      ### 7 ###
    } else if (opcode == 7){
      if (n1 < n2){
        input_changed[update_location] = 1
      }else{
        input_changed[update_location] = 0
      }
      
      ### 8 ###
    } else if (opcode == 8){
      if (n1 == n2){
        input_changed[update_location] = 1
      }else{
        input_changed[update_location] = 0
      }
      
      ### 9 ###
    } else if (opcode == 9){
      rel_base = rel_base + n1
      
      ### NA ###
    } else{
      print(opcode)
      paste("Unrecognised upcode. Location:", opcode_loc) %>% 
        stop()
    }
    
    # shift location ----
    # if opcode is 1,2, move on by 4, if 3,4, move on by 2
    #print("increasing opcode loc")
    opcode_loc = opcode_loc + case_when(
      opcode %in% c(1, 2, 7, 8) ~ 4,
      opcode %in% c(3, 4, 9)    ~ 2,
      opcode %in% c(5, 6)       ~ 3,
      TRUE ~ NA_real_
    )
    #test_investigate = input_changed %>% enframe()
  }
  # return(list(value = input_changed[update_location],
  #             program = input_changed,
  #             loc = opcode_loc + case_when(
  #               opcode %in% c(1, 2, 7, 8) ~ 4,
  #               opcode %in% c(3, 4)       ~ 2,
  #               opcode %in% c(5, 6)       ~ 3,
  #               TRUE ~ NA_real_
  #             )
  # )
  # )
}
