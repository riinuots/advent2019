library(tidyverse)

source("solutions/utils/intcode.R")

phases = 0:4
phases = 5:9
phase_permutations = gtools::permutations(5, 5, phases)


input_test = c(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
input = c(3,8,1001,8,10,8,105,1,0,0,21,42,67,88,105,114,195,276,357,438,99999,3,9,101,4,9,9,102,3,9,9,1001,9,2,9,102,4,9,9,4,9,99,3,9,1001,9,4,9,102,4,9,9,101,2,9,9,1002,9,5,9,1001,9,2,9,4,9,99,3,9,1001,9,4,9,1002,9,4,9,101,2,9,9,1002,9,2,9,4,9,99,3,9,101,4,9,9,102,3,9,9,1001,9,5,9,4,9,99,3,9,102,5,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,99)

input_orig = c(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
               27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)

input_orig = input

run_amps = function(input_program, locations, input_value, amp, use_val1 = TRUE, feedback){
  
  #paste("starting at", locations$l1) %>% print()
  #paste("input at this location is:", input_program$m1[locations$l1]) %>% print()
  #print("a1")
  a1 = intcode(input_program$m1,
               opcode_loc = locations$l1,
               input_val1 = if_else(feedback, input_value, as.numeric(amp[1])),
               input_val2 = input_value,
               use_val1 = use_val1)
  
  #paste("starting at", locations$l2) %>% print()
  #browser()
  #print(a1$program)

  
  print("a2")
  a2 = intcode(input_program$m2,
               opcode_loc = locations$l2,
               input_val1 = if_else(feedback, a1$value, as.numeric(amp[2])),
               input_val2 = a1$value,
               use_val1 = use_val1)
  print("a3")
  a3 = intcode(input_program$m3,
               opcode_loc = locations$l3,
               input_val1 = if_else(feedback, a2$value, as.numeric(amp[3])),
               input_val2 = a2$value,
               use_val1 = use_val1)
  print("a4")
  a4 = intcode(input_program$m4,
               opcode_loc = locations$l4,
               input_val1 = if_else(feedback, a3$value, as.numeric(amp[4])),
               input_val2 = a3$value,
               use_val1 = use_val1)
  print("a5")
  a5 = intcode(input_program$m5, 
               opcode_loc = locations$l5,
               input_val1 = if_else(feedback, a4$value, as.numeric(amp[5])),
               input_val2 = a4$value,
               use_val1 = use_val1)
  if (a5$program == "END"){
    #browser()
    #print("a1 signalling end")
    #print(input_value)
    #stop("ENOUGH")
    #print(list(memory = "END", out_value = input_value))
    return(list(memory = "END", out_value = input_value))
    
  }
  
  
  return(
    list(
      memory = 
        list(
          m1 = a1$program, 
          m2 = a2$program, 
          m3 = a3$program, 
          m4 = a4$program, 
          m5 = a5$program), 
      locations = 
        list(
          l1 = a1$loc, 
          l2 = a2$loc, 
          l3 = a3$loc, 
          l4 = a4$loc, 
          l5 = a5$loc), 
      out_value = a5$value)
  )
}

try_phases = function(try_amp = 9:5){
  
  first = TRUE
  counter = 1
  while (TRUE){
    #paste(counter, "nth time running") %>%
      #print()
    #if (counter == 10) {browser()}
    counter = counter +1
    if (first){
      result = run_amps(input_program = list(m1 = input_orig,
                                             m2 = input_orig,
                                             m3 = input_orig,
                                             m4 = input_orig,
                                             m5 = input_orig),
                        locations = list(l1 = 1, l2 = 1, l3 = 1, l4 = 1, l5 = 1),
                        input_value = 0, amp = try_amp,
                        feedback = FALSE)
      first = FALSE
    } else if (result$memory == "END"){
      print("END found")
      #print(result)
      #print(result$out_value)
      return(result$out_value)
      #return(result$out_value)
      break
    } else{
      result = run_amps(input_program = result$memory,
                        locations =  result$locations,
                        input_value = result$out_value,
                        amp = try_amp,
                        use_val1 = FALSE,
                        feedback = TRUE)
    }
  }
  
  #print(result)
}


#val = suppressWarnings(try_phases())


# Part I
# max = 0
# for (i in 1:nrow(phase_permutations)){
#   result = run_amps(input,
#            phase_permutations[i, 1],
#            phase_permutations[i, 2],
#            phase_permutations[i, 3],
#            phase_permutations[i, 4],
#            phase_permutations[i, 5])
#   
#   if (result > max){
#     max = result
#   }
# }
# max

# Part II

max = 0

for (i in 1:nrow(phase_permutations)){
  print(i)
  my_amp = c(phase_permutations[i, 1],
           phase_permutations[i, 2],
           phase_permutations[i, 3],
           phase_permutations[i, 4],
           phase_permutations[i, 5])
  val = suppressWarnings(try_phases(my_amp))
  if (val > max){
    max = val
  }
}
max






