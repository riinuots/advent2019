library(tidyverse)
theme_set(theme_bw())

input = "59790132880344516900093091154955597199863490073342910249565395038806135885706290664499164028251508292041959926849162473699550018653393834944216172810195882161876866188294352485183178740261279280213486011018791012560046012995409807741782162189252951939029564062935408459914894373210511494699108265315264830173403743547300700976944780004513514866386570658448247527151658945604790687693036691590606045331434271899594734825392560698221510565391059565109571638751133487824774572142934078485772422422132834305704887084146829228294925039109858598295988853017494057928948890390543290199918610303090142501490713145935617325806587528883833726972378426243439037" %>% strsplit("") %>% unlist() %>% as.numeric()
if (FALSE){
  
  pattern = c(0, 1, 0, -1)
  input_len = length(input)
  
  
  # Part I 
  input_updated = input
  
  for (j in 1:100){
    for (i in 1:input_len){
      # repeat pattern
      n_times = ceiling(input_len/(4*i)) + 1
      phase_pattern = rep(rep(pattern, each = i), n_times)[2:(input_len+1)]
      
      # multiply pattern and input
      input_updated[i] = (phase_pattern*input_updated)  %>%
        sum() %>%
        as.character() %>%
        str_extract("\\d$") %>%
        as.numeric()
    }
  }
  
  
  input_updated[1:8] %>% paste(collapse = "")
}


# Part II

# After trying all kind of weird and wonderful things,
# I went online to realise that Part II was a bit of a trick and that it should not be
# reusing any of the code from Part I

# Just wrapping my head around the modular arithmetic here
# so not to blindly follow online hints:
# offset = 50
# input2 = input %>% rep(10)
# input_len = length(input2)
# pattern = c(0, 1, 0, -1)
# phase_pattern  = rep(rep(pattern, each = offset), 5)[2:(input_len+1)]
# phase_pattern2 = rep(rep(pattern, each = offset+1), 5)[2:(input_len+1)]
# phase_pattern
# 
# mydata  = tibble(x = input2, pattern = phase_pattern, pattern2 = phase_pattern2) %>% 
#   t()


offset = input[1:7] %>% paste(collapse = "") %>% as.numeric()

input2 = input %>% rep(10000)
newlen = length(input2)

# more testing
#phase_pattern1  = rep(rep(pattern, each = offset), 10)[(offset+1):newlen]
#phase_pattern2  = rep(rep(pattern, each = offset+1), 10)[(offset+1):newlen]
#phase_pattern3  = rep(rep(pattern, each = offset+2), 10)[(offset+1):newlen]

input_updated = input2[(offset+1):newlen]
for (i in 1:100){
  if (i %% 10 == 0){print(i)}
  mydata  = tibble(input = input_updated) %>% 
    mutate(rowid = (offset+1):newlen) %>% 
    #slice((offset+1):newlen) %>% 
    #mutate(phase1 = phase_pattern1,
    #       phase2 = phase_pattern2,
    #       phase3 = phase_pattern3,
    #       ) %>% 
    arrange(-rowid) %>% 
    mutate(added = cumsum(input)) %>% 
    mutate(answer = added %% 10) %>% 
    arrange(rowid)
  
  input_updated = mydata$answer
}
input_updated[1:8] %>% paste(collapse = "")
