library(tidyverse)

source("solutions/utils/intcode.R")
input_orig = scan("solutions/day11/11-input", sep = ",")

robot = tibble(x = 0, y = 0, colour = 0, dir = 1, colour_before = 0)

robot_move = function(robot, turn, to_paint){
  #browser()
  last = nrow(robot)
  #turn = 1
  #to_paint = 1
  if (! turn %in% c(0, 1)){stop("turn not 0/1")}
  
  # turn dir left or right
  new_dir = robot$dir[last] + if_else(turn == 0, -1, 1)
  # only values 1:4 make sense
  if (robot$dir[last] == 0){robot$dir[last] = 4}
  if (robot$dir[last] == 5){robot$dir[last] = 1}
  
  robot = bind_rows(robot, tibble(dir = new_dir))
  
  robot$x[last + 1] = robot$x[last] + case_when(
    robot$dir[last + 1] %in% c(1, 3) ~ 0,
    robot$dir[last + 1] ==     2     ~ 1,
    robot$dir[last + 1] ==     4     ~ -1,
    TRUE                             ~ NA_real_
  )
  robot$y[last + 1] = robot$y[last] + case_when(
    robot$dir[last + 1] %in% c(2, 4) ~ 0,
    robot$dir[last + 1] ==     1     ~ 1,
    robot$dir[last + 1] ==     3     ~ -1,
    TRUE                             ~ NA_real_
  )
  robot$colour[last + 1]  = to_paint
  return(robot)
}


to_paint = 0
firsttime = TRUE
counter = 1
while (to_paint %in% c(0, 1)){
  print(counter)
  if(counter > 100){stop}
  if (firsttime){
    res  = intcode(input_orig, opcode_loc = 1, input_val1 = 1)
    to_paint = res$value
    res = intcode(res$program, opcode_loc = res$loc, input_val1 = res$value)
    turn = res$value
    firsttime = FALSE
  } else{
    res = intcode(res$program, opcode_loc = res$loc, input_val1 = res$value)
    to_paint = res$value
    if(counter == 3){browser()}
    res = intcode(res$program, opcode_loc = res$loc, input_val1 = res$value)
    turn = res$value
  }
  robot = robot_move(robot, turn, to_paint)
  counter = counter + 1
}
