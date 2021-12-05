#### Part 1 ####

#Count the number of times there is an increase in a vector of values
library(magrittr)

count_increase <- function(depths){
  sum(diff(depths) > 0)
}


example_data <- c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
count_increase(example_data)

input_data <- readr::read_lines('../Inputs/day1.txt') %>% as.numeric()

count_increase(input_data)


#### Part 2 ####
count_sliding_window_increase <- function(depth, window_size){
  sum(diff(depth, window_size) > 0)
}

count_sliding_window_increase(example_data, 3)

count_sliding_window_increase(input_data, 3)
