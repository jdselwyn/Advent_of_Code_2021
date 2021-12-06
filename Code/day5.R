# --- Day 5: Hydrothermal Venture ---
#   
#   You come across a field of hydrothermal vents on the ocean floor! These vents constantly produce large, opaque clouds, so it would be best to avoid them if possible.
# 
# They tend to form in lines; the submarine helpfully produces a list of nearby lines of vents (your puzzle input) for you to review. For example:
#   
#   0,9 -> 5,9
# 8,0 -> 0,8
# 9,4 -> 3,4
# 2,2 -> 2,1
# 7,0 -> 7,4
# 6,4 -> 2,0
# 0,9 -> 2,9
# 3,4 -> 1,4
# 0,0 -> 8,8
# 5,5 -> 8,2
# 
# Each line of vents is given as a line segment in the format x1,y1 -> x2,y2 where x1,y1 are the coordinates of one end the line segment and x2,y2 are the coordinates of the other end. These line segments include the points at both ends. In other words:
#   
#   An entry like 1,1 -> 1,3 covers points 1,1, 1,2, and 1,3.
# An entry like 9,7 -> 7,7 covers points 9,7, 8,7, and 7,7.
# 
# For now, only consider horizontal and vertical lines: lines where either x1 = x2 or y1 = y2.
# 
# So, the horizontal and vertical lines from the above list would produce the following diagram:
#   
#   .......1..
# ..1....1..
# ..1....1..
# .......1..
# .112111211
# ..........
# ..........
# ..........
# ..........
# 222111....
# 
# In this diagram, the top left corner is 0,0 and the bottom right corner is 9,9. Each position is shown as the number of lines which cover that point or . if no line covers that point. The top-left pair of 1s, for example, comes from 2,2 -> 2,1; the very bottom row is formed by the overlapping lines 0,9 -> 5,9 and 0,9 -> 2,9.
# 
# To avoid the most dangerous areas, you need to determine the number of points where at least two lines overlap. In the above example, this is anywhere in the diagram with a 2 or larger - a total of 5 points.
# 
# Consider only horizontal and vertical lines. At how many points do at least two lines overlap?
#   
rm(list=ls())
library(tidyverse)

read_coordinates <- function(file){
  read_lines(file) %>%
    tibble(start_end = .) %>%
    separate(start_end, into = c('x1', 'y1', 'x2', 'y2'), convert = TRUE) %>%
    mutate(across(everything(), ~.+1L)) #make 1 based not 0 based
}


#Initial attempt - was super inefficient so gave up and tried different approach
position_check <- function(start, end, test_position){
  x <- c(start[1], end[1])
  y <- c(start[2], end[2])
  
  within_x <- (test_position[1] >= min(x) & test_position[1] <= max(x))
  within_y <- (test_position[2] >= min(y) & test_position[2] <= max(y))
  
  if(diff(x) == 0L){
    within_x & within_y
  } else {
    slope <- diff(y) / diff(x)
    intercept <- unique(y - slope*x)
    
    on_line <- test_position[2] == round(slope*test_position[1] + intercept)
    on_line & within_x & within_y
  }

}

fill_map <- function(x, y, coords){
  coords %>%
    summarise(t1 = position_check(start, end, c(x,y)), .groups = 'drop') %>%
    pull(t1) %>%
    sum
}

fill_full_map <- function(coordinates){
  x_range <- range(c(coordinates$x1, coordinates$x2))
  y_range <- range(c(coordinates$y1, coordinates$y2))
  
  the_map <- matrix(0, ncol = diff(x_range) + 1, nrow = diff(y_range) + 1)
  
  coords <- coordinates %>%
    rowwise() %>%
    summarise(start = cbind(x1, y1),
              end = cbind(x2, y2),
              .groups = 'rowwise') 
  
  hits <- expand_grid(x = 1:ncol(the_map), y = 1:nrow(the_map)) %>%
    rowwise() %>%
    mutate(n = fill_map(x, y, coords)) %>%
    ungroup
  
  the_map[as.matrix(select(hits, -n))] <- hits$n
  t(the_map)
}

# 
# filled_map <- read_coordinates('../Inputs/day5_example.txt') %>%
#   filter(x1 == x2 | y1 == y2) %>%
#   fill_full_map
# 
# sum(filled_map >= 2)
# 
# filled_map <- read_coordinates('../Inputs/day5.txt') %>%
#   filter(x1 == x2 | y1 == y2) %>%
#   fill_full_map
# 
# sum(filled_map >= 2)

read_coordinates('../Inputs/day5.txt') %>%
  filter(x1 == x2 | y1 == y2) %>%
  rowwise %>%
  summarise(x = seq(x1, x2),
            y = seq(y1, y2),
            .groups = 'drop') %>%
  count(x, y) %>%
  summarise(sum(n > 1))


#### Part 2 ####
# --- Part Two ---
#   
#   Unfortunately, considering only horizontal and vertical lines doesn't give you the full picture; you need to also consider diagonal lines.
# 
# Because of the limits of the hydrothermal vent mapping system, the lines in your list will only ever be horizontal, vertical, or a diagonal line at exactly 45 degrees. In other words:
# 
#     An entry like 1,1 -> 3,3 covers points 1,1, 2,2, and 3,3.
#     An entry like 9,7 -> 7,9 covers points 9,7, 8,8, and 7,9.
# 
# Considering all lines from the above example would now produce the following diagram:
# 
# 1.1....11.
# .111...2..
# ..2.1.111.
# ...1.2.2..
# .112313211
# ...1.2....
# ..1...1...
# .1.....1..
# 1.......1.
# 222111....
# 
# You still need to determine the number of points where at least two lines overlap. In the above example, this is still anywhere in the diagram with a 2 or larger - now a total of 12 points.
# 
# Consider all of the lines. At how many points do at least two lines overlap?

read_coordinates('../Inputs/day5.txt') %>%
  # filter(x1 == x2 | y1 == y2) %>%
  rowwise %>%
  summarise(x = seq(x1, x2),
            y = seq(y1, y2),
            .groups = 'drop') %>%
  count(x, y) %>%
  summarise(sum(n > 1))
