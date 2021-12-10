# --- Day 9: Smoke Basin ---
#   
#   These caves seem to be lava tubes. Parts are even still volcanically active; small hydrothermal vents release smoke into the caves that slowly settles like rain.
# 
# If you can model how the smoke flows through the caves, you might be able to avoid it and be that much safer. The submarine generates a heightmap of the floor of the nearby caves for you (your puzzle input).
# 
# Smoke flows to the lowest point of the area it's in. For example, consider the following heightmap:
# 
# 2199943210
# 3987894921
# 9856789892
# 8767896789
# 9899965678
# 
# Each number corresponds to the height of a particular location, where 9 is the highest and 0 is the lowest a location can be.
# 
# Your first goal is to find the low points - the locations that are lower than any of its adjacent locations. Most locations have four adjacent locations (up, down, left, and right); locations on the edge or corner of the map have three or two adjacent locations, respectively. (Diagonal locations do not count as adjacent.)
# 
# In the above example, there are four low points, all highlighted: two are in the first row (a 1 and a 0), one is in the third row (a 5), and one is in the bottom row (also a 5). All other locations on the heightmap have some lower adjacent location, and so are not low points.
# 
# The risk level of a low point is 1 plus its height. In the above example, the risk levels of the low points are 2, 1, 6, and 6. The sum of the risk levels of all low points in the heightmap is therefore 15.
# 
# Find all of the low points on your heightmap. What is the sum of the risk levels of all low points on your heightmap?


library(terra)
library(tidyverse)
library(magrittr)

read_map <- function(file){
  initial_read <- read_lines(file) %>%
    str_split('') %>%
    do.call(rbind, .) %>%
    apply(2, as.integer) %>%
    rast
  initial_read
}

find_lowpoints <- function(r){
  # lowpoints <- (focal(r, 3, 'min') == r)

  possible_minima <- focal(r, matrix(c(NA,1,NA,1,1,1,NA,1,NA), nrow=3), 'min', fillvalue = NA, expand = TRUE)
  
  
  # most_common <- focal(r, matrix(c(NA,1,NA,1,1,1,NA,1,NA), nrow=3), 'modal', fillvalue = NA, expand = TRUE)
  # slope <- terrain(r, 'slope', neighbors = 4)
  
  lowpoints <- (possible_minima == r) & (r != 9)
  
  lowpoints[lowpoints == 0] <- NA
  out <- mask(r, lowpoints)
  
  plot(out)
  out
}

analyze_input <- function(file){
  ex_r <- read_map(file)
  plot(ex_r)
  lows <- find_lowpoints(ex_r)
  tmp <- values(lows) %>% add(1) %>% na.omit %>% as.numeric 
  sum(tmp[tmp < 10], na.rm = TRUE)
}

analyze_input('../Inputs/day9_example.txt')

analyze_input('../Inputs/day9.txt')


#### Part 2 ####
find_basin_sizes <- function(r){
  r[r == 9] <- NA

  groups <- patches(r)
  
  plot(groups)
  
  map_dbl(as.numeric(unique(values(groups) %>% na.omit)), ~as.numeric(global(groups == .x, sum, na.rm = TRUE))) %>% sort(decreasing = TRUE)
  
}


read_map('../Inputs/day9_example.txt') %>%
  find_basin_sizes() %>%
  extract(1:3) %>%
  prod

read_map('../Inputs/day9.txt') %>%
  find_basin_sizes() %>%
  extract(1:3) %>%
  prod
