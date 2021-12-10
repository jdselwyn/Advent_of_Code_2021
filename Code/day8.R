# --- Day 8: Seven Segment Search ---
#   
#   You barely reach the safety of the cave when the whale smashes into the cave mouth, collapsing it. Sensors indicate another exit to this cave at a much greater depth, so you have no choice but to press on.
# 
# As your submarine slowly makes its way through the cave system, you notice that the four-digit seven-segment displays in your submarine are malfunctioning; they must have been damaged during the escape. You'll be in a lot of trouble without them, so you'd better figure out what's wrong.
# 
# Each digit of a seven-segment display is rendered by turning on or off any of seven segments named a through g:
# 
#   0:      1:      2:      3:      4:
#  aaaa    ....    aaaa    aaaa    ....
# b    c  .    c  .    c  .    c  b    c
# b    c  .    c  .    c  .    c  b    c
#  ....    ....    dddd    dddd    dddd
# e    f  .    f  e    .  .    f  .    f
# e    f  .    f  e    .  .    f  .    f
#  gggg    ....    gggg    gggg    ....
# 
#   5:      6:      7:      8:      9:
#  aaaa    aaaa    aaaa    aaaa    aaaa
# b    .  b    .  .    c  b    c  b    c
# b    .  b    .  .    c  b    c  b    c
#  dddd    dddd    ....    dddd    dddd
# .    f  e    f  .    f  e    f  .    f
# .    f  e    f  .    f  e    f  .    f
#  gggg    gggg    ....    gggg    gggg
# 
# So, to render a 1, only segments c and f would be turned on; the rest would be off. To render a 7, only segments a, c, and f would be turned on.
# 
# The problem is that the signals which control the segments have been mixed up on each display. The submarine is still trying to display numbers by producing output on signal wires a through g, but those wires are connected to segments randomly. Worse, the wire/segment connections are mixed up separately for each four-digit display! (All of the digits within a display use the same connections, though.)
# 
# So, you might know that only signal wires b and g are turned on, but that doesn't mean segments b and g are turned on: the only digit that uses two segments is 1, so it must mean segments c and f are meant to be on. With just that information, you still can't tell which wire (b/g) goes to which segment (c/f). For that, you'll need to collect more information.
# 
# For each display, you watch the changing signals for a while, make a note of all ten unique signal patterns you see, and then write down a single four digit output value (your puzzle input). Using the signal patterns, you should be able to work out which pattern corresponds to which digit.
# 
# For example, here is what you might see in a single entry in your notes:
#   
#   acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |
#   cdfeb fcadb cdfeb cdbaf
# 
# (The entry is wrapped here to two lines so it fits; in your notes, it will all be on a single line.)
# 
# Each entry consists of ten unique signal patterns, a | delimiter, and finally the four digit output value. Within an entry, the same wire/segment connections are used (but you don't know what the connections actually are). The unique signal patterns correspond to the ten different ways the submarine tries to render a digit using the current wire/segment connections. Because 7 is the only digit that uses three segments, dab in the above example means that to render a 7, signal lines d, a, and b are on. Because 4 is the only digit that uses four segments, eafb means that to render a 4, signal lines e, a, f, and b are on.
# 
# Using this information, you should be able to work out which combination of signal wires corresponds to each of the ten digits. Then, you can decode the four digit output value. Unfortunately, in the above example, all of the digits in the output value (cdfeb fcadb cdfeb cdbaf) use five segments and are more difficult to deduce.
# 
# For now, focus on the easy digits. Consider this larger example:
# 
# be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |
# fdgacbe cefdb cefbgd gcbe
# edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |
# fcgedb cgb dgebacf gc
# fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |
# cg cg fdcagb cbg
# fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |
# efabcd cedba gadfec cb
# aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |
# gecf egdcabf bgf bfgea
# fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |
# gebdcfa ecba ca fadegcb
# dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |
# cefg dcbef fcge gbcadfe
# bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |
# ed bcgafe cdgba cbgef
# egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |
# gbdfcae bgc cg cgb
# gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |
# fgae cfgab fg bagce
# 
# Because the digits 1, 4, 7, and 8 each use a unique number of segments, you should be able to tell which combinations of signals correspond to those digits. Counting only digits in the output values (the part after | on each line), in the above example, there are 26 instances of digits that use a unique number of segments (highlighted above).
# 
# In the output values, how many times do digits 1, 4, 7, or 8 appear?

library(tidyverse)
library(magrittr)

code_translation <- tribble(
  ~number, ~sequence, ~top, ~middle, ~bottom, ~tL, ~tR, ~bL, ~bR,
  0, 'abcefg', T, F, T, T, T, T, T,
  1, 'cf', F, F, F, F, T, F, T,
  2, 'acdeg', T, T, T, F, T, T, F,
  3, 'acdfg', T, T, T, F, T, F, T,
  4, 'bcdf', F, T, F, T, T, F, T,
  5, 'abdfg', T, T, T, T, F, F, T,
  6, 'abdefg', T, T, T, T, F, T, T,
  7, 'acf', T, F, F, F, T, F, T,
  8, 'abcdefg', T, T, T, T, T, T, T,
  9, 'abcdfg', T, T, T, T, T, F, T
) %>%
  mutate(word_length = nchar(sequence))

read_notebook <- function(file){
  read_lines(file) %>%
    tibble(full_line = .) %>%
    mutate(message_number = row_number()) %>%
    rowwise(message_number) %>%
    summarise(str_split(full_line, '\\|') %>%
                map_dfr(~str_trim(.) %>% set_names(c('input', 'output'))),
              .groups = 'drop')
}

process_signals <- function(input, output){
  convert_matrix <- function(dat){
    as.matrix(dat[,-1]) %>%
      set_rownames(dat$lett)
  }
  
  convert_matrix_to_number <- function(value, conversion_matrix){
    lit <- rownames(conversion_matrix) %in% unlist(str_split(value, ''))
    
    lit_positions <- set_names(c(rep(TRUE, sum(lit)), rep(FALSE, sum(!lit))), 
                               c(colnames(conversion_matrix)[lit], colnames(conversion_matrix)[!lit])) %>%
      enframe() %>%
      pivot_wider()
    
    out <- inner_join(code_translation, lit_positions,
               by = c("top", "middle", "bottom", "tL", "tR", "bL", "bR")) %>%
      pull(number)
    if(length(out) == 1){
      out
    } else {
      NA_real_
    }
  }
  
  initial_matrix <- tibble(input = input, output = output) %>%
    mutate(across(everything(), ~list(str_split(., ' ') %>% unlist))) %>%
    ungroup %>%
    pivot_longer(cols = c(input, output),
                 names_to = 'message_portion',
                 values_to = 'word') %>%
    unnest(word) %>%
    rowwise %>%
    mutate(lett = list(str_split(word, '') %>% unlist)) %>%
    ungroup %>%
    mutate(word_length = nchar(word)) %>%
    left_join(group_by(code_translation, word_length) %>%
                filter(n() == 1),
              by = 'word_length') %>%
    filter(!is.na(number)) %>%
    select(lett, top:bR) %>%
    unnest(lett) %>%
    group_by(lett) %>%
    summarise(across(where(is.logical), ~mean(.) == 1)) %>%
    convert_matrix
  
  possible_orderings <- tibble(ordering = combinat::permn(7)) %>%
    filter(map_lgl(ordering, ~all(diag(initial_matrix[.x, ])))) %>%
    rowwise %>%
    mutate(mat = list(initial_matrix[ordering,])) %>%
    ungroup %>%
    mutate(ordering = row_number()) %>%
    rowwise %>%
    mutate(in_values = list(unlist(str_split(input, ' '))))
  
  which_order <- possible_orderings %>%
    unnest(in_values) %>%
    rowwise() %>%
    mutate(number = convert_matrix_to_number(in_values, mat)) %>%
    group_by(ordering) %>%
    filter(all(!is.na(number))) %>%
    pull(ordering) %>%
    unique
  
  conversion_mat <- possible_orderings$mat[[which_order]]
  
  unlist(str_split(output, ' ')) %>%
    map_dbl(convert_matrix_to_number, conversion_mat) %>%
    as.character() %>%
    str_c(collapse = '') %>%
    as.integer()
}

process_signals('acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab', 'cdfeb fcadb cdfeb cdbaf')

#### Part 1 ####

read_notebook('../Inputs/day8_example.txt') %>%
  mutate(number = map2_int(input, output, process_signals)) %>%
  mutate(count_easy = str_count(number, '1|4|7|8')) %>%
  summarise(sum(count_easy))

full_out <- read_notebook('../Inputs/day8.txt') %>%
  mutate(number = map2_int(input, output, process_signals)) 

#P1
full_out %>%
  mutate(count_easy = str_count(number, '1|4|7|8')) %>%
  summarise(sum(count_easy))

#P2
sum(full_out$number)
#### Pretty inefficient overall...but interesting. Probably look for better way to find order or converstion matrix