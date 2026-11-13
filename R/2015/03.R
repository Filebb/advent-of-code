# Day 3: Perfectly Spherical Houses in a Vacuum
# https://adventofcode.com/2015/day/3

# Packages ----
options(tidyverse.quiet = TRUE)
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/03.txt")

# Pre-processing ----
moves <- tibble(move = str_split_1(input, "")) |>
  mutate(
    move_x = recode_values(move, "^" ~ 0, ">" ~ 1, "v" ~ 0, "<" ~ -1),
    move_y = recode_values(move, "^" ~ 1, ">" ~ 0, "v" ~ -1, "<" ~ 0)
  )

# Part 1 - Houses visited by Santa ----
santa <- moves |>
  mutate(
    pos_x = cumsum(move_x),
    pos_y = cumsum(move_y)
  ) |>
  add_row(pos_x = 0, pos_y = 0, .before = 1)

santa_houses <- n_distinct(santa$pos_x, santa$pos_y)
cli_alert_success("Houses visited by Santa: {.val {santa_houses}}")

# Part 2 - Houses visited by Santa and Robo ----
both <- moves |>
  mutate(who = if_else(row_number() %% 2 == 1, "santa", "robo")) |>
  mutate(
    pos_x = cumsum(move_x),
    pos_y = cumsum(move_y),
    .by = who
  ) |>
  add_row(pos_x = 0, pos_y = 0, .before = 1)

both_houses <- n_distinct(both$pos_x, both$pos_y)
cli_alert_success("Houses visited by Santa and Robo: {.val {both_houses}}")
