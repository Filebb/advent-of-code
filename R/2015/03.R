# Day 3: Perfectly Spherical Houses in a Vacuum
# https://adventofcode.com/2015/day/3

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/03.txt")

# Pre-processing ----
moves <- tibble(raw = str_split_1(input, "")) |>
    mutate(
        move_x = case_match(raw, "^" ~ 0, ">" ~ 1, "v" ~ 0, "<" ~ -1),
        move_y = case_match(raw, "^" ~ 1, ">" ~ 0, "v" ~ -1, "<" ~ 0)
    )

# Part 1 - Houses visited by Santa ----
santa <- moves |>
    mutate(
        pos_x = cumsum(move_x),
        pos_y = cumsum(move_y)
    ) |>
    add_row(pos_x = 0, pos_y = 0, .before = 1)

santa_houses_visited <- santa |>
    distinct(pos_x, pos_y) |>
    nrow()

cli_alert_success("Houses visited by Santa: {santa_houses_visited}")

# Part 2 - Houses visited by Santa and Robo ----
both <- moves |>
    mutate(who = if_else(row_number() %% 2 == 1, "santa", "robo")) |>
    mutate(
        pos_x = cumsum(move_x),
        pos_y = cumsum(move_y),
        .by = who
    ) |>
    add_row(pos_x = 0, pos_y = 0, .before = 1)

both_houses_visited <- both |>
    distinct(pos_x, pos_y) |>
    nrow()

cli_alert_success("Houses visited by Santa and Robo: {both_houses_visited}")
