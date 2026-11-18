# Day 16: Aunt Sue
# https://adventofcode.com/2015/day/16

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/16.txt")

# Target MFCSAM ticker output ----
target <- c(
  children    = 3,
  cats        = 7,
  samoyeds    = 2,
  pomeranians = 3,
  akitas      = 0,
  vizslas     = 0,
  goldfish    = 5,
  trees       = 3,
  cars        = 2,
  perfumes    = 1
)

# Pre-processing ----
aunts_long <- tibble(raw = input) |>
  mutate(sue = as.integer(str_extract(raw, "\\d+"))) |>
  mutate(props = str_extract_all(raw, "[a-z]+: \\d+")) |>
  unnest(props) |>
  separate_wider_delim(props, delim = ": ", names = c("property", "value")) |>
  mutate(value = as.integer(value))

# Part One ----
matching_sue_part1 <- aunts_long |>
  filter(value == target[property]) |>
  count(sue) |>
  filter(n == 3) |>
  pull(sue)

cli_alert_success("Part 1 - Matching Aunt Sue: {matching_sue_part1}")

# Part Two ----
matching_sue_part2 <- aunts_long |>
  mutate(
    target_val = target[property],
    is_match = case_when(
      property %in% c("cats", "trees")           ~ value >  target_val,
      property %in% c("pomeranians", "goldfish") ~ value <  target_val,
      TRUE                                       ~ value == target_val
    )
  ) |>
  filter(is_match) |>
  count(sue) |>
  filter(n == 3) |>
  pull(sue)

cli_alert_success("Part 2 - Real Aunt Sue: {matching_sue_part2}")
