# Day 8: Matchsticks
# https://adventofcode.com/2015/day/8

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines(file = "inputs/2015/08.txt")

# Part One ----
strings <- tibble(
  original = input,
  code     = str_length(input),
  memory   = map_int(
    .x = input,
    .f = \(string) parse(text = string) |> eval() |> nchar(type = "bytes")
  )
) |>
  mutate(diff = code - memory)

code_vs_memory_overhead <- sum(strings$diff)
cli_alert_success("Code vs Memory Char Overhead: {code_vs_memory_overhead}")

# Part Two ----
strings <- strings |>
  mutate(
    # Each existing '\' and '"' adds 1 extra character when encoded,
    # plus 2 characters for the new surrounding outer quotes.
    diff2 = str_count(input, fixed("\\")) + str_count(input, fixed("\"")) + 2
  )

encoded_vs_code_expansion <- sum(strings$diff2)
cli_alert_success("Encoded vs Code Char Expansion: {encoded_vs_code_expansion}")
