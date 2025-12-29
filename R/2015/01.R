# Day 1: Not Quite Lisp
# https://adventofcode.com/2015/day/1

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/01.txt")

# Pre-processing ----
moves <- tibble(
    input = str_split_1(input, ""),
    move = case_match(input, "(" ~  1L, ")" ~ -1L),
    floor = cumsum(move)
)

# Part 1 - Find final floor ----
final_floor <- tail(moves$floor, 1)
cli_alert_success("Final floor: {final_floor}")

# Part 2 - Find first basement entry ----
basement_entry_position <- which(moves$floor == -1)[1]
cli_alert_success("Basement position: {basement_entry_position}")
