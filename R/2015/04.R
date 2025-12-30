# Day 4: The Ideal Stocking Stuffer
# https://adventofcode.com/2015/day/4

# Setup ----
library(tidyverse)
library(cli)
library(digest)
library(furrr)

# Examples ----
# digest(str_c("abcdef", 609043), algo = "md5", serialize = FALSE)
# digest(str_c("pqrstuv", 1048970), algo = "md5", serialize = FALSE)

# Load Data ----
input <- read_lines("inputs/2015/04.txt")

# Pre-processing ----
# Note: Computing all hashes upfront allows reuse for both parts
n <- 1040000 # Empirically sufficient to find both answers

# Configure parallel processing (leave one core free for system)
plan(multisession, workers = availableCores() - 1)

md5_hashes <- tibble(
    number = 1:n,
    string = str_c(input, number),
    hash = future_map_chr(string, \(x) digest(x, algo = "md5", serialize = FALSE))
)

# Reset to sequential processing
plan(sequential)

# Part 1 - Find lowest number with 5 leading zeros ----
lowest_number_5zeros <- md5_hashes |>
    filter(str_starts(hash, "00000")) |>
    slice(1) |>
    pull(number)

cli_alert_success("Lowest number with 5 leading zeros: {lowest_number_5zeros}")

# Part 2 - Find lowest number with 6 leading zeros ----
lowest_number_6zeros <- md5_hashes |>
    filter(str_starts(hash, "000000")) |>
    slice(1) |>
    pull(number)

cli_alert_success("Lowest number with 6 leading zeros: {lowest_number_6zeros}")
