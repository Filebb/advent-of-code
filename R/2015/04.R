# Day 4: The Ideal Stocking Stuffer
# https://adventofcode.com/2015/day/4

# Setup ----
library(tidyverse)
library(cli)

# Examples ----
examples <- c(
  str_c("abcdef", 609043),
  str_c("pqrstuv", 1048970)
)

# Calculate MD5 ----
# We can use two libraries to find md5
waldo::compare(
  map_chr(examples, \(x) digest::digest(x, algo = "md5", serialize = FALSE)),
  as.character(openssl::md5(examples)) # no need to map since md5 is vectorised
)

# openssl::md5 looks faster
bench::mark(
  digest = map_chr(examples, \(x) {
    digest::digest(x, algo = "md5", serialize = FALSE)
  }),
  openssl = as.character(openssl::md5(examples)),
  min_iterations = 1e4L
)

# Load Data ----
input <- read_lines("inputs/2015/04.txt")

# Function ----

#' Find the Advent Coin (Lowest Number with N Leading Zeros)
#'
#' @description
#' Iterates through numbers in vectorized chunks, appending them to a secret
#' string and computing MD5 hashes until a hash starting with the specified
#' number of zeros is found.
#'
#' @param secret Character string. The base secret key for the puzzle input.
#' @param zeros Integer. The number of leading zeros required in the MD5 hash.
#' @param chunk_size Integer. The number of hashes to evaluate per vectorized
#' batch (default 1,000,000).
#'
#' @return An integer representing the lowest number that produces the target
#' hash.
find_advent_coin <- function(secret, zeros, chunk_size = 1e6L) {
  # Construct the target prefix string (e.g., "00000")
  prefix <- str_dup("0", zeros)
  start <- 1L

  repeat {
    # Generate a chunk of numbers and their hashes vectorised
    numbers <- start:(start + chunk_size - 1L)
    hashes <- as.character(openssl::md5(str_c(secret, numbers)))

    # Check if the target prefix exists in this chunk
    match_idx <- detect_index(hashes, \(x) startsWith(x, prefix))

    # If found, map the index back to the actual number and return it
    if (match_idx > 0) {
      return(numbers[match_idx])
    }

    # Otherwise, shift the search window to the next chunk
    start <- start + chunk_size
  }
}

# Part 1 - Find lowest number with 5 leading zeros ----
lowest_number_5zeros <- find_advent_coin(input, 5)
cli_alert_success(
  "Lowest number with 5 leading zeros: {.val {lowest_number_5zeros}}"
)

# Part 2 - Find lowest number with 6 leading zeros ----
lowest_number_6zeros <- find_advent_coin(input, 6)
cli_alert_success(
  "Lowest number with 6 leading zeros: {.val {lowest_number_6zeros}}"
)
