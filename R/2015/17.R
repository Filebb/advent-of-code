# Day 17: No Such Thing as Too Much
# https://adventofcode.com/2015/day/17

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/17.txt") |>
  as.integer()
target_liters <- 150

# Pre-processing ----
grid_args <- rep(list(c(FALSE, TRUE)), length(input))

combos <- expand_grid(!!!grid_args) |>
  transmute(
    total_capacity = pmap_int(
      .l = pick(where(is.logical)),
      .f = \(...) sum(input[c(...)])
    ),
    n_containers = pmap_int(
      .l = pick(where(is.logical)),
      .f = \(...) length(input[c(...)])
    )
  )

# Part One ----
matching_combos <- combos |>
  filter(total_capacity == target_liters)

cli_alert_success("Total Combinations: {nrow(matching_combos)}")

# Part Two ----
min_combos <- matching_combos |>
  filter(n_containers == min(n_containers))

cli_alert_success("Combinations using minimum containers: {nrow(min_combos)}")

# Method 1: Matrix Multiplication (Fastest Base/Tidy Hybrid)
# Pre-processing (another approach: Matrix Vectorization) ----
grid_matrix <- as.matrix(expand_grid(!!!grid_args))

combos <- tibble(
  total_capacity = as.vector(grid_matrix %*% input),
  n_containers   = rowSums(grid_matrix)
)

matching_combos <- combos |>
  filter(total_capacity == target_liters)

cli_alert_success("Total Combinations: {nrow(matching_combos)}")

min_combos <- matching_combos |>
  filter(n_containers == min(n_containers))

cli_alert_success("Combinations using minimum containers: {nrow(min_combos)}")

# Method 2: Recursive Backtracking / Depth-First Search (Algorithmic Approach)
find_combos <- function(idx = 1, current_sum = 0, count = 0) {
  if (current_sum == target_liters) return(count)
  if (current_sum > target_liters || idx > length(input)) return(NULL)

  c(
    find_combos(idx + 1, current_sum + input[idx], count + 1), # include container
    find_combos(idx + 1, current_sum, count)                  # exclude container
  )
}

# Run DFS
valid_container_counts <- find_combos()

# Results
part1 <- length(valid_container_counts)
part2 <- sum(valid_container_counts == min(valid_container_counts))

cli_alert_success("Part 1: {part1}")
cli_alert_success("Part 2: {part2}")
