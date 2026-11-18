# Day 18: Like a GIF For Your Yard
# https://adventofcode.com/2015/day/18

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/18.txt")

grid <- matrix(
  unlist(strsplit(input, "")) == "#",
  nrow = length(input),
  byrow = TRUE
)

count_neighbors <- function(m) {
  nr <- nrow(m)
  nc <- ncol(m)

  # Pad grid with FALSE (0) around the border to handle edges automatically
  padded <- matrix(FALSE, nrow = nr + 2, ncol = nc + 2)
  padded[2:(nr + 1), 2:(nc + 1)] <- m

  # Sum the 8 surrounding direction slices
  padded[1:nr,       1:nc]       + # Top-Left
    padded[1:nr,       2:(nc + 1)] + # Top
    padded[1:nr,       3:(nc + 2)] + # Top-Right
    padded[2:(nr + 1), 1:nc]       + # Left
    padded[2:(nr + 1), 3:(nc + 2)] + # Right
    padded[3:(nr + 2), 1:nc]       + # Bottom-Left
    padded[3:(nr + 2), 2:(nc + 1)] + # Bottom
    padded[3:(nr + 2), 3:(nc + 2)]   # Bottom-Right
}

# 3. Step Function ----
step_grid <- function(m, part2 = FALSE) {
  nr <- nrow(m)
  nc <- ncol(m)

  # If Part 2, force four corners ON before calculating
  if (part2) {
    m[1, 1] <- m[1, nc] <- m[nr, 1] <- m[nr, nc] <- TRUE
  }

  neighbors <- count_neighbors(m)

  # Rules:
  # - A light that is ON stays ON if it has 2 or 3 neighbors.
  # - A light that is OFF turns ON if it has exactly 3 neighbors.
  next_m <- (m & (neighbors == 2 | neighbors == 3)) | (!m & (neighbors == 3))

  # If Part 2, force four corners ON after calculating
  if (part2) {
    next_m[1, 1] <- next_m[1, nc] <- next_m[nr, 1] <- next_m[nr, nc] <- TRUE
  }

  return(next_m)
}

run_simulation <- function(grid, steps = 100, part2 = FALSE) {
  1:steps |>
    reduce(\(m, ...) step_grid(m, part2 = part2), .init = grid) |>
    sum()
}

# 5. Output Results ----
p1 <- run_simulation(grid, steps = 100, part2 = FALSE)
p2 <- run_simulation(grid, steps = 100, part2 = TRUE)

p1
p2
