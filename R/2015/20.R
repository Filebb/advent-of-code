# Day 20: Medicine for Rudolph
# https://adventofcode.com/2015/day/20

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
target <- read_lines("inputs/2015/20.txt") |>
  as.integer()

max_houses <- input %/% 42

# Part One ----
houses <- numeric(max_houses)

for (elf in 1:max_houses) {
  # Vectorized indexing: deliver to all multiples of 'elf'
  stops <- seq.int(elf, max_houses, by = elf)
  houses[stops] <- houses[stops] + (elf * 10)
}

which(houses >= target)[1]

# Part Two ----
houses <- numeric(max_houses)

for (elf in 1:max_houses) {
  # Elf stops delivering after 50 houses
  max_stop <- min(max_houses, elf * 50)
  stops <- seq.int(elf, max_stop, by = elf)
  houses[stops] <- houses[stops] + (elf * 11)
}

which(houses >= target)[1]
