# Day 2: I Was Told There Would Be No Math
# https://adventofcode.com/2015/day/2

# Packages ----
options(tidyverse.quiet = TRUE)
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/02.txt")

# Pre-processing ----
presents <- tibble(dimensions = input) |>
  separate_wider_delim(
    cols = dimensions,
    names = c("l", "w", "h"),
    delim = "x",
    cols_remove = FALSE
  ) |>
  #relocate(dimensions, l, w, h) |>
  mutate(
    across(c(l, w, h), as.integer),
    paper = 2L * (l * w + w * h + h * l) + pmin(l * w, w * h, h * l),
    ribbon = 2L * pmin(l + w, l + h, w + h) + l * w * h
  )

# Part 1 - Calculate wrapping paper needed ----
paper_total <- sum(presents$paper)
cli_alert_success("Total paper needed: {.val {paper_total}} sq ft")

# Part 2 - Calculate ribbon needed ----
ribbon_total <- sum(presents$ribbon)
cli_alert_success("Total ribbon needed: {.val {ribbon_total}} ft")
