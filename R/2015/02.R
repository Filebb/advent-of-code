# Day 2: I Was Told There Would Be No Math
# https://adventofcode.com/2015/day/2

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/02.txt")

# Pre-processing ----
paper <- tibble(present_dimensions = input) |>
    separate_wider_delim(cols = present_dimensions,
                         names = c("l", "w", "h"),
                         delim = "x",
                         cols_remove = FALSE) |>
    relocate(present_dimensions, l, w, h) |>
    mutate(across(c(l, w, h), as.integer))

# Part 1 - Calculate wrapping paper needed ----
paper <- paper |>
    mutate(paper_needed = 2L * (l*w + w*h + h*l) + pmin(l*w, w*h, h*l))

total_paper_needed <- sum(paper$paper_needed)
cli_alert_success("Total paper needed: {total_paper_needed} sq ft")

# Part 2 - Calculate ribbon needed ----
paper <- paper |>
    mutate(ribbon_needed = 2L * pmin(l+w, l+h, w+h) + l*w*h)

total_ribbon_needed <- sum(paper$ribbon_needed)
cli_alert_success("Total ribbon needed: {total_ribbon_needed} ft")
