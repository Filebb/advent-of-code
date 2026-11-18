# Day 13: Knights of the Dinner Table
# https://adventofcode.com/2015/day/13

# Setup ----
library(tidyverse)
library(cli)
library(gtools)

input <- read_lines("inputs/2015/13.txt")

weights <- tibble(raw = input) |>
  transmute(
    subject   = str_extract(raw, "^[A-Za-z]+"),
    sign      = str_extract(raw, "(gain|lose)"),
    value     = as.integer(str_extract(raw, "\\d+")),
    object    = str_extract(raw, "[A-Za-z]+(?=\\.)"),
    happiness = if_else(sign == "gain", value, -value)
  )

attendees <- unique(weights$subject)

# Create Fast Matrix Lookup Table ----
# (Rows = Subject, Cols = Object)
happiness_matrix <- weights |>
  select(subject, object, happiness) |>
  pivot_wider(names_from = object, values_from = happiness, values_fill = 0) |>
  column_to_rownames("subject") |>
  as.matrix()

# Vectorized Happiness Calculator ----
calc_total_happiness <- function(perm) {
  n <- length(perm)
  left_neighbors  <- perm[c(n, 1:(n - 1))]
  right_neighbors <- perm[c(2:n, 1)]

  # Fast matrix lookup using row-column index vectors
  sum(
    happiness_matrix[cbind(perm, left_neighbors)] +
      happiness_matrix[cbind(perm, right_neighbors)]
  )
}
# Solution Generator Function ----
solve_seating <- function(attendee_list) {
  perms <- gtools::permutations(length(attendee_list), length(attendee_list), attendee_list)

  # Evaluate all permutations via vectorized apply
  apply(perms, 1, calc_total_happiness) |> max()
}

# Part One ----
max_p1 <- solve_seating(attendees)
cli_alert_success("Part 1 - Max Seating Happiness: {max_p1}")

# Part Two: Add yourself with 0 happiness to everyone ----
attendees_p2 <- c(attendees, "Me")

# Extend lookup matrix for "Me"
happiness_matrix <- rbind(happiness_matrix, Me = 0)
happiness_matrix <- cbind(happiness_matrix, Me = 0)

max_p2 <- solve_seating(attendees_p2)
cli_alert_success("Part 2 - Max Seating Happiness (with Me): {max_p2}")
