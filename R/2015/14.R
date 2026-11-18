# Day 14: Reindeer Olympics
# https://adventofcode.com/2015/day/14

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/14.txt")
total_time <- 2503L

# Pre-processing ----
reindeer <- tibble(raw = input) |>
  separate_wider_regex(
    raw,
    c(name = "[:alpha:]+", " can fly ",
      speed = "\\d+", " km/s for ",
      fly_time = "\\d+", " seconds, but then must rest for ",
      rest_time = "\\d+", " seconds.")
  ) |>
  mutate(across(-name, as.integer))

# Helper function ----
distance <- function(elapsed_time, speed, fly_time, rest_time) {
  cycle_time <- fly_time + rest_time
  full_cycles <- elapsed_time %/% cycle_time
  remaining_time <- elapsed_time %% cycle_time
  active_remaining <- pmin(remaining_time, fly_time)
  total_distance <- (full_cycles * fly_time + active_remaining) * speed

  return(total_distance)
}

# Part One ----
reindeer_distances <- reindeer |>
  mutate(total_distance = distance(total_time, speed, fly_time, rest_time))

max_distance <- max(reindeer_distances$total_distance)
cli_alert_success("Max Distance Covered: {max_distance}")

# Part Two ----
reindeer_timeline <- reindeer_distances |>
  mutate(
    timeline_distance = pmap(
      list(speed, fly_time, rest_time),
      \(speed, fly_time, rest_time) {
        distance(1:total_time, speed, fly_time, rest_time)
      }
    )
  )

lead_position <- reindeer_timeline |>
  pull(timeline_distance) |>
  simplify2array() |>
  apply(1, max)

reindeer_score <- reindeer_timeline |>
  mutate(score = map_int(timeline_distance, \(x) sum(x == lead_position)))

max_score <- max(reindeer_score$score)
cli_alert_success("Max Leader Points: {max_score}")

# Example ----
example <- c(
  "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.",
  "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."
)

example_preprocess <- tibble(raw = example) |>
  separate_wider_regex(
    raw,
    c(name = "[:alpha:]+", " can fly ",
      speed = "[:digit:]+", " km/s for ",
      fly_time = "[:digit:]+", " seconds, but then must rest for ",
      rest_time = "[:digit:]+", " seconds.")
  ) |>
  mutate(across(-name, as.integer))

# part one
example_preprocess |>
  mutate(total_distance = distance(1000L, speed, fly_time, rest_time))

# part two
example_timeline <- example_preprocess |>
  mutate(
    timeline_distance = pmap(
      list(speed, fly_time, rest_time),
      \(speed, fly_time, rest_time) {
        distance(1:1000, speed, fly_time, rest_time)
      }
    )
  )

example_lead_position <- example_timeline |>
  pull(timeline_distance) |>
  simplify2array() |>
  apply(1, max)

example_timeline |>
  mutate(
    score = map_int(timeline_distance, \(x) sum(x == example_lead_position))
  )
