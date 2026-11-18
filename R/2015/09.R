# Day 9: All in a Single Night"
# https://adventofcode.com/2015/day/9

# Setup ----
library(tidyverse)
library(cli)
library(gtools)

# Load Data ----
input <- read_lines(file = "inputs/2015/09.txt")

# Parse Distances ----
distance_df <- tibble(original = input) |>
  separate_wider_delim(
    original, delim = " = ", names = c("trip", "distance")
  ) |>
  separate_wider_delim(
    trip, delim = " to ", names = c("from", "to")
  ) |>
  mutate(distance = as.integer(distance))

# Extract Unique Locations ----
locations <- unique(c(distance_df$from, distance_df$to))
n_loc <- length(locations)

# Build Distance Matrix ----
dist_matrix <- matrix(
  0,
  nrow = n_loc,
  ncol = n_loc,
  dimnames = list(locations, locations)
)

for (i in seq_len(nrow(distance_df))) {
  u <- distance_df$from[i]
  v <- distance_df$to[i]
  d <- distance_df$distance[i]
  dist_matrix[u, v] <- d
  dist_matrix[v, u] <- d
}

# Generate Permutations & Calculate Route Distances ----
routes <- permutations(n = n_loc, r = n_loc, v = locations)

# Vectorized route calculation: sum consecutive leg distances for each route
route_distances <- apply(routes, 1, \(route) {
  sum(dist_matrix[cbind(route[-n_loc], route[-1])])
})

# Part One ----
shortest_route_distance <- min(route_distances)
cli_alert_success("Shortest Route Distance: {shortest_route_distance}")

# Part Two ----
longest_route_distance <- max(route_distances)
cli_alert_success("Longest Route Distance: {longest_route_distance}")
