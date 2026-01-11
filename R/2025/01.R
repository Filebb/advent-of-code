# Day 1: Secret Entrance
# https://adventofcode.com/2025/day/1
#
# Circular dial: 100 positions (0-99), starting at 50
# R = clockwise, L = counter-clockwise

# Setup ----
library(tidyverse)
library(cli)

TRACK_SIZE <- 100L
START_POSITION <- 50L

# Load Data ----
input <- read_lines("inputs/2025/01.txt")

# Parse and calculate positions ----
moves <- tibble(input) |>
    mutate(
        direction = str_extract(input, "(L|R)"),
        distance = as.integer(str_extract(input, "\\d+")),
        distance_signed = if_else(direction == "R", distance, -distance),
        start_pos = (
            START_POSITION + c(0L, cumsum(distance_signed[-n()]))
        ) %% TRACK_SIZE,
        end_pos = (start_pos + distance_signed) %% TRACK_SIZE
    )

# Part 1 - Moves that land on position 0 ----
lands_on_0 <- sum(moves$end_pos == 0)
cli_alert_success("Times landed on position 0: {lands_on_0}")

# Part 2 - Count moves that pass through position 0 ----
#
# Key insight: Unwrap the circular track to a number line
# Position 0 = multiples of 100 (0, 100, 200, ...)
# Crossing 0 = crossing a multiple of 100
#
# Method: Use floor division to count crossings
#   floor(x/100) tells which "century" position x is in
#   Difference between end and start = number of crossings
#
# IMPORTANT: Why we exclude certain boundaries
#
# For RIGHT moves: interval is (start, start+distance]
#   - Exclude LEFT boundary (start): We're already AT start position
#     before moving, so we don't "cross" it - we START there
#   - Include RIGHT boundary (end): We DO cross/reach the end position
#   - Example: At position 0, move R5 → crosses 0? NO (we started there)
#   - Example: At position 95, move R5 → crosses 0? YES (we end there)
#
# For LEFT moves: interval is [start+distance, start)
#   - Include LEFT boundary (end): We DO cross/reach the end position
#   - Exclude RIGHT boundary (start): We're already AT start position
#     before moving, so we don't "cross" it - we START there
#   - Example: At position 0, move L5 → crosses 0? NO (we started there)
#   - Example: At position 5, move L5 → crosses 0? YES (we end there)
#
# The -1 adjustments handle this mathematically:
#   - Right: floor((start+dist)/100) - floor(start/100)
#     Naturally excludes start, includes end
#   - Left: floor((start-1)/100) - floor((start+dist-1)/100)
#     The -1 shifts boundaries to exclude start, include end

moves <- moves |>
    mutate(
        crosses = if_else(
            distance_signed > 0,

            # Right: Interval (start, start+distance]
            # Excludes start (already there), includes end (arrive there)
            # Ex: Position 0, R100 → 0 to 100
            #     floor(100/100) - floor(0/100) = 1 - 0 = 1
            #     Correctly counts crossing at END (position 0/100)
            # Ex: Position 0, R50 → 0 to 50
            #     floor(50/100) - floor(0/100) = 0 - 0 = 0
            #     Correctly doesn't count start as crossing
            pmax(0L,
                 floor((start_pos + distance_signed) / TRACK_SIZE) -
                     floor(start_pos / TRACK_SIZE)
            ),

            # Left: Interval [start+distance, start)
            # Includes end (arrive there), excludes start (already there)
            # The -1 adjusts boundaries to match this logic
            # Ex: Position 0, L100 → 0 to -100
            #     floor(-1/100) - floor(-101/100) = -1 - (-2) = 1
            #     Correctly counts crossing at END (position 0/-100)
            # Ex: Position 50, L50 → 50 to 0
            #     floor(49/100) - floor(-1/100) = 0 - (-1) = 1
            #     Correctly counts arriving at 0
            # Ex: Position 100, L50 → 100 to 50
            #     floor(99/100) - floor(49/100) = 0 - 0 = 0
            #     Correctly doesn't count start (position 100/0)
            pmax(0L,
                 floor((start_pos - 1) / TRACK_SIZE) -
                     floor((start_pos + distance_signed - 1) / TRACK_SIZE)
            )
        )
    )

crosses_0 <- sum(moves$crosses)
cli_alert_success("Times dial points at 0: {crosses_0}")
