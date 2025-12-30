# Day 12: JSAbacusFramework.io
# https://adventofcode.com/2015/day/12

# Setup ----
library(tidyverse)
library(cli)
library(jsonlite)

# Part 1 - Sum all numbers ----
total_sum_all <- read_lines("inputs/2015/12.txt") |>
    str_extract_all("-?\\d+") |>
    unlist() |>
    as.integer() |>
    sum()

cli_alert_success("Sum of all numbers: {total_sum_all}")

# Part 2 - Sum excluding objects with 'red' ----
## IMPORTANT:
## simplifyVector = FALSE prevents jsonlite from automatically converting
## JSON objects or arrays into data frames or atomic vectors.
##
## Advent of Code Day 12 requires preserving the *exact* JSON structure:
##   - JSON objects must remain named lists
##   - JSON arrays must remain unnamed lists
##   - Mixed-type arrays must NOT be simplified
##
## If simplifyVector = TRUE (the default), jsonlite will:
##   • turn some objects into data frames
##   • turn arrays of numbers into numeric vectors
##   • turn arrays of strings into character vectors
##
## These simplifications BREAK the recursion logic, because:
##   • data frames are not JSON objects
##   • numeric vectors lose their list structure
##   • character vectors lose their element boundaries
##
## With simplifyVector = FALSE, the JSON tree is preserved faithfully,
## and the recursive walker behaves exactly as intended.
input <- fromJSON("inputs/2015/12.txt", simplifyVector = FALSE)

sum_json <- function(x) {
    # Case 1: numeric
    if (is.numeric(x)) {
        return(sum(x))
    }

    # Case 2: character or NULL
    if (is.character(x) || is.null(x)) {
        return(0)
    }

    # Case 3: list (array or object)
    if (is.list(x)) {
        # Check if it's an object (has names)
        if (!is.null(names(x))) {
            # Object rule: skip if any value is "red"
            has_red <- x |>
                keep(is.character) |>
                map_lgl(~ any(.x == "red")) |>
                any()

            if (any(has_red)) {
                return(0)
            }
        }

        # Sum recursively (works for both arrays and objects)
        return(x |> map_dbl(sum_json) |> sum())
    }

    # Fallback
    return(0)
}

total_sum_no_red <- sum_json(input)
cli_alert_success("Sum excluding red objects: {total_sum_no_red}")

# Examples (for verification) ----
fromJSON('[1,2,3]', simplifyVector = FALSE) |> sum_json()
fromJSON('[1,{"c":"red","b":2},3]', simplifyVector = FALSE) |> sum_json()
fromJSON('{"d":"red","e":[1,2,3,4],"f":5}', simplifyVector = FALSE) |> sum_json()
fromJSON('[1,"red",5]', simplifyVector = FALSE) |> sum_json()
