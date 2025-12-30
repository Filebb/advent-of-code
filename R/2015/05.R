# Day 5: Doesn't He Have Intern-Elves For This?
# https://adventofcode.com/2015/day/5

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/05.txt")

# Part 1 - Check strings with old rules ----
nice <- tibble(string = input) |>
    mutate(
        three_more_vowels = str_count(string, "[aeiou]") >= 3,
        one_more_double = str_detect(string, "([a-z])\\1"),
        not_ab_cd_pq_xy = !str_detect(string, "ab|cd|pq|xy"),
        nice = three_more_vowels & one_more_double & not_ab_cd_pq_xy
    )

total_nice_strings <- sum(nice$nice)
cli_alert_success("Total nice strings (old rules): {total_nice_strings}")

# Part 2 - Check strings with new rules ----
nice_new <- tibble(string = input) |>
    mutate(
        repeating_pair = str_detect(string, "([a-z]{2}).*\\1"),
        repeat_with_gap = str_detect(string, "([a-z]).\\1"),
        nice_new = repeating_pair & repeat_with_gap
    )

total_nice_strings_new <- sum(nice_new$nice_new)
cli_alert_success("Total nice strings (new rules): {total_nice_strings_new}")
