# Day 19: Medicine for Rudolph
# https://adventofcode.com/2015/day/19

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/19.txt")
input

replacements <- head(input, -2)
medicine_molecule <- tail(input, 1)

# Part One ----
new_molecules <- tibble(replacements) |>
  separate_wider_delim(replacements, delim = " => ",names = c("from", "to")) |>
  mutate(
    locations = map(
      .x = from,
      .f = \(x) str_locate_all(medicine_molecule, fixed(x))[[1]] |> as_tibble()
    )
  ) |>
  unnest(locations) |>
  mutate(
    new_molecule = str_c(
      str_sub(medicine_molecule, 1, start - 1),
      to,
      str_sub(medicine_molecule, end + 1, -1)
    )
  )

new_molecules_distinct <- n_distinct(new_molecules$new_molecule)
cli::cli_alert_success("Possible New Molecules: {new_molecules_distinct}")
