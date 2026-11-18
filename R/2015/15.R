# Day 15: Science for Hungry People
# https://adventofcode.com/2015/day/15

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/15.txt")

# Pre-processing ----
ingredients <- tibble(raw = input) |>
  mutate(
    name = str_extract(raw, "^[A-Za-z]+"),
    nums = str_extract_all(raw, "-?\\d+")
  ) |>
  unnest_wider(nums, names_sep = "_") |>
  set_names(
    c("raw", "name", "capacity", "durability", "flavor", "texture", "calories")
  ) |>
  select(-raw) |>
  mutate(across(-name, as.integer))

ingredients_names <- ingredients |> pull(name)

# Convert property columns into a named list of vectors
feature_list <- ingredients |>
  select(-name) |>
  as.list()

# Helper function ----
calc_feature <- function(feature_name) {
  pmap_int(
    .l = pick(all_of(ingredients_names)),
    .f = \(...) max(0L, sum(c(...) * feature_list[[feature_name]]))
  )
}

# Part One ----
recipes_score <- expand_grid(0:100, 0:100, 0:100) |>
  filter(reduce(pick(everything()), `+`) <= 100) |>
  mutate(last_ingredient = 100 - reduce(pick(everything()), `+`)) |>
  set_names(ingredients_names) |>
  mutate(
    capacity   = calc_feature("capacity"),
    durability = calc_feature("durability"),
    flavor     = calc_feature("flavor"),
    texture    = calc_feature("texture"),
    calories   = calc_feature("calories"),
    score      = capacity * durability * flavor * texture
  )

max_score <- max(recipes_score$score)
cli_alert_success("Max Cookie Score: {max_score}")

# Part Two ----
recipes_score_500_calories <- recipes_score |>
  filter(calories == 500)

max_500_calories_score <- max(recipes_score_500_calories$score)
cli_alert_success("Max 500 Calories Cookie Score: {max_500_calories_score}")
