# Day 7: Some Assembly Required
# https://adventofcode.com/2015/day/7

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines(file = "inputs/2015/07.txt")

# Bitwise Operations ----
`%AND%`    <- bitwAnd
`%OR%`     <- bitwOr
`%NOT%`    <- function(dummy, x) bitwAnd(bitwNot(x), 0xFFFF)
`%RSHIFT%` <- bitwShiftR
`%LSHIFT%` <- function(x, y) bitwAnd(bitwShiftL(x, y), 0xFFFF)

# Solver function ----
solve_circuit <- function(instructions_df) {
  lhs <- instructions_df |> pull(lhs)
  rhs <- instructions_df |> pull(rhs)

  # Track which wires are completely resolved to a raw number
  solved <- str_detect(lhs, "^[0-9]+$")

  repeat {
    if (all(solved)) break

    # 1. Grab resolved numbers to use as replacements
    newly_solved <- solved & !is.na(solved)

    if (any(newly_solved)) {
      replacement <- str_extract(lhs[newly_solved], "[0-9]+")
      names(replacement) <- paste0("(?<=^| )", rhs[newly_solved], "(?=$| )")

      # Keep unique replacement patterns to avoid redundant regex operations
      replacement <- replacement[!duplicated(names(replacement))]

      # 2. Only apply string replacement to UNRESOLVED wires
      unsolved_idx <- which(!solved)

      if (length(replacement) > 0 && length(unsolved_idx) > 0) {
        lhs[unsolved_idx] <- str_replace_all(lhs[unsolved_idx], replacement)
      }
    }

    # 3. Find unresolved wires that are now ready to evaluate (no lowercase left)
    unsolved_idx <- which(!solved)
    ready_to_eval <- unsolved_idx[!str_detect(lhs[unsolved_idx], "[a-z]+")]

    # 4. Evaluate only those ready expressions
    if (length(ready_to_eval) > 0) {
      lhs[ready_to_eval] <- map_chr(
        lhs[ready_to_eval],
        ~ as.character(eval(parse(text = .x)))
      )
      # Mark them as solved
      solved[ready_to_eval] <- TRUE
    }
  }

  setNames(as.numeric(lhs), rhs)
}

# Prepare Base Instructions ----
base_instructions <- tibble(original = input) |>
  separate_wider_delim(
    cols = original,
    delim = " -> ",
    names = c("lhs", "rhs"),
    cols_remove = FALSE
  ) |>
  mutate(
    lhs = str_replace_all(
      lhs, c(
        "AND"    = "%AND%",
        "OR"     = "%OR%",
        "NOT"    = "1 %NOT%",
        "RSHIFT" = "%RSHIFT%",
        "LSHIFT" = "%LSHIFT%"
      )
    )
  )

# Part One ----
results_p1 <- solve_circuit(base_instructions)
signal_a <- results_p1["a"]
cli_alert_success("Signal on wire a: {signal_a}")

# Part Two ----
instructions_p2 <- base_instructions |>
  mutate(lhs = if_else(rhs == "b", as.character(signal_a), lhs))

results_p2 <- solve_circuit(instructions_p2)
signal_a_new <- results_p2["a"]
cli_alert_success("Signal on wire a (override b): {signal_a_new}")
