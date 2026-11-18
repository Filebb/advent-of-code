# Day 23: Opening the Turing Lock
# https://adventofcode.com/2015/day/23

# Setup ----
library(tidyverse)
library(cli)

# Load Data ----
input <- read_lines("inputs/2015/23.txt")

# Helper function to run program
run_program <- function(instructions, initial_a = 0) {
  registers <- c("a" = initial_a, "b" = 0)
  index <- 1

  while (index >= 1 && index <= length(instructions)) {
    line <- instructions[index]

    # Extract components
    instr  <- str_extract(line, "^[a-z]{3}")
    reg    <- str_extract(line, "[ab]")
    offset <- str_extract(line, "([+-]\\d+)$") |> as.integer()

    # 1. Math Instructions
    if (instr %in% c("hlf", "tpl", "inc")) {
      if (instr == "hlf") registers[reg] <- registers[reg] / 2
      if (instr == "tpl") registers[reg] <- registers[reg] * 3
      if (instr == "inc") registers[reg] <- registers[reg] + 1
      index <- index + 1
    }

    # 2. Jump Instruction (unconditional)
    else if (instr == "jmp") {
      index <- index + offset
    }

    # 3. Conditional Jump Instructions
    else if (instr %in% c("jie", "jio")) {
      condition_met <- FALSE
      if (instr == "jie") { # Jump if even
        condition_met <- (registers[reg] %% 2 == 0)
      } else if (instr == "jio") { # Jump if one
        condition_met <- (registers[reg] == 1)
      }

      if (condition_met) {
        index <- index + offset
      } else {
        index <- index + 1
      }
    }
  }

  return(registers)
}

# Part One (start with a = 0)
result_part1 <- run_program(input, initial_a = 0)
cli_alert_success("Part 1, register b: {.field {result_part1['b']}}")

# Part Two (start with a = 1)
result_part2 <- run_program(input, initial_a = 1)
cli_alert_success("Part 2, register b: {.field {result_part2['b']}}")
