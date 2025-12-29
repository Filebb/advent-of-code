library(tidyverse)
library(cli)

input <- read_lines(file = "inputs/2015/01.txt")

moves <- tibble(
    input = str_split_1(input, ""),
    move = case_match(input, "(" ~  1L, ")" ~ -1L),
    height = cumsum(move)
)

part1 <- tail(moves$height, 1)

cli_alert_success("Final floor: {part1}")

basement_entry_position <- moves |>
    rowid_to_column("time") |>
    filter(height == -1)

part2 <- head(basement_entry_position$time, 1)

cli_alert_success("Basement position: {part2}")
