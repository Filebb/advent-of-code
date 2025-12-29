library(tidyverse)
library(cli)

input <- read_lines(file = "inputs/2015/01.txt")

moves <- tibble(
    input = str_split_1(input, ""),
    move = case_match(input, "(" ~  1L, ")" ~ -1L),
    height = cumsum(move)
)

final_floor <- tail(moves$height, 1)
cli_alert_success("Final floor: {final_floor}")

basement_entries <- moves |>
    rowid_to_column("time") |>
    filter(height == -1)

basement_entry_position <- head(basement_entries$time, 1)
cli_alert_success("Basement position: {basement_entry_position}")
