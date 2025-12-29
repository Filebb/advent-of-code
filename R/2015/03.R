library(tidyverse)
library(cli)

input <- read_lines("inputs/2015/03.txt") |>
    str_split_1("")

moves <- tibble(input) |>
    mutate(
        move_x = case_match(input, "^" ~ 0, ">" ~ 1, "v" ~ 0, "<" ~ -1),
        move_y = case_match(input, "^" ~ 1, ">" ~ 0, "v" ~ -1, "<" ~ 0),
        pos_x = cumsum(move_x),
        pos_y = cumsum(move_y)
    ) |>
    add_row(pos_x = 0, pos_y = 0, .before = 1)

houses_visited_p1 <- moves |>
    distinct(pos_x, pos_y) |>
    nrow()

cli_alert_success("Houses visited: {houses_visited_p1}")

moves <- tibble(input) |>
    mutate(
        move_x = case_match(input, "^" ~ 0, ">" ~ 1, "v" ~ 0, "<" ~ -1),
        move_y = case_match(input, "^" ~ 1, ">" ~ 0, "v" ~ -1, "<" ~ 0),
        who = if_else(row_number() %% 2 == 1, "santa", "robo")
    ) |>
    mutate(
        pos_x = cumsum(move_x),
        pos_y = cumsum(move_y),
        .by = who
    ) |>
    add_row(pos_x = 0, pos_y = 0, .before = 1)

houses_visited_p2 <- moves |>
    distinct(pos_x, pos_y) |>
    nrow()

cli_alert_success("Houses visited: {houses_visited_p2}")
