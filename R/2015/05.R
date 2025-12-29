library(tidyverse)
library(cli)

input <- read_csv("inputs/2015/05.txt", col_names = "original")

nice <- input |>
    mutate(
        three_more_vowels = str_count(original, "[aeiou]") >= 3,
        one_more_double = str_detect(original, "([a-z])\\1"),
        not_ab_cd_pq_xy = !str_detect(original, "ab|cd|pq|xy"),
        nice = three_more_vowels & one_more_double & not_ab_cd_pq_xy
    )

total_nice_strings <- sum(nice$nice)
cli_alert_success("Total nice strings (old rules): {total_nice_strings}")

nice_new <- input |>
    mutate(
        repeating_pair = str_detect(original, "([a-z]{2}).*\\1"),
        repeat_with_gap = str_detect(original, "([a-z]).\\1"),
        nice_new = repeating_pair & repeat_with_gap
    )

total_nice_strings_new <- sum(nice_new$nice_new)
cli_alert_success("Total nice strings (new rules): {total_nice_strings_new}")
