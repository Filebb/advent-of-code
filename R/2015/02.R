library(tidyverse)
library(cli)

input <- read_csv("inputs/2015/02.txt", col_names = "original")

paper <- input |>
    separate_wider_delim(cols = original,
                         names = c("l", "w", "h"),
                         delim = "x",
                         cols_remove = FALSE) |>
    relocate(original, l, w, h) |>
    mutate(across(c(l, w, h), as.integer)) |>
    mutate(paper_needed = 2L * (l*w + w*h + h*l) + pmin(l*w, w*h, h*l))

total_paper_needed <- sum(paper$paper_needed)
cli_alert_success("Total paper needed: {total_paper_needed} sq ft")

paper <- paper |>
    mutate(ribbon_needed = 2L * pmin(l+w, l+h, w+h) + l*w*h)

total_ribbon_needed <- sum(paper$ribbon_needed)
cli_alert_success("Total ribbon needed: {total_ribbon_needed} ft")
