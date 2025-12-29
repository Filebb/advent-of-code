library(tidyverse)
library(cli)
library(digest)

# example 1
digest(str_c("abcdef", 609043), algo = "md5", serialize = FALSE)

# example 2
digest(str_c("pqrstuv", 1048970), algo = "md5", serialize = FALSE)

# load input
input <- read_lines("inputs/2015/04.txt")

n <- 1040000 # Empirically sufficient to find the answers (also for part 2)

md5_hashes <- tibble(
    number = 1:n,
    string = str_c(input, number),
    hash = map_chr(string, \(x) digest(x, algo = "md5", serialize = FALSE))
)

lowest_number_5zeros <- md5_hashes |>
    filter(str_starts(hash, "00000")) |>
    slice(1) |>
    pull(number)

cli_alert_success("Lowest number with 5 leading zeros: {lowest_number_5zeros}")

lowest_number_6zeros <- md5_hashes |>
    filter(str_starts(hash, "000000")) |>
    slice(1) |>
    pull(number)

cli_alert_success("Lowest number with 6 leading zeros: {lowest_number_6zeros}")
