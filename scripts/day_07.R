test_07 <- readLines("inputs/test_07.txt")
input_07 <- readLines("inputs/input_07.txt")


day_07a <- function(input) {
  input <-
    input |>
    strsplit(",") |>
    unlist() |>
    as.numeric()

  fuel <- rep(NA, max(input))
  for (i in seq_along(fuel)) {
    fuel[i] <- sum(abs(input - i))
  }

  min(fuel)
}

day_07a(test_07)
day_07a(input_07)

day_07b <- function(input) {
  input <-
    input |>
    strsplit(",") |>
    unlist() |>
    as.numeric()

  fuel <- rep(NA, max(input))
  for (i in seq_along(fuel)) {
    fuel[i] <- sum(abs(input - i) * (abs(input - i) + 1) / 2)
  }

  min(fuel)
}

day_07b(test_07)
day_07b(input_07)
