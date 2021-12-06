test_06 <- readLines("inputs/test_06.txt")
input_06 <- readLines("inputs/input_06.txt")

get_states <- function(input) {
  input |>
    strsplit(",") |>
    unlist() |>
    as.numeric() |>
    factor(levels = 0:8) |>
    table()
}

solve_06a <- function(input, days = 80) {
  states <- get_states(input)
  for(i in seq_len(days)) {
    new_states <- states
    new_states[1:8] <- states[2:9]
    new_states[7] <- new_states[7] + states[1]
    new_states[9] <- states[1]
    states <- new_states
  }
  sum(states)
}

solve_06a(test_06)
solve_06a(input_06)

solve_06b <- function(input) {
  states <- get_states(input)
  sum <- 0
  for(i in seq_along(states)) {
    sum <- sum + states[i] * solve_06a(names(states)[i], 256)
  }
  unname(sum)
}

solve_06b(test_06)
sprintf("%.0f",solve_06b(input_06))
