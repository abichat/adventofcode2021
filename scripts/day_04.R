test_04 <- readLines("inputs/test_04.txt")
input_04 <- readLines("inputs/input_04.txt")

get_numbers <- function(input) {
  input |>
    getElement(1) |>
    strsplit(",") |>
    unlist() |>
    as.numeric()
}

get_cards <- function(input) {
  input <- input[-1]
  n_cards <- length(input) / 6
  input |>
    split(rep(seq_len(n_cards), each = 6)) |>
    lapply(\(x) x[-1]) |>
    lapply(\(x) unlist(strsplit(x, " +"))) |>
    lapply(\(x) Filter(\(y) y != "", x)) |>
    lapply(\(x) matrix(x, ncol = 5, byrow = TRUE)) |>
    lapply(\(x) apply(x, 1:2, as.numeric))
}

day_04a <- function(input) {
  numbers <- get_numbers(input)
  cards <- get_cards(input)
  n_cards <- length(cards)

  for (i in seq_along(numbers)) {
    current_numbers <- numbers[seq_len(i)]
    for (j in seq_len(n_cards)) {
      bool <- apply(cards[[j]], 1:2, \(x) x %in% current_numbers)
      if (5 %in% c(colSums(bool), rowSums(bool))) {
        return(sum(cards[[j]][!bool]) * numbers[i])
      }
    }
  }
}

day_04a(test_04)
day_04a(input_04)

check_card <- function(mat, numbers) {
  bool <- apply(mat, 1:2, \(x) x %in% numbers)
  5 %in% c(colSums(bool), rowSums(bool))
}

day_04b <- function(input) {
  numbers <- get_numbers(input)
  cards <- get_cards(input)

  for (i in seq_along(numbers)) {
    current_numbers <- numbers[seq_len(i)]
    if (length(cards) > 1) {
      wins <- sapply(cards, \(x) check_card(x, current_numbers))
      cards <- cards[!wins]
    } else {
      if (check_card(cards[[1]], current_numbers)) {
        bool <- apply(cards[[1]], 1:2, \(x) x %in% current_numbers)
        return(sum(cards[[1]][!bool]) * numbers[i])
      }
    }
  }
}

day_04b(test_04)
day_04b(input_04)
