test_05 <- readLines("inputs/test_05.txt")
input_05 <- readLines("inputs/input_05.txt")

get_positions <- function(input) {
  input |>
    strsplit("(,| -> )") |>
    lapply(as.numeric)
}

create_card <- function(position, size) {
  mat <- matrix(0, nrow = size + 1, ncol = size + 1)
  if (position[1] == position[3]) {
    line <- sort(position[c(2, 4)]) + 1
    mat[seq(line[1], line[2]), position[1] + 1] <- 1
  } else {
    line <- sort(position[c(1, 3)]) + 1
    mat[position[2] + 1, seq(line[1], line[2])] <- 1
  }
  mat
}

day_05a <- function(input) {
  positions <- get_positions(input)
  size <- max(unlist(positions))
  positions <- Filter(\(x) x[1] == x[3] || x[2] == x[4],
                      positions)
  mat <- matrix(0, nrow = size + 1, ncol = size + 1)
  for (i in seq_along(positions)) {
    mat <- mat + create_card(positions[[i]], size)
  }
  sum(mat > 1)
}

day_05a(test_05)
day_05a(input_05)

create_card2 <- function(position, size) {
  mat <- matrix(0, nrow = size + 1, ncol = size + 1)
  if (position[1] == position[3]) {
    line <- sort(position[c(2, 4)]) + 1
    mat[seq(line[1], line[2]), position[1] + 1] <- 1
  } else if (position[2] == position[4]) {
    line <- sort(position[c(1, 3)]) + 1
    mat[position[2] + 1, seq(line[1], line[2])] <- 1
  } else {
    line1 <- seq(position[1], position[3]) + 1
    line2 <- seq(position[2], position[4]) + 1
    for (i in seq_along(line1)) {
      mat[line2[i], line1[i]] <- 1
    }
  }
  mat
}

day_05b <- function(input) {
  positions <- get_positions(input)
  size <- max(unlist(positions))
  mat <- matrix(0, nrow = size + 1, ncol = size + 1)
  for (i in seq_along(positions)) {
    mat <- mat + create_card2(positions[[i]], size)
  }
  sum(mat > 1)
}

day_05b(test_05)
day_05b(input_05)
