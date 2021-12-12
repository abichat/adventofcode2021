test_11 <- readLines("inputs/test_11.txt")
input_11 <- readLines("inputs/input_11.txt")

get_grid <- function(input) {
  input |>
    strsplit("") |>
    unlist() |>
    as.numeric() |>
    matrix(byrow = TRUE, ncol = 10)
}

increase <- function(mat) {
  mat + 1
}

increase_0 <- function(cell) {
  if (cell != 0) {
    cell <- cell + 1
  }
  cell
}

update_flash <- function(mat, i, j) {
  if (i > 1) {
    mat[i - 1, j] <- increase_0(mat[i - 1, j])
    if (j > 1) {
      mat[i - 1, j - 1] <- increase_0(mat[i - 1, j - 1])
    }
    if (j < 10) {
      mat[i - 1, j + 1] <- increase_0(mat[i - 1, j + 1])
    }
  }
  if (i < 10) {
    mat[i + 1, j] <- increase_0(mat[i + 1, j])
    if (j > 1) {
      mat[i + 1, j - 1] <- increase_0(mat[i + 1, j - 1])
    }
    if (j < 10) {
      mat[i + 1, j + 1] <- increase_0(mat[i + 1, j + 1])
    }
  }
  if (j > 1) {
    mat[i, j - 1] <- increase_0(mat[i, j - 1])
  }
  if (j < 10) {
    mat[i, j + 1] <- increase_0(mat[i, j + 1])
  }
  mat[i, j] <- 0
  mat
}


flashes <- function(mat) {
  while (any(mat > 9)) {
    for (i in 1:10) {
      for (j in 1:10) {
        if (mat[i, j] > 9) {
          mat <- update_flash(mat, i, j)
        }
      }
    }
  }
  mat
}

day_11a <- function(input) {
  mat <- get_grid(input)
  flashes <- 0
  for (i in seq_len(100)) {
    mat <-
      mat |>
      increase() |>
      flashes()
    flashes <- flashes + sum(mat == 0)
  }
  flashes
}

day_11a(test_11)
day_11a(input_11)

day_11b <- function(input) {
  mat <- get_grid(input)
  step <- 0
  while (!all(mat == 0)) {
    mat <-
      mat |>
      increase() |>
      flashes()
    step <- step + 1
  }
  step
}

day_11b(test_11)
day_11b(input_11)
