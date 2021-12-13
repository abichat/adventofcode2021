test_13 <- readLines("inputs/test_13.txt")
input_13 <- readLines("inputs/input_13.txt")

get_dots <- function(input) {
  coords <-
    input |>
    Filter(f = \(x) !grepl("fold", x)) |>
    Filter(f = \(x) x!= "") |>
    strsplit(",") |>
    unlist() |>
    as.numeric() |>
    matrix(ncol = 2, byrow = TRUE)
  mat <- matrix(0, nrow = max(coords[, 2]) + 1, ncol = max(coords[, 1] + 1))
  for (i in seq_len(nrow(coords))) {
    mat[coords[i, 2] + 1, coords[i, 1] + 1] <- 1
  }
  mat
}

get_folds <- function(input) {
  input |>
    Filter(f = \(x) grepl("fold", x)) |>
    sapply(\(x) gsub("fold along ", "", x)) |>
    unname() |>
    strsplit("=") |>
    lapply(\(x) list(direction = x[1], line = as.numeric(x[2])))
}

day_13a <- function(input) {
  dots <- get_dots(input)
  fold <- get_folds(input)[[1]]

  if (fold$direction == "x") {
    nc <- ncol(dots)
    dots <- dots[, 1:fold$line] + dots[, nc:(fold$line + 2)]
  } else {
    nr <- nrow(dots)
    dots <- dots[1:fold$line, ] + dots[nr:(fold$line + 2), ]
  }

  sum(dots > 0)
}

day_13a(test_13)
day_13a(input_13)

day_13b <- function(input) {
  dots <- get_dots(input)
  folds <- get_folds(input)
  for (fold in folds) {
    if (fold$direction == "x") {
      nc <- ncol(dots)
      dots <- dots[, 1:fold$line] + dots[, nc:(fold$line + 2)]
    } else {
      nr <- nrow(dots)
      dots <- dots[1:fold$line,] + dots[nr:(fold$line + 2),]
    }
  }
  nc <- ncol(dots)
  length_letters <- nc / 8
  letters <- vector(mode = "list", length = 8)
  for (i in seq_len(8)) {
    letters[[i]] <- dots[, (length_letters * (i - 1)):(length_letters * i)]
  }
  letters
}

day_13b(input_13)
