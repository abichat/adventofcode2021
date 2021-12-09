test_09 <- readLines("inputs/test_09.txt")
input_09 <- readLines("inputs/input_09.txt")

get_heights <- function(input) {
  nr <- length(input)
  input |>
    strsplit("") |>
    unlist() |>
    as.numeric() |>
    matrix(nrow = nr, byrow = TRUE)
}

is_low_point <- function(mat, i, j) {
  nr <- nrow(mat)
  nc <- ncol(mat)

  if (i > 1 && mat[i, j] >= mat[i - 1, j]) {
    return(FALSE)
  }
  if (j > 1 && mat[i, j] >= mat[i, j - 1]) {
    return(FALSE)
  }
  if (i < nr && mat[i, j] >= mat[i + 1, j]) {
    return(FALSE)
  }
  if (j < nc && mat[i, j] >= mat[i, j + 1]) {
    return(FALSE)
  }

  return(TRUE)
}

day_09a <- function(input) {
  mat <- get_heights(input)
  s <- 0
  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      if (is_low_point(mat, i, j)) {
        s <- s + 1 + mat[i, j]
      }
    }
  }
  s
}

day_09a(test_09)
day_09a(input_09)

day_09b <- function(input) {
  mat <- get_heights(input)
  nr <- nrow(mat)
  nc <- ncol(mat)
  mat_b <- mat
  n_low_point <- 1

  for (i in seq_len(nrow(mat))) {
    for (j in seq_len(ncol(mat))) {
      if (mat[i,j] == 9) {
        mat_b[i, j] <- -1
      } else if (is_low_point(mat, i, j)) {
        mat_b[i, j] <- n_low_point
        n_low_point <- n_low_point + 1
      } else {
        mat_b[i, j] <- NA
      }
    }
  }

  while (sum(is.na(mat_b)) > 0) {
    for (i in seq_len(nrow(mat))) {
      for (j in seq_len(ncol(mat))) {
        if (is.na(mat_b[i,j])) {
          if (i > 1 && !is.na(mat_b[i - 1, j]) && mat_b[i - 1, j] != -1) {
            mat_b[i, j] <- mat_b[i - 1, j]
          } else if (j > 1 && !is.na(mat_b[i, j - 1]) && mat_b[i, j - 1] != -1) {
            mat_b[i, j] <- mat_b[i, j - 1]
          } else if (i < nr && !is.na(mat_b[i + 1, j]) && mat_b[i + 1, j] != -1) {
            mat_b[i, j] <- mat_b[i + 1, j]
          } else if (j < nc && !is.na(mat_b[i, j + 1]) && mat_b[i, j + 1] != -1) {
            mat_b[i, j] <- mat_b[i, j + 1]
          }
        }
      }
    }
  }

  tab <- table(mat_b)[-1]
  prod(rev(sort(tab))[1:3])
}

day_09b(test_09)
day_09b(input_09)
