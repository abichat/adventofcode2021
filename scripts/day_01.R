test_01 <- c(199, 200, 208, 210, 200, 207, 240, 269, 260, 263)
input_01 <- as.numeric(readLines("inputs/input_01.txt"))


solve_01a <- function(input) {
  sum(diff(input) > 0, na.rm = TRUE)
}

solve_01a(test_01)
solve_01a(input_01)


solve_01b <- function(input) {
  l1 <- split(input, (seq_along(input) - 1) %/% 3)
  l2 <- split(input, (seq_along(input) - 2) %/% 3)
  l3 <- split(input, (seq_along(input) - 3) %/% 3)

  l1 <- l1[sapply(l1, length) == 3]
  l2 <- l2[sapply(l2, length) == 3]
  l3 <- l3[sapply(l3, length) == 3]

  s1 <- sapply(l1, sum)
  s2 <- sapply(l2, sum)
  s3 <- sapply(l3, sum)

  seq <- c(s1, s2, s3)
  seq <- c(seq, rep(NA, (3 - (length(seq) %% 3)) %% 3))

  mat <- matrix(seq, nrow = 3, byrow = TRUE)
  vec <- as.vector(mat)

  solve_01a(vec)
}

solve_01b(test_01)
solve_01b(input_01)
