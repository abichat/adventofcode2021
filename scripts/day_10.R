test_10 <- readLines("inputs/test_10.txt")
input_10 <- readLines("inputs/input_10.txt")

remove_void_chunks <- function(str) {
  cond <- TRUE
  while (cond) {
    new_str <- gsub("(\\(\\)|\\[\\]|\\{\\}|<>)", "", str)
    if (all(new_str == str)) cond <- FALSE
    str <- new_str
  }
  str
}

remove_openings <- function(str) {
  cond <- TRUE
  while (cond) {
    new_str <- gsub("(^\\(|^\\[|^\\{|^<)", "", str)
    if (all(new_str == str)) cond <- FALSE
    str <- new_str
  }
  str
}

day_10a <- function(input) {
  incorrect_closings <-
    input |>
    remove_void_chunks() |>
    remove_openings() |>
    strsplit("") |>
    sapply(\(x) x[1]) |>
    Filter(f = \(x) !is.na(x))

  points <- c(")" = 3, "]" = 57, "}" = 1197, ">" = 25137)

  sum(points[incorrect_closings])
}

day_10a(test_10)
day_10a(input_10)

filter_incomplete <- function(str) {
  str_removed <-
    str |>
    remove_void_chunks() |>
    remove_openings()
  str[which(str_removed == "")]
}

compute_score <- function(str) {
  score <- 0
  points <- c(")" = 1, "]" = 2, "}" = 3, ">" = 4)
  for (i in seq_along(str)) {
    score <- score * 5 + points[str[i]]
  }
  score
}

day_10b <- function(input) {
  completion_strings <-
    input |>
    filter_incomplete() |>
    remove_void_chunks() |>
    gsub(pattern = "\\(", replacement = ")") |>
    gsub(pattern = "\\[", replacement = "]") |>
    gsub(pattern = "\\{", replacement = "}") |>
    gsub(pattern = "<", replacement = ">") |>
    strsplit("") |>
    lapply(rev)

  completion_strings |>
    sapply(compute_score) |>
    median()
}

day_10b(test_10)
day_10b(input_10)

