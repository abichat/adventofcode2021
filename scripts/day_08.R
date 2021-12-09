test_08 <- readLines("inputs/test_08.txt")
input_08 <- readLines("inputs/input_08.txt")

get_digits <- function(input) {
  input |>
    strsplit(" \\| ") |>
    lapply(\(x) strsplit(x, " "))
}

day_08a <- function(input) {
  n_segments <-
    input |>
    get_digits() |>
    lapply(\(x) x[[2]]) |>
    lapply(nchar) |>
    unlist()

  sum(n_segments %in% c(2, 4, 3, 7))
}

day_08a(test_08)
day_08a(input_08)

reorder_digits <- function(digits) {
  digits |>
    strsplit("") |>
    unlist() |>
    sort() |>
    paste(collapse = "")
}

reorder_digits <- Vectorize(reorder_digits, USE.NAMES = FALSE)

split_segment <- function(str) {
  unlist(strsplit(str, ""))
}

contains_letters <- function(digit, letters) {
  all(sapply(letters, \(x) grepl(x, digit)))
}

find_1478 <- function(digits, code = rep(NA, 10)) {
  digits <- reorder_digits(digits)
  code[2] <- digits[nchar(digits) == 2]
  code[5] <- digits[nchar(digits) == 4]
  code[8] <- digits[nchar(digits) == 3]
  code[9] <- digits[nchar(digits) == 7]
  code
}

find_9 <- function(digits, code) {
  seg_069 <- digits[nchar(digits) == 6]
  seg_4 <- split_segment(code[5])
  code[10] <- Filter(\(x) contains_letters(x, seg_4), seg_069)
  code
}

find_3 <- function(digits, code) {
  seg_235 <- digits[nchar(digits) == 5]
  seg_1 <- split_segment(code[2])
  code[4] <- Filter(\(x) contains_letters(x, seg_1), seg_235)
  code
}

find_06 <- function(digits, code) {
  seg_069 <- digits[nchar(digits) == 6]
  seg_06 <- setdiff(seg_069, code[10])
  seg_1 <- split_segment(code[2])
  code[1] <- Filter(\(x) contains_letters(x, seg_1), seg_06)
  code[7] <- Filter(\(x) !contains_letters(x, seg_1), seg_06)
  code
}

find_C <- function(digits, code) {
  seg_8 <- split_segment(code[9])
  seg_6 <- split_segment(code[7])
  setdiff(seg_8, seg_6)
}

find_25 <- function(digits, code, letter_c) {
  seg_235 <- digits[nchar(digits) == 5]
  seg_25 <- setdiff(seg_235, code[4])
  code[3] <- Filter(\(x) contains_letters(x, letter_c), seg_25)
  code[6] <- Filter(\(x) !contains_letters(x, letter_c), seg_25)
  code
}

decode <- function(digits) {
  code <- find_1478(digits)
  code <- find_9(digits, code)
  code <- find_3(digits, code)
  code <- find_06(digits, code)
  letter_c <- find_C(digits, code)
  code <- find_25(digits, code, letter_c)
  code
}

read_code <- function(digits, code) {
  dict <- 0:9
  names(dict) <- reorder_digits(code)
  dict[reorder_digits(digits)] |>
    paste(collapse = "") |>
    as.numeric()
}

day_08b <- function(input) {
  sum <- 0
  digits <- get_digits(input)
  for (i in seq_along(digits)) {
    code <- decode(digits[[i]][[1]])
    sum <- sum + read_code(digits[[i]][[2]], code)
  }
  sum
}

day_08b(test_08)
day_08b(input_08)
