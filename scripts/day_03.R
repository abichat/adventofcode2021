test_03 <- readLines("inputs/test_03.txt")
input_03 <- readLines("inputs/input_03.txt")

solve_03a <- function(input) {

  n <- length(input)
  bits_by_order <-
    input |>
    strsplit("") |>
    unlist() |>
    matrix(nrow = n, byrow = TRUE) |>
    as.data.frame() |>
    as.list() |>
    lapply(table) |>
    lapply(sort) |>
    lapply(names)

  gamma <-
    bits_by_order |>
    lapply(\(x) x[2]) |>
    paste0(collapse = "") |>
    as.numeric() |>
    strtoi(base = 2)

  epsilon <-
    bits_by_order |>
    lapply(\(x) x[1]) |>
    paste0(collapse = "") |>
    as.numeric() |>
    strtoi(base = 2)

  gamma * epsilon
}

solve_03a(test_03)
solve_03a(input_03)

solve_03b <- function(input) {

  n <- length(input)
  bits_by_row <-
    input |>
    strsplit("")

  bits_by_column <-
    input |>
    strsplit("") |>
    unlist() |>
    matrix(nrow = n, byrow = TRUE) |>
    as.data.frame() |>
    as.list()

  # oxygen
  bits_by_row_i <- bits_by_row
  bits_by_column_i <- bits_by_column

  i <- 1

  while(length(bits_by_row_i) > 1) {
    count_bit_column_i <-
      bits_by_column_i |>
      getElement(i) |>
      table() |>
      sort()

    bit_criteria_i <-
      ifelse(count_bit_column_i[1] == count_bit_column_i[2],
             "1", names(count_bit_column_i[2])) |>
      unname()

    position_bit_criteria_i <-
      which(sapply(bits_by_row_i, \(x) x[i]) == bit_criteria_i)

    bits_by_row_i <- bits_by_row_i[position_bit_criteria_i]
    bits_by_column_i <- lapply(bits_by_column_i, \(x) x[position_bit_criteria_i])
    i <- i + 1
  }


  oxygen_generator_rating <-
    bits_by_row_i |>
    getElement(1) |>
    paste0(collapse = "") |>
    as.numeric() |>
    strtoi(base = 2)

  # CO2
  bits_by_row_i <- bits_by_row
  bits_by_column_i <- bits_by_column

  i <- 1

  while(length(bits_by_row_i) > 1) {
    count_bit_column_i <-
      bits_by_column_i |>
      getElement(i) |>
      table() |>
      sort()

    bit_criteria_i <-
      ifelse(count_bit_column_i[1] == count_bit_column_i[2],
             "0", names(count_bit_column_i[1])) |>
      unname()

    position_bit_criteria_i <-
      which(sapply(bits_by_row_i, \(x) x[i]) == bit_criteria_i)

    bits_by_row_i <- bits_by_row_i[position_bit_criteria_i]
    bits_by_column_i <- lapply(bits_by_column_i, \(x) x[position_bit_criteria_i])
    i <- i + 1
  }

  co2_scrubber_rating <-
    bits_by_row_i |>
    getElement(1) |>
    paste0(collapse = "") |>
    as.numeric() |>
    strtoi(base = 2)

  oxygen_generator_rating * co2_scrubber_rating

}

solve_03b(test_03)
solve_03b(input_03)
