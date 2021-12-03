test_02 <- readLines("inputs/test_02.txt")
input_02 <- readLines("inputs/input_02.txt")

solve_02a <- function(input) {
  direction <- gsub(" [0-9]+", "", input)
  length <- as.numeric(gsub("[^0-9]", "", input))
  list <- split(length, direction)
  list <- lapply(list, sum)
  list$forward * (list$down - list$up)
}

solve_02a(test_02)
solve_02a(input_02)

solve_02b <- function(input) {
  direction <- gsub(" [0-9]+", "", input)
  length <- as.numeric(gsub("[^0-9]", "", input))

  position_v <- 0
  position_h <- 0
  position_aim <- 0

  for (i in seq_along(direction)) {
    if (direction[i] == "forward") {
      position_h <- position_h + length[i]
      position_v <- position_v + position_aim * length[i]
    } else if (direction[i] == "down") {
      position_aim <- position_aim  + length[i]
    } else if (direction[i] == "up") {
      position_aim <- position_aim  - length[i]
    } else {
      stop("Unrecognised direction")
    }
  }

  position_h * position_v
}

solve_02b(test_02)
solve_02b(input_02)
