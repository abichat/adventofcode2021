test_12 <- readLines("inputs/test_12.txt")
input_12 <- readLines("inputs/input_12.txt")

get_edges <- function(input) {
  edges <-
    input |>
    strsplit("-") |>
    unlist() |>
    matrix(ncol = 2, byrow = TRUE)

  edges <- as.data.frame(rbind(edges, edges[, 2:1]))
  edges <- edges[edges$V1 != "end", ]
  edges <- edges[edges$V2 != "start", ]
  edges
}

get_neighbors <- function(position, edges) {
  edges[edges$V1 == position, "V2"]
}

filter_neighbors_a <- function(ngb, path) {
  ngb_low <- ngb[ngb == tolower(ngb)]
  setdiff(ngb, intersect(ngb_low, path))
}

complete_path_a <- function(path, edges) {
  last <- path[length(path)]
  if (last == "end") {
    return(list(path))
  } else {
    ngb <- get_neighbors(last, edges)
    next_steps <- filter_neighbors_a(ngb, path)
    ll <- list()
    for (i in seq_along(next_steps)) {
      ll <- c(ll, complete_path_a(c(path, next_steps[i]), edges))
    }
    return(ll)
  }
}

day_12a <- function(input) {
  edges <- get_edges(input)
  all_path <- complete_path_a("start", edges)
  length(all_path)
}

day_12a(test_12)
day_12a(input_12)

filter_neighbors_b <- function(ngb, path) {
  path_low <- path[path == tolower(path)]
  if (length(path_low) == length(unique(path_low))) {
    return(ngb)
  } else {
    ngb_low <- ngb[ngb == tolower(ngb)]
    setdiff(ngb, intersect(ngb_low, path))
  }
}

complete_path_b <- function(path, edges) {
  last <- path[length(path)]
  if (last == "end") {
    return(list(path))
  } else {
    ngb <- get_neighbors(last, edges)
    next_steps <- filter_neighbors_b(ngb, path)
    ll <- list()
    for (i in seq_along(next_steps)) {
      ll <- c(ll, complete_path_b(c(path, next_steps[i]), edges))
    }
    return(ll)
  }
}

day_12b <- function(input) {
  edges <- get_edges(input)
  all_path <- complete_path_b("start", edges)
  length(all_path)
}

day_12b(test_12)
day_12b(input_12)
