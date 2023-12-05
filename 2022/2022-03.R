# Advent of Code 2022
# Day 03

input <- readLines(
  "2022/input/2022-03"
)

# part one
rucksack_length <- nchar(input)
compartment1_length <- rucksack_length/2

compartment1_chars <- lapply(strsplit(substr(input, 1, compartment1_length), split = character()), unique)
compartment2_chars <- lapply(strsplit(substr(input, compartment1_length + 1, rucksack_length), split = character()), unique)

shared_vals <- numeric()

alphaZETA <- c(letters, LETTERS)

for (i in 1:length(compartment1_chars)) {
  for (j in 1:length(compartment1_chars[[i]])) {
    if (compartment1_chars[[i]][[j]] %in% compartment2_chars[[i]]) {
      shared_vals <- c(
        shared_vals,
        which(compartment1_chars[[i]][[j]] == alphaZETA)
      )
    }
  }
}

sum(shared_vals)

# part two

badge_vals <- numeric()

for (i in seq(1, length(input), 3)) {
  group_rucksacks <- input[i:(i + 2)]
  group_chars <- lapply(strsplit(group_rucksacks, character()), unique)
  for (j in 1:length(group_chars[[1]])) {
    if ((group_chars[[1]][[j]] %in% group_chars[[2]]) &&
        (group_chars[[1]][[j]] %in% group_chars[[3]])) {
      badge_vals <- c(
        badge_vals,
        which(group_chars[[1]][[j]] == alphaZETA)
      )
    }
  }
}

sum(badge_vals)
