# advent of code 2015
# day 5

input <- readLines("2015/input/2015-05")

# part 1

vowel_count <- function(x) {
  x <- unlist(strsplit(x, character()))
  sum(x == "a" | x == "e" | x == "i" | x == "o" | x == "u") > 2
}

dbl_letter <- function(x) {
  x <- unlist(strsplit(x, character()))
  sum(x[1:(length(x) - 1)] == x[2:length(x)]) > 0
}

good_strings <- function(x) {
  !grepl("ab|cd|pq|xy", x)
}

vc <- sapply(input, vowel_count)
dl <- sapply(input, dbl_letter)
bs <- sapply(input, good_strings)

sum(vc & dl & bs)

# part 2

