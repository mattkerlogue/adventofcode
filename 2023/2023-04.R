# advent of code 2023
# day 4

input <- readLines("2023/input/2023-04")

# part 1

winners <- strsplit(gsub("Card\\s+\\d+:\\s+(.*)\\s+\\|.*", "\\1", input), split = "\\s+")
my_nums <- strsplit(gsub("Card\\s+\\d+:\\s+.*\\s+\\|\\s+(.*)", "\\1", input), split = "\\s+")

wins <- numeric(length(input))

for (i in 1:length(input)) {
  wins[i] <- sum(my_nums[[i]] %in% winners[[i]])
}

sum(2^(wins[wins > 0] - 1))

# part 2

copies <- rep(1, length(wins))

for (j in 1:length(wins)) {
  if (wins[j] > 0) {
    for (r in 1:copies[j]) {
      for (w in 1:wins[j]) {
        copies[j + w] <- copies[j + w] + 1
      }
    }
  }
}

sum(copies)
