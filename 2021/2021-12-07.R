# Advent of Code 2021
# Day 7

input <- scan(
  file = "2021/data/2021-12-07.txt",
  what = numeric(),
  sep = ","
)

# Part One

sum(Mod(input - median(input)))

# Part Two

positions <- seq(min(input), max(input))

fuel <- c()

for (p in positions) {
  distances <- Mod(input - p)
  fuel <- c(fuel, sum(distances * (distances + 1)/2))
}

names(fuel) <- positions

fuel[fuel == min(fuel)]
