# Advent of Code 2022
# Day 01

input <- readLines(
  "2022/input/2022-01"
)

input <- as.numeric(input)

# part one

df <- data.frame(
  calories = input,
  group = cumsum(is.na(input))
)

agg_df <- aggregate(calories ~ group, data = df, FUN = sum, na.rm = TRUE)

max(agg_df$calories)

# part two

sum(sort(agg_df$calories, decreasing = TRUE)[1:3])
