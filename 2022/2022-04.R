# Advent of Code 2022
# Day 04

input <- readLines(
  "2022/input/2022-04"
)

# part one

input_df <- read.csv(
  "2022/input/2022-04", header = FALSE
)

input_df$source <- input
input_df$min1 <- as.numeric(gsub("-.*", "", input_df$V1))
input_df$max1 <- as.numeric(gsub(".*-", "", input_df$V1))
input_df$min2 <- as.numeric(gsub("-.*", "", input_df$V2))
input_df$max2 <- as.numeric(gsub(".*-", "", input_df$V2))

input_df$r1in2 <- input_df$min1 >= input_df$min2 & input_df$max1 <= input_df$max2
input_df$r2in1 <- input_df$min2 >= input_df$min1 & input_df$max2 <= input_df$max1

sum(input_df$r1in2 | input_df$r2in1)

# part two

input_df$r1ov2 <- (input_df$min1 <= input_df$min2 & input_df$max1 >= input_df$min2) |
  (input_df$min1 >= input_df$min2 & input_df$max1 <= input_df$min2)

input_df$r2ov1 <- (input_df$min2 <= input_df$min1 & input_df$max2 >= input_df$min1) |
  (input_df$min2 >= input_df$min1 & input_df$max2 <= input_df$min1)

sum(input_df$r1ov2 | input_df$r2ov1)
