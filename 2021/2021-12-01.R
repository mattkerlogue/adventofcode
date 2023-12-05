# Advent of Code 2021

input <- scan(
  file = "2021/data/2021-12-01.txt",
  what = numeric(),
  sep = "\n")

# Part One
# How many measurements are larger than the previous measurement

comparison <- NA

for (i in 2:length(input)) {
  if (input[i] > input[i - 1]) {
    comparison <- c(comparison, TRUE)
  } else {
    comparison <- c(comparison, FALSE)
  }
}

sum(comparison, na.rm = TRUE)

# Part Two

slider <- numeric()

for (i in 3:length(input)) {
  slider <- c(slider, sum(input[i - 2], input[i - 1], input[i]))
}

slide_comparison <- NA

for (i in 2:length(slider)) {
  if (slider[i] > slider[i - 1]) {
    slide_comparison <- c(slide_comparison, TRUE)
  } else {
    slide_comparison <- c(slide_comparison, FALSE)
  }
}

sum(slide_comparison, na.rm = TRUE)
