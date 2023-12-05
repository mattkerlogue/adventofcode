# Advent of Code 2022
# Day 6

input <- scan(
  "2022/input/2022-06",
  what = character()
)

# part one

chars <- strsplit(input, split = character())[[1]]

for (i in 4:length(chars)) {
  chk_chars <- chars[(i-3):i]
  multi_chars <- 0
  for (j in chk_chars) {
    if (sum(chk_chars == j) > 1) {
      multi_chars <- multi_chars + 1
    }
  }
  if (multi_chars == 0) {
    break
  }
}

print(i)

# part two

for (i in 14:length(chars)) {
  chk_chars <- chars[(i-13):i]
  multi_chars <- 0
  for (j in chk_chars) {
    if (sum(chk_chars == j) > 1) {
      multi_chars <- multi_chars + 1
    }
  }
  if (multi_chars == 0) {
    break
  }
}

print(i)
