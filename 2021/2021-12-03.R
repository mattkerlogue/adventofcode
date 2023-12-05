# Advent of Code 2021

input <- scan(
  file = "2021/data/2021-12-03.txt",
  what = character(),
  sep = "\n")

# part one

x <- matrix(as.numeric(unlist(strsplit(input, split = character()))),
            byrow = TRUE, nrow = length(input))

gamma <- strtoi(paste0(round(colMeans(x)), collapse = ""),2)
epsilon <- strtoi(paste0(1 - round(colMeans(x)), collapse = ""),2)

gamma * epsilon

# part two

o2 <- as.data.frame(x)

for (i in 1:ncol(o2)) {

  t <- table(o2[, i])

  if (t[1] == t[2]) {
    t <- 1
  } else {
    t <- as.numeric(names(t[t == max(t)]))
  }

  o2 <- o2[o2[, i] == t, ]

  if (nrow(o2) == 1) {
    break
  }

}

o2 <- strtoi(paste0(o2, collapse = ""), 2)

co2 <- x

for (i in 1:ncol(co2)) {

  t <- table(co2[, i])

  if (t[1] == t[2]) {
    t <- 0
  } else {
    t <- as.numeric(names(t[t == min(t)]))
  }

  co2 <- co2[co2[, i] == t, ]

  if (nrow(co2) == 1) {
    break
  }

}

co2 <- strtoi(paste0(co2, collapse = ""), 2)

o2 * co2

