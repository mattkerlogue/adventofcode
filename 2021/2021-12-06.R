# Advent of Code 2021
# Day 6

input <- scan(
  file = "2021/data/2021-12-06.txt",
  what = numeric(),
  sep = ","
)

# Part one

day <- 1

lantern_fish <- input

while (day < 81) {

  lantern_fish <- lantern_fish - 1

  if (any(lantern_fish < 0)) {
    lantern_fish <- c(lantern_fish, rep(8, sum(lantern_fish < 0)))
    lantern_fish[lantern_fish < 0] <- 6
  }

  day <- day + 1

}

length(lantern_fish)

# Part two

day <- 1

fish_counter <- numeric(9)

for (i in 1:9) {
  fish_counter[i] <- sum(input == i - 1)
}

base_count <- fish_counter

while (day < 257) {

  spawning <- fish_counter[1]
  fish_counter <- c(fish_counter[2:9], fish_counter[1])
  fish_counter[7] <- fish_counter[7] + spawning

  day <- day + 1

}

sprintf("%0.f", sum(fish_counter))
