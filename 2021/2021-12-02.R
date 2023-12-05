# Advent of Code 2021

input <- scan(
  file = "2021/data/2021-12-02.txt",
  what = character(),
  sep = "\n")

# part one
horizontal <- sum(
  as.numeric(
    gsub("\\D", "", input[grepl("forward", input)])
  )
)

vertical <- sum(
  as.numeric(
    gsub("down ", "",
         gsub("up ", "-", input[!grepl("forward", input)])
    )
  )
)

horizontal * vertical

# part two
x <- 0
aim <- 0
depth <- 0

for (i in input) {
  value <- as.numeric(gsub("\\D", "", i))
  if (grepl("forward", i)) {
    x <- x + value
    depth <- depth + (aim * value)
  } else if (grepl("down", i)) {
    aim <- aim + value
  } else if (grepl("up", i)) {
    aim <- aim - value
  }
}

x * depth
