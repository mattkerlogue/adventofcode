# Advent of Code 2022
# Day 02

input <- readLines(
  "2022/input/2022-02"
)

elf_play <- substr(input, 1, 1)
strategy <- substr(input, 3, 3)

# part one

game_key <- c(1:3, 1:3)
names(game_key) <- c("A", "B", "C", "X", "Y", "Z")

game_score <- function(elf, me) {

  elf <- game_key[elf]
  me <- game_key[me]

  score <- 0

  if (me == elf) {
    score <- 3
  } else if (elf == 3 & me == 1) {
    score <- 6
  } else if (elf == 1 & me == 3) {
    score <- 0
  } else if (me > elf) {
    score <- 6
  }

  return(score)

}

matches <- mapply(game_score, elf = elf_play, me = strategy)

sum(game_key[strategy], matches)

# part two

out_key <- c(0, 3, 6)
names(out_key) <- c("X", "Y", "Z")

new_game <- function(elf, outcome) {

  elf <- game_key[elf]

  my_play <- 0

  if (outcome == "Y") {
    my_play <- elf
  } else if (outcome == "X") {
    if (elf == 1) {
      my_play <- 3
    } else {
      my_play <- elf - 1
    }
  } else if (outcome == "Z") {
    if (elf == 3) {
      my_play <- 1
    } else {
      my_play <- elf + 1
    }
  }

  return(my_play)

}

new_matches <- mapply(new_game, elf = elf_play, outcome = strategy)


sum(out_key[strategy], new_matches)
