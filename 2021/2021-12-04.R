# Advent of Code 2021
library(tidyverse)

input <- scan(
  file = "2021/data/2021-12-04.txt",
  what = numeric(),
  sep = ",",
  nlines = 1)

boards <- read_table(
  file = "2021/data/2021-12-04.txt",
  skip = 1,
  col_names = FALSE
) |>
  mutate(
    board = lag(row_number() %/% 5, default = 0)
  ) |>
  group_by(board) |>
  mutate(
    row = row_number()
  ) |>
  ungroup() |>
  pivot_longer(cols = starts_with("X"), names_to = "column") |>
  mutate(column = as.numeric(gsub("X", "", column)))

play <- boards |>
  mutate(marked = FALSE)

for (i in input) {

  play <- play |>
    mutate(marked = if_else(value == i, TRUE, marked))

  check_rows <- play |>
    group_by(board, row) |>
    summarise(marks = sum(marked), .groups = "drop") |>
    filter(marks == 5) |>
    pull(board)

  if (length(check_rows) > 0) {
    break
  }

  check_cols <- check_rows <- play |>
    group_by(board, column) |>
    summarise(marks = sum(marked), .groups = "drop") |>
    filter(marks == 5) |>
    pull(board)

  if (length(check_cols) > 0) {
    break
  }

}

winning_board <- play |>
  filter(board == min(check_rows, check_cols))

winning_board |>
  filter(!marked) |>
  pull(value) |>
  sum() * i

# Part Two

play <- boards |>
  mutate(marked = FALSE,
         board_won = FALSE)

board_stats <- boards |>
  distinct(board) |>
  mutate(won = FALSE)

for (i in input) {

  play <- play |>
    mutate(marked = if_else(value == i, TRUE, marked))

  check_rows <- play |>
    filter(!board_won) |>
    group_by(board, row) |>
    summarise(marks = sum(marked), .groups = "drop") |>
    filter(marks == 5) |>
    pull(board)

  if (length(check_rows) > 0) {

    play <- play |>
      mutate(
        board_won = if_else(board %in% check_rows, TRUE, board_won)
      )

    board_stats <- board_stats |>
      mutate(won = if_else(board %in% check_rows, TRUE, won))
  }

  check_board <- board_stats |>
    filter(!won) |>
    nrow()

  if (check_board == 0) {
    break
  }

  check_cols <- check_rows <- play |>
    filter(!board_won) |>
    group_by(board, column) |>
    summarise(marks = sum(marked), .groups = "drop") |>
    filter(marks == 5) |>
    pull(board)

  if (length(check_cols) > 0) {
    play <- play |>
      mutate(
        board_won = if_else(board %in% check_rows, TRUE, board_won)
      )

    board_stats <- board_stats |>
      mutate(won = if_else(board %in% check_rows, TRUE, won))
  }

  check_board <- board_stats |>
    filter(!won) |>
    nrow()

  if (check_board == 0) {
    break
  }

}

last_board <- min(check_cols, check_rows)

play |>
  filter(board == last_board) |>
  filter(!marked) |>
  pull(value) |>
  sum() * i
