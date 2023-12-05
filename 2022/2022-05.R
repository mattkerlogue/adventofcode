# advent of code 2022
# day 5
library(dplyr)

stack_input <- read.fwf(
  "2022/input/2022-05",
  widths = c(rep(4, 8), 3),
  n = 8
)

instruction_set <- scan(
  "2022/input/2022-05",
  what = character(),
  sep = "\n",
  skip = 10
)

# part one

clean_stacks <- tibble::as_tibble(stack_input) |>
  mutate(
    across(everything(), rev),
    across(everything(), ~gsub("\\W", "", .x)),
    across(everything(), ~if_else(.x == "", NA_character_, .x))
  ) |>
  rename_with(~gsub("V", "stack", .x)) |>
  as.list() |>
  purrr::map(
    ~purrr::discard(.x, is.na)
  )

instructions_clean <- tibble::as_tibble(
  read.table(text = instruction_set)
  ) |>
  select(move = V2, from = V4, to = V6) |>
  mutate(
    across(c(from, to), ~paste0("stack", .x))
  )

p1_stacks <- clean_stacks

for (i in 1:nrow(instructions_clean)) {

  from_stack <- p1_stacks[[instructions_clean$from[i]]]
  to_stack <- p1_stacks[[instructions_clean$to[i]]]

  crates_in_motion <- rev(from_stack)[1:instructions_clean$move[i]]

  to_stack <- c(to_stack, crates_in_motion)

  from_stack <- from_stack[1:(length(from_stack) - instructions_clean$move[i])]

  p1_stacks[[instructions_clean$from[i]]] <- from_stack
  p1_stacks[[instructions_clean$to[i]]] <- to_stack

}

paste0(purrr::map2_chr(p1_stacks, lengths(p1_stacks), ~.x[.y]), collapse = "")

# part two

p2_stacks <- clean_stacks

for (j in 1:nrow(instructions_clean)) {

  from_stack <- p2_stacks[[instructions_clean$from[j]]]
  to_stack <- p2_stacks[[instructions_clean$to[j]]]

  crates_in_motion <- rev(from_stack)[1:instructions_clean$move[j]]

  to_stack <- c(to_stack, rev(crates_in_motion))

  from_stack <- from_stack[1:(length(from_stack) - instructions_clean$move[j])]

  p2_stacks[[instructions_clean$from[j]]] <- from_stack
  p2_stacks[[instructions_clean$to[j]]] <- to_stack

}

paste0(purrr::map2_chr(p2_stacks, lengths(p2_stacks), ~.x[.y]), collapse = "")
