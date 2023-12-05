# Advent of Code 2021
library(tidyverse)

input <- scan(
  file = "2021/data/2021-12-05.txt",
  what = character(),
  sep = "\n"
  )

# part one
lines <- tibble(
  source = input
) |>
  separate(source, into = c("x1", "y1", "x2", "y2"), remove = FALSE) |>
  mutate(
    line_id = row_number(),
    across(-source, as.numeric),
    direction = case_when(
      x1 == x2 ~ "v",
      y1 == y2 ~ "h",
      TRUE ~ "d")
  )

x_ext <- range(c(lines$x1, lines$x2))
y_ext <- range(c(lines$y1, lines$y2))

grid <- expand_grid(x = x_ext[1]:x_ext[2], y = y_ext[1]:y_ext[2]) |>
  mutate(across(everything(), as.numeric),
         total_height = 0)

sonar <- tibble(
  x = numeric(),
  y = numeric(),
  height = numeric()
)

for (i in lines$line_id) {

  d <- lines$direction[i]

  if (d == "d") {
    next
  }

  x <- lines[[i, "x1"]]:lines[[i, "x2"]]
  y <- lines[[i, "y1"]]:lines[[i, "y2"]]

  sonar <- sonar |>
    bind_rows(
      tibble(
        x = x,
        y = y,
        height = 1
      )
    )

}

heights <- sonar |>
  group_by(x, y) |>
  summarise(height = sum(height), .groups = "drop")

nrow(filter(heights, height > 1))

# part two

sonar <- tibble(
  x = numeric(),
  y = numeric(),
  height = numeric()
)

for (i in lines$line_id) {

  x <- lines[[i, "x1"]]:lines[[i, "x2"]]
  y <- lines[[i, "y1"]]:lines[[i, "y2"]]


  sonar <- sonar |>
    bind_rows(
      tibble(
        x = x,
        y = y,
        height = 1
      )
    )

}

heights <- sonar |>
  group_by(x, y) |>
  summarise(height = sum(height))

nrow(filter(heights, height > 1))
