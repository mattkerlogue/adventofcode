# advent of code 2022
# day seven
library(dplyr)

input <- readLines(
  "2022/input/2022-07"
)

# part one

all_paths <- "/"
current_path <- "/"

for (i in 2:length(input)) {
  if (grepl("^\\$ cd \\.\\.", input[i])) {
    current_path <- current_path[-1]
  } else if (grepl("^\\$ cd", input[i])) {
    current_path <- c(gsub("^\\$ cd (.*)", "\\1", input[i]), current_path)
  }
  all_paths <- c(all_paths, paste0(rev(current_path), collapse = "/"))
}

console <- tibble::tibble(
  console = input,
  file = grepl("^\\d", input),
  dir_move = grepl("^\\$ cd", input),
  dir_path = all_paths
) |>
  mutate(
    file_size = if_else(
      file,
      as.numeric(gsub("\\D", "", console)),
      NA_real_
    )
  )

all_dirs <- unique(console$dir_path)

dir_sizes <- numeric(length(all_dirs))

for (j in 1:length(all_dirs)) {
  dir_sizes[j] <- sum(console$file_size[grepl(all_dirs[j], console$dir_path)], na.rm = TRUE)
}

sum(dir_sizes[dir_sizes <= 100000])

# part two

free_space <- (70000000 - max(dir_sizes))
req_space <- 30000000 - free_space

min(dir_sizes[dir_sizes >= req_space])
