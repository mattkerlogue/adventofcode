# Advent of Code
# Day 8

input <- readLines("2021/data/2021-12-08.txt")

# Part One
output_vals <- unlist(strsplit(gsub(".*\\| ", "", input), split = " "))

char_count <- nchar(output_vals)

length(char_count[char_count %in% c(2, 3, 4, 7)])

# Part Two

process_signals <- function(x, y) {

  suppress_join <- function(...) {
    suppressMessages(left_join(...))
  }

  df <- tibble(signal = x) |>
    separate_rows(signal, sep = " ") |>
    mutate(
      digit_id = row_number(),
      n = nchar(signal),
      value = case_when(
        n == 2 ~ 1,
        n == 3 ~ 7,
        n == 4 ~ 4,
        n == 7 ~ 8
      ),
      chk_a = grepl("a", signal),
      chk_b = grepl("b", signal),
      chk_c = grepl("c", signal),
      chk_d = grepl("d", signal),
      chk_e = grepl("e", signal),
      chk_f = grepl("f", signal),
      chk_g = grepl("g", signal)
    )

  wires <- df |>
    filter(n == 2 | n == 3 | n == 4) |>
    select(signal, n) |>
    separate_rows(signal, sep = "") |>
    filter(signal != "")

  # get 2 chars
  chr_2 <- wires |>
    filter(n == 2) |>
    distinct(signal)

  # get 3 chars
  chr_3 <- wires |>
    filter(n == 3) |>
    distinct(signal) |>
    filter(!(signal %in% chr_2$signal)) |>
    mutate(
      signal = paste0("chk_", signal),
      value = TRUE
    ) |>
    pivot_wider(names_from = signal, values_from = value) |>
    mutate(top_bar = TRUE)

  df <- df |>
    suppress_join(chr_3)

  # get 4 chars
  chr_4 <- wires |>
    filter(n == 4) |>
    distinct(signal) |>
    filter(!(signal %in% chr_2$signal)) |>
    mutate(
      signal = paste0("chk_", signal),
      value = TRUE
    ) |>
    pivot_wider(names_from = signal, values_from = value) |>
    mutate(upper_l = TRUE)

  df <- df |>
    suppress_join(chr_4)

  chr_2 <- chr_2 |>
    mutate(
      signal = paste0("chk_", signal),
      value = TRUE
    ) |>
    pivot_wider(names_from = signal, values_from = value) |>
    mutate(right_bar = TRUE)

  df <- df |>
    suppress_join(chr_2)

  chk_df <- df |>
    mutate(
      value = case_when(
        n == 5 & right_bar ~ 3,
        n == 5 & upper_l ~ 5,
        n == 5 ~ 2,
        n == 6 & upper_l & right_bar ~ 9,
        n == 6 & right_bar ~ 0,
        n == 6 ~ 6,
        TRUE ~ value
      )
    ) |>
    select(value, starts_with("chk_")) |>
    distinct()

  o_df <- tibble(output = y) |>
    separate_rows(output, sep = " ") |>
    mutate(
      digit_id = row_number(),
      chk_a = grepl("a", output),
      chk_b = grepl("b", output),
      chk_c = grepl("c", output),
      chk_d = grepl("d", output),
      chk_e = grepl("e", output),
      chk_f = grepl("f", output),
      chk_g = grepl("g", output)
    ) |>
    suppress_join(chk_df)

  as.numeric(paste0(o_df$value, collapse = ""))

}

signal_processing <- tibble(
  signal = gsub("\\| ", "", input),
  output = gsub(".*\\| ", "", input)
) |>
  mutate(signal_id = row_number(),
         output_value = map2_dbl(signal, output, process_signals))

sum(signal_processing$output_value)
