# advent of code 2023
# day 3

input <- readLines("2023/input/2023-03")

# part 1

all_chars <- sort(unique(unlist(strsplit(input, character()))))
syms <- all_chars[!(all_chars %in% c(0:9, "."))]
syms_regex <- paste0("\\", paste0(syms, collapse = "|\\"))

num_locs <- gregexec("\\d+", input, perl = TRUE)
sym_locs <- gregexec(syms_regex, input, perl = TRUE)

part_nums <- c()

for (i in 1:length(num_locs)) {

  starts <- c(num_locs[[i]] - 1)
  ends <- c(num_locs[[i]] + attr(num_locs[[i]], "match.length"))

  for (j in 1:length(starts)) {
    if (i > 1){
      row_above <- substr(input[i - 1], starts[j], ends[j])
    } else {
      row_above <- ""
    }
    my_row <- substr(input[i], starts[j], ends[j])
    if (i < length(num_locs)){
      row_below <- substr(input[i + 1], starts[j], ends[j])
    } else {
      row_below <- ""
    }

    if (grepl(syms_regex, paste0(row_above, my_row, row_below, collapse = ""))) {
      part_nums <- c(part_nums, gsub("\\D", "", my_row))
    }

  }

}

sum(as.numeric(part_nums))
