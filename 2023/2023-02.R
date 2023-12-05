# advent of code 2023
# day 2

input <- readLines("2023/input/2023-02")

# part 1
sets <- strsplit(gsub("^Game \\d+: ", "", input), split = ",|;")
blues <- sapply(lapply(sets, function(x) as.numeric(gsub("\\D", "", x[grepl("blue", x)]))), max)
reds <- sapply(lapply(sets, function(x) as.numeric(gsub("\\D", "", x[grepl("red", x)]))), max)
greens <- sapply(lapply(sets, function(x) as.numeric(gsub("\\D", "", x[grepl("green", x)]))), max)

sum(which(reds <= 12 & greens <= 13 & blues <= 14))

# part 2

sum(reds * blues * greens)
