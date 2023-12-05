# advent of code 2023
# day 1

input <- readLines("2023/input/2023-01")

# part 1
nums <- gsub("\\D", "", input)

n1 <- substr(nums, 1, 1)
n2 <- substr(nums, nchar(nums), nchar(nums))

vals <- as.numeric(paste0(n1, n2))

sum(vals)

# part 2

numbers <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

nn1 <- regexec("\\d|one|two|three|four|five|six|seven|eight|nine", input, perl = TRUE)
nn1_num <- substr(
  input,
  sapply(nn1, function(x) x[1]),
  (sapply(nn1, function(x) x[1]) + sapply(nn1, attr, which = "match.length") - 1)
)

rev_input <- sapply(lapply(strsplit(input, split = character()), rev), paste0, collapse = "")
nn2 <- regexec("\\d|eno|owt|eerht|ruof|evif|xis|neves|thgie|enin", rev_input, perl = TRUE)

nn2_numr <- substr(
  rev_input,
  sapply(nn2, function(x) x[1]),
  (sapply(nn2, function(x) x[1]) + sapply(nn2, attr, which = "match.length") - 1)
)

nn2_num <- sapply(lapply(strsplit(nn2_numr, split = character()), rev), paste0, collapse = "")

new_nums <- paste0(nn1_num, nn2_num)

for (i in 1:length(numbers)){
  new_nums <- gsub(numbers[i], i, new_nums)
}

sum(as.numeric(new_nums))
