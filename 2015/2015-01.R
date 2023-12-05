# advent of code 2015
# day 1

input <- scan(file = "2015/input/2015-01", what = character())

# part 1

floors <- ((unlist(strsplit(input, split = character())) == "(") * 2) - 1

sum(floors)

# part 2

min(which(cumsum(floors) < 0))
