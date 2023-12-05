# advent of code 2015
# day 2

input <- scan("2015/input/2015-02", what = character(), sep = "x")

# part 1

dims <- matrix(as.numeric(input), ncol = 3, byrow = TRUE)

faces <- matrix(c(dims[,1] * dims[,2], dims[,2] * dims[,3], dims[,3] * dims[,1]), ncol = 3)

sum(faces*2, apply(faces, 1, min))

# part 2

ribbon <- (2 * (apply(dims, 1, sum) - apply(dims, 1, max))) + apply(dims, 1, prod)

sum(ribbon)
