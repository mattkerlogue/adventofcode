# advent of code 2015
# day 3

input <- scan("2015/input/2015-03", what = character())

# direction interpretter

get_direction <- function(x) {
  if (x == "^") {
    return(c(0, 1))
  } else if (x == "v") {
    return(c(0, -1))
  } else if (x == ">") {
    return(c(1, 0))
  } else if (x == "<") {
    return(c(-1, 0))
  }
}


# part 1

directions <- unlist(strsplit(input, split = character()))

path <- matrix(c(0,0, unlist(lapply(directions, get_direction))),
               ncol = 2, byrow = TRUE)

length(unique(paste(cumsum(path[,1]), cumsum(path[,2]), sep = ",")))

# part 2

santa_dirs <- directions[seq(1, length(directions), 2)]
robos_dirs <- directions[seq(2, length(directions), 2)]

santa_path <- matrix(c(0,0, unlist(lapply(santa_dirs, get_direction))),
                     ncol = 2, byrow = TRUE)
robos_path <- matrix(c(0,0, unlist(lapply(robos_dirs, get_direction))),
                     ncol = 2, byrow = TRUE)

length(unique(
  c(
    paste(cumsum(santa_path[,1]), cumsum(santa_path[,2]), sep = ","),
    paste(cumsum(robos_path[,1]), cumsum(robos_path[,2]), sep = ",")
  )
))


