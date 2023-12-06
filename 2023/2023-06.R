# advent of code 2023
# day 6

input <- readLines("2023/input/2023-06")

# part 1

times <- as.numeric(unlist(strsplit(gsub("Time:\\s+", "", input[1]), split = "\\s+")))
dists <- as.numeric(unlist(strsplit(gsub("Distance:\\s+", "", input[2]), split = "\\s+")))

wins <- numeric(length(times))

race_sim <- function(total_time, button_time) {
  button_time*(total_time-button_time)
}

for (i in 1:length(times)) {
  wins[i] <- sum(((times[i] * (0:times[i])) - ((0:times[i])^2)) > dists[i])
}

prod(wins)

# part 2

new_time <- as.numeric(gsub("\\D", "", input[1]))
new_dist <- as.numeric(gsub("\\D", "", input[2]))

sum(((new_time * (0:new_time)) - ((0:new_time)^2)) > new_dist)

