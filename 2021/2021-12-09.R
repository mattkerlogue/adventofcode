# Advent of Code
# Day 9

input <- readLines("2021/data/2021-12-09.txt")

# Part One
height_map <- matrix(
  as.numeric(unlist(strsplit(input, split = character()))),
  nrow = length(input),
  byrow = TRUE
  )

risk_scores <- matrix(rep(0, 10000), nrow = 100)

for(i in 1:100){
  for(j in 1:100){

    if (i == 1) {
      if (j == 1) {
        values <- c(height_map[i, j + 1], height_map[i + 1, j])
      } else if (j == 100) {
        values <- c(height_map[i, j - 1], height_map[i + 1, j])
      } else {
        values <- c(height_map[i, j - 1], height_map[i + 1, j], height_map[i, j + 1])
      }
    } else if (i == 100) {
      if (j == 1) {
        values <- c(height_map[i, j + 1], height_map[i - 1, j])
      } else if (j == 100) {
        values <- c(height_map[i, j - 1], height_map[i - 1, j])
      } else {
        values <- c(height_map[i, j - 1], height_map[i - 1, j], height_map[i, j + 1])
      }
    } else {

      if (j == 1) {
        values <- c(height_map[i, j + 1], height_map[i - 1, j], height_map[i + 1, j])
      } else if (j == 100) {
        values <- c(height_map[i, j - 1], height_map[i - 1, j], height_map[i + 1, j])
      } else {
        values <- c(height_map[i + 1 , j],
                    height_map[i - 1, j],
                    height_map[i, j - 1],
                    height_map[i, j + 1])
      }

    }

    values <- c(values, height_map[i, j])

    if (sum(values == height_map[i, j]) > 1) {
      next
    } else if (height_map[i, j] == min(values)) {
      risk_scores[i, j] <- 1 + height_map[i, j]
    }

  }
}

sum(risk_scores)

# Part two

basins <- height_map != 9

r <- raster::raster(basins, xmn = 1, xmx = 100, ymn = 1, ymx = 100)
rc <- raster::clump(r, directions = 4)
sizes <- raster::freq(rc)

prod(sort(sizes[!is.na(sizes[,1]),2], decreasing = TRUE)[1:3])
