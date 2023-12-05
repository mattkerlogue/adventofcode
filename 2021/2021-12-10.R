# Advent of Code 2021
# Day 10

input <- readLines("2021/data/2021-12-10.txt")

# Part one

lines <- strsplit(input, character())
error_score <- 0

for (i in 1:length(input)){

  line_chrs <- lines[[i]]

  chunks <- character()

  for (j in 1:length(line_chrs)) {

    chr <- line_chrs[j]

    if (chr %in% c("(", "[", "{", "<")) {
      chunks <- c(chr, chunks)
    } else if(chr == ")") {

      if(chunks[1] == "(") {
        chunks <- chunks[-1]
      } else {
        error_score <- error_score + 3
        break
      }

    } else if(chr == "]") {

      if(chunks[1] == "[") {
        chunks <- chunks[-1]
      } else {
        error_score <- error_score + 57
        break
      }

    } else if(chr == "}") {

      if(chunks[1] == "{") {
        chunks <- chunks[-1]
      } else {
        error_score <- error_score + 1197
        break
      }

    } else if(chr == ">") {

      if(chunks[1] == "<") {
        chunks <- chunks[-1]
      } else {
        error_score <- error_score + 25137
        break
      }

    }

  }

}

error_score

# Part 2

completion_scores <- numeric()

for (i in 1:length(input)){

  line_chrs <- lines[[i]]

  chunks <- character()

  line_error <- FALSE

  for (j in 1:length(line_chrs)) {

    chr <- line_chrs[j]

    if (chr %in% c("(", "[", "{", "<")) {
      chunks <- c(chr, chunks)
    } else if(chr == ")") {

      if(chunks[1] == "(") {
        chunks <- chunks[-1]
      } else {
        line_error <- TRUE
        break
      }

    } else if(chr == "]") {

      if(chunks[1] == "[") {
        chunks <- chunks[-1]
      } else {
        line_error <- TRUE
        break
      }

    } else if(chr == "}") {

      if(chunks[1] == "{") {
        chunks <- chunks[-1]
      } else {
        line_error <- TRUE
        break
      }

    } else if(chr == ">") {

      if(chunks[1] == "<") {
        chunks <- chunks[-1]
      } else {
        line_error <- TRUE
        break
      }

    }

  }

  if (length(chunks) > 0 & !line_error) {

    line_completion_score <- 0

    for (k in 1:length(chunks)) {

      line_completion_score <- line_completion_score * 5

      if (chunks[k] == "(") {
        line_completion_score <- line_completion_score + 1
      } else if (chunks[k] == "[") {
        line_completion_score <- line_completion_score + 2
      } else if (chunks[k] == "{") {
        line_completion_score <- line_completion_score + 3
      } else if (chunks[k] == "<") {
        line_completion_score <- line_completion_score + 4
      }

    }

    completion_scores <- c(completion_scores, line_completion_score)

  }

}

median(completion_scores)
