# advent of code 2015
# day 4

input <- "ckczppom"

# part 1 (~3s)

hash_num <- 0

while (TRUE) {
  if (substr(openssl::md5(paste0(input, hash_num)), 1, 5) == "00000") {
    break
  }
  hash_num <- hash_num + 1
}

hash_num

# part 2 (~94s)

hash_num <- 0

while (TRUE) {
  if (substr(openssl::md5(paste0(input, hash_num)), 1, 6) == "000000") {
    break
  }
  hash_num <- hash_num + 1
}

hash_num
