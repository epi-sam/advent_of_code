
# Common header
.DAY <- "day_01"
source("/mnt/share/code/ssbyrne/advent_of_code/2023/paths.R")

# Part 1 ----

# read inputs
sam <- readLines(PATHS$inputs)
# all have at least one digit, great
which(grepl("^[^0-9]*$", sam))

# find all the digits in each string
matches <- gregexpr("\\d", sam)
# find position of first and last digit
first_last_index <- lapply(matches, function(x){
  a <- x[1]
  b <- x[length(x)]
  return(c(a,b))
})
# find first and last digit from the indices
first_last_digit <- lapply(seq_along(sam), function(item){
  string <- sam[[item]]
  idx_a <- first_last_index[[item]][1]
  idx_b <- first_last_index[[item]][2]
  a <- as.integer(substr(string, idx_a, idx_a))
  b <- as.integer(substr(string, idx_b, idx_b))
  # return(c(a,b))
  return(as.integer(paste(c(a,b), collapse = "")))
})

(total_sum <- sum(unlist(first_last_digit)))

# Part 2 ----

# Restart, assuming we need to grab numbers in text form -----

# reference vectors
num_eng <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
nums <- 1:9; names(nums) <- num_eng
regex_num_eng <- paste(num_eng, collapse = "|")
regex_nums <- paste(c("\\d", regex_num_eng), collapse = "|")
# find all the digits in each string
matches <- gregexpr(regex_nums, sam)
# find position of first and last digit
first_last_index <- lapply(matches, function(x){
  a <- x[1]
  b <- x[length(x)]
  return(c(a,b))
})
# find length of first and last digit, whether int or char
first_last_length <- lapply(seq_along(matches), function(item){
  idx_a <- first_last_index[[item]][1]
  idx_b <- first_last_index[[item]][2]
  x <- attr(matches[[item]], "match.length")
  a <- x[1]
  b <- x[length(x)]
  return(c(a,b))
})
# grab those digits as character
first_last_as_char <- lapply(seq_along(matches), function(item){
  pos_a <- first_last_index[[item]][1]
  pos_b <- first_last_index[[item]][2]
  len_a <- first_last_length[[item]][1]
  len_b <- first_last_length[[item]][2]
  
  idx_a <- pos_a:(pos_a + len_a - 1)
  idx_b <- pos_b:(pos_b + len_b - 1)
  
  num_a <- substr(sam[[item]], idx_a[1], idx_a[length(idx_a)])
  num_b <- substr(sam[[item]], idx_b[1], idx_b[length(idx_b)])
  
  return(c(num_a, num_b))
})
# covert character to integer
first_last_digit <- lapply(first_last_as_char, function(x){
  # x <- first_last_as_char[[2]]
  ab <- list()
  for(i in 1:2){
    if(is.na(as.integer(x[i]))){
      ab[i] <- nums[which(names(nums) == x[i])]
    } else {
      ab[i] <- as.integer(x[i])
    }
  }
  return(as.integer(paste(ab, collapse = "")))
})

(total_sum <- sum(unlist(first_last_digit)))

# Troubleshoot
.idx <- 555
item <- .idx
sam[[.idx]]
matches[[.idx]]
first_last_index[[.idx]]
first_last_length[[.idx]]
first_last_as_char[[.idx]]
first_last_digit[[.idx]]
