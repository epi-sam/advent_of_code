
# Common header
.DAY <- "day_01"
source("/mnt/share/code/ssbyrne/advent_of_code/2023/paths.R")
sam <- readLines(PATHS$inputs)

# Part 1 ----

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
  return(as.integer(paste(c(a,b), collapse = "")))
})

(total_sum <- sum(unlist(first_last_digit)))

# Part 2 ----

# tricksy oneight run-on english digits
test_string <- '3sevenoneight'

find_digits_int_eng <- function(x, nums){
  
  matches <- vector(mode = 'character')
  for(i_start in 1:nchar(x)){
    # Extract integer digits
    if(substr(x, i_start, i_start) %in% nums) matches <- c(matches, substr(x, i_start, i_start))
    # Extract English digits
    minL <- min(nchar(names(nums))) - 1
    maxL <- max(nchar(names(nums))) - 1
    for(i_end in minL:maxL){
      bound <- i_start + i_end
      if(substr(x, i_start, bound) %in% names(nums)) matches <- c(matches, substr(x, i_start, bound))
    }
  }
  # convert from English to digit
  digits <- vector('character')
  for(m in matches){
    digit <- match(m, names(nums))
    if(is.na(as.integer(digit))) digit <- m
    digits <- c(digits, digit)
  }
  return(as.integer(paste0(
    c(digits[1], digits[length(digits)]),
    collapse = ""
  )))
}

sum(
  unlist(
    lapply(sam, find_digits_int_eng, nums)
  )
)
