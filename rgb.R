# # Part A, 4/24 -- 1) 10 pts
create_rgb <- function(n=5) {
  rgb_codes = c()

  for (i in 1:n) {                   

    rgb = toString(sample(0:255, 3))
    rgb = sprintf('(%s)', rgb)
    rgb_codes = c(rgb_codes, rgb)
  }
  return(rgb_codes)
}

Q1 = create_rgb()

# # Part A, 5/24 -- 2) 5 pts
find_quotient <- function(a, b) { 
  return(a%/%b)
}

# # Part A, 6/24 -- 3) 5 pts
find_remainder <- function(a, b) {
  return(a%%b)
}
# # Part A, 7/24 -- 4) 10 pts
extract_rgb <- function(rgb_codes) {
  red_vector = c()
  green_vector = c()
  blue_vector = c()
  
  for (rgb in rgb_codes) {
    rgb = gsub('[()]', '', rgb)
    rgb = unlist(strsplit(rgb, ','))
    rgb = as.integer(rgb)
    
    red_vector = c(red_vector, rgb[1])
    green_vector = c(green_vector, rgb[2])
    blue_vector = c(blue_vector, rgb[3])
  }
  
  df = data.frame(R=red_vector, G=green_vector, B=blue_vector)
  return(df)
}

Q4 = extract_rgb(Q1)

# # Part A , 8/24 -- 5) 5 pts
Q5a_quotient = data.frame(lapply(Q4, find_quotient, b=16))
Q5b_remainder = data.frame(lapply(Q4, find_remainder, b=16))

# # Part A, 10/24 -- 6) 5 pts
replace_10_16 <- function(vector) {
  result = c()
  
  for (i in vector) {
    result = c(result, c(seq(0, 9), LETTERS[1:6])[i + 1])
  }
  return(result)
}

# # Part A, 11/24 -- 7) 5 pts
Q7a_quotient = data.frame(apply(Q5a_quotient, MARGIN=2, FUN=replace_10_16))
Q7b_remainder = data.frame(apply(Q5b_remainder, MARGIN=2, FUN=replace_10_16))

# # Part A, 12/24 -- 8) 10 pts
generate_hex <- function(quotient, remainder) {
  hex_codes = c()
  
  for (i in 1:nrow(quotient)) {
    hex = c()
    for (j in 1:ncol(quotient)) {
      color_code = paste(quotient[[i, j]], remainder[[i, j]], sep='')
      hex = paste(hex, color_code, sep='')
    }
    hex_codes = c(hex_codes, sprintf('#%s', hex))
  }
  return(hex_codes)
}

hex_codes = generate_hex(Q7a_quotient, Q7b_remainder)

final_result = data.frame(rgb=Q1, hex_code=hex_codes)