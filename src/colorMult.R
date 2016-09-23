col.mult <- function(col1 = 0x000000, col2 = "gray50"){
  # Mickey Warner: 
  # https://github.com/mickwar/r-sandbox/blob/master/mcmc/bayes_functions.R
  # returns the x-value in dens that is closest
  # to the given x
  if (is.character(col1))
      val1 <- t(col2rgb(col1) / 255)
  if (is.numeric(col1))
      val1 <- t(int2rgb(col1) / 255)
  if (is.character(col2))
      val2 <- t(col2rgb(col2) / 255)
  if (is.numeric(col2))
      val2 <- t(int2rgb(col2) / 255)
  rgb(val1 * val2)
}

int2rgb = function(x){
# int2rgb()
# convert an integer between 0 and 16777215 = 256^3 - 1,
# or between 0 and 0xFFFFFF
# this function is depended upon by col.mult
  # Mickey Warner: 
  # https://github.com/mickwar/r-sandbox/blob/master/mcmc/bayes_functions.R
  # returns the x-value in dens that is closest
  # to the given x
  hex <- as.character(as.hexmode(x))
  hex <- paste0("#", paste0(rep("0", 6-nchar(hex)), collapse=""), hex)
  col2rgb(hex)
}
