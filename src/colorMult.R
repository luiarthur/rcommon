col.mult <- function(col1 = 0x000000, col2 = "gray50"){
  #' col.mult
  #' @description
  #' multiply two colors
  #' @param col1    the major color
  #' @param col2    the color to mix with (usually grey)
  #' @export 

  int2rgb <- function(x){
    hex <- as.character(as.hexmode(x))
    hex <- paste0("#", paste0(rep("0", 6-nchar(hex)), collapse=""), hex)
    col2rgb(hex)
  }

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
