rgba = function(col, alpha=1) {
  #' Add transparency to colors
  #' @examples
  #' plot(cbind(rnorm(300),rnorm(300)), col=rgba('steelblue', a=.5), pch=20, cex=3)
  #' @export
  sapply(col, function(co) {
    RGB = col2rgb(co) / 255
    rgb(RGB[1], RGB[2], RGB[3], alpha)
  })
}

