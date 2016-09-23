color.btwn <-
function(x,ylo,yhi,from,to,col.area="grey") {
  x <- c(x,rev(x))
  y <- c(yhi,rev(ylo))

  polygon(c(x[x>=from & x<= to]),
          c(y[x>=from & x<=to]),
          col=col.area,border=F)
}
