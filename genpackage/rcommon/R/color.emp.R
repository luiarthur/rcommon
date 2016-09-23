color.emp <-
function(M,from,to,col.area="red") {
  x <- M[,1]
  y <- M[,2]
  polygon(c(from,x[x>=from & x<= to],to),
          c(0,y[x>=from & x<=to],0),col=col.area,border=F)
}
