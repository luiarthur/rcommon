color.fn <-
function(f,from,to,col.area="red") {
  x <- seq(from,to,by=(to-from)/1e6)
  polygon(c(from,x,to),
          c(0,f(x),0),col=col.area,border=F)
}
