my.color <- function(dat,from,to,col.den="black",col.area="red",...) {
  #' color under curve
  #' @export

  if (is(dat)[1] == "function") {
    color.fn(dat,from,to,col.area)
  } else if (is(dat)[1] == "density") {
    color.den(dat,from,to,col.den,col.area,...)
  } else if (is(dat)[1] == "matrix") {
    color.emp(dat,from,to,col.area)
  }
}


color.den <- function(den,from,to,col.den="white",col.area="red",add=F,...) {
  #' color area under a density within an interval
  #' den has to be a density object
  #' @export

  if (add) {
    lines(den,col=col.den,...)
  } else {
    plot(den,col=col.den,...)
  }
  polygon(c(from, den$x[den$x>=from & den$x<=to], to),
          c(0, den$y[den$x>=from & den$x<=to], 0),
          col=col.area,border=col.den)
}

color.fn <- function(f,from,to,col.area="red") {
  #' color area under a function within an interval
  #' @export

  x <- seq(from,to,by=(to-from)/1e6)
  polygon(c(from,x,to),
          c(0,f(x),0),col=col.area,border=F)
}


color.emp <- function(M,from,to,col.area="red") {
  #' color area under a curve for a sample
  #' @export

  x <- M[,1]
  y <- M[,2]
  polygon(c(from,x[x>=from & x<= to],to),
          c(0,y[x>=from & x<=to],0),col=col.area,border=F)
}

color.btwn <- function(x,ylo,yhi,from,to,col.area="grey") {
  #' Color area between ylo and yhi
  #' @export
  x <- c(x,rev(x))
  y <- c(yhi,rev(ylo))

  polygon(c(x[x>=from & x<= to]),
          c(y[x>=from & x<=to]),
          col=col.area,border=F)
}

color.btwn.mat <- function(x,y,from=min(x),to=max(x),col.area="grey") {
  #' Color area between ylo and yhi
  #' @export
  x <- c(x,rev(x))
  y <- c(yhi,rev(ylo))

  polygon(c(x[x>=from & x<= to]),
          c(y[x>=from & x<=to]),
          col=col.area,border=F)
}

#Examples: ######################################################

## color.den
#  x <- rnorm(10000)
#  denx <- density(x)
#  color.den(denx,1,2,col.area="blue")
#  color.den(denx,-1,0,col.area="red",add=T)
#
## color.fn
#  fn <- function(x) dnorm(x)
#  curve(dnorm(x),-5,5)
#  color.fn(fn,-2,5)
#
## color.emp
#  x <- seq(-5,5,length=1000)
#  y <- dnorm(x)
#  plot(x,y,type='l')
#  color.emp(cbind(x,y),-2,5)
