source("R/mypairs.R")
source("R/plotInPlot.R")
minor <- function() 
  plot(rnorm(1000),bty="n",axes=F,type='l',ylab="",xlab="",col="grey",lwd=1)

plot(rnorm(1000),type='l',ylim=c(-3,6))
plotInPlot(minor,"topright",stay=F)

source("R/colorMult.R")
source("R/colorUnderCurve.R")
den <- density(rnorm(1000))
color.den(den,from=-6,to=6)
color.den(den,from=-2,2,col.area=col.mult("red"),add=T)

source("R/plotPost.R",chdir=TRUE)
plotPost(rt(1000,30),col.area="red")
plotPost(rt(1000,3),dig=2)

source("R/mypairs.R")
X <- matrix(rnorm(5*100),ncol=5)
colnames(X) <- 1:5
my.pairs(X)

source("R/plotPost.R",chdir=TRUE)
Y <- matrix(rnorm(4*3000),ncol=4)
colnames(Y) <- 1:4
plotPosts(Y,dig=2,cex.a=1)
plotPosts(rnorm(1000),cex.a=1)
plotPosts(rnorm(1000),cex.a=.5)
plotPosts(Y[,1:2],dig=2,stats=FALSE)
plotPosts(Y[,1:2],dig=2,stats=TRUE)

### GENERATE PACKAGE IF ALL TESTS PASS
roxygen2::roxygenise()
