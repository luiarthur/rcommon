#source("plotInPlot.R")
#source("colorMult.R")
#source("colorUnderCurve.R")
#source("mypairs.R")

plotPosts <- function(M,cnames=colnames(M),...) {
  #' plot posteriors
  #' @examples
  #' M <- matrix(rnorm(1000),ncol=4)
  #' colnames(M) <- 1:4
  #' plotPosts(M)
  #' @export

  customDiag <- function(i,X) 
    plotPost(X[,i],main=cnames[i],float=T,stats=FALSE,yaxt="n",...)

  customUpper <- function(i,j,X) {
    plot(X[,c(j,i)],xlab=cnames[j],ylab=cnames[i],pch=20,
         xaxt='n',yaxt="n",
         bty="n",fg="grey",main="",col="grey",type='l',lwd=.5)
    plot.contour(X[,c(j,i)],col=col.mult("cornflowerblue"),add=TRUE)
  }


  if (NCOL(M) == 1) 
    plotPost(M,...)
  else {
    par.orig <- par(no.readonly=TRUE)
    my.pairs(M,customDiag=customDiag,customUpper=customUpper)
    par(par.orig)
  }
}


plotPost <- function(x,ci=TRUE,stats=TRUE,trace=TRUE,dig=3,cex.a=1,
                     col.area="cornflowerblue",float=FALSE,...) {
  #' plot posterior
  #' @export

  par.orig <- par(no.readonly=TRUE)
  maj.col <- col.area
  ci.col <- col.mult(maj.col)
  xbar <- mean(x)
  den.x <- density(x)
  ci.x <- get.ci(x,a=.05)
  color.den(den.x,from=min(den.x$x),to=max(den.x$x),col.main="grey20",
            col.area=maj.col,col.den="white",fg="grey",bty="n",xaxt="n",...)
  axis(1,at=c(ci.x,xbar),labels=round(c(ci.x,xbar),dig),las=0,fg="grey",
       cex.axis=cex.a)
  color.den(den.x,from=ci.x[1],to=ci.x[2],
            col.area=ci.col,col.den=maj.col,add=TRUE)
  lines(c(xbar,xbar),c(0,bound(xbar,den.x,ret=F)),lwd=2,col="red")
  if (trace) plotInPlot(function() 
                          plot(x,fg="grey",bty="n",col="grey",type='l',
                               col.axis="grey",axes=FALSE))
  if (stats) {
    ciString <- paste0("(",round(ci.x[1],dig),", ",round(ci.x[2],dig),")")
    legend("topleft",bty="n",text.col="grey20",
           legend=paste(c("Mean: ","SD: ","95% CI: "),
                        c(round(xbar,dig),round(sd(x),dig),ciString)))
  }
  if (!float) par(par.orig)
}

add.errbar <- function(ci,transpose=FALSE,x=NULL,...) {
  #' Add error bar
  #' @export
  if (any(is.null(x))) x <- 1:nrow(ci)
  if (!transpose)
    segments(x,ci[,1],x,ci[,2],...)
  else
    segments(ci[,1],x,ci[,2],x,...)
}

plot.contour <- function(M,...) {
  #' plot contour
  #' @export

  library(MASS) # filled.contour, kde2d
  J <- kde2d(M[,1],M[,2])
  contour(J,bty="n",fg="grey",...)
}

get.hpd <- function(x,a=.05,len=1e3) {
  #' get the HPD
  #' @export

  V <- matrix(seq(0,a,length=len))
  quants <- t(apply(V,1,function(v) quantile(x,c(v,v+1-a))))
  diff <- quants[,2]-quants[,1]
  min.d <- V[which.min(diff)]
  hpd <- quantile(x,c(min.d,min.d+1-a))
  hpd
}

get.ci <- function(x,a=.05) {
  stopifnot(0 <= a && a <= 1)
  quantile(x,a/2,1-a/2)
}

bound <- function(x, dens, return.x=TRUE){
  #' returns the x-value in dens that is closest to the given x
  #' @export
  #  by Mickey Warner
  if (return.x)
    dens$x[which.min(abs(dens$x-x))]
  else 
    dens$y[which.min(abs(dens$x-x))]
}
