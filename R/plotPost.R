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

  customDiag <- function(i,X) {
    plotPost(X[,i],main=cnames[i],float=T,yaxt="n",...)
  }


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


plotPost <- function(x,ci=TRUE,stats=TRUE,trace=TRUE,dig=3,cex.ap=1,
                     legend.pos="right",col.area="cornflowerblue",float=FALSE,
                     cex.l=1,show.xaxis.mean=TRUE,acc=TRUE,...) {
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
  x.axis <- if (show.xaxis.mean) c(ci.x,xbar) else ci.x
  axis(1,at=x.axis,labels=round(x.axis,dig),las=0,fg="grey",
       cex.axis=cex.ap)
  color.den(den.x,from=ci.x[1],to=ci.x[2],
            col.area=ci.col,col.den=maj.col,add=TRUE)
  lines(c(xbar,xbar),c(0,bound(xbar,den.x,ret=F)),lwd=2,col="red")
  acc_rate <- length(unique(x)) / length(x)
  if (stats) {
    ciString <- ""#paste0("(",round(ci.x[1],dig),", ",round(ci.x[2],dig),")")
    leg <- paste(c("Mean: ","SD: ","95% CI"),
                 c(round(xbar,dig),
                   round(sd(x),dig),
                   ciString))
    if (grepl("right",legend.pos)) {
      argmax_nchar <- which.max(nchar(leg))
      tmp <- legend(legend.pos,cex=cex.l,bty="n",
                    text.width = strwidth(argmax_nchar),
                    xjust=1,yjust=1,
                    legend=c("","",""))
      text(tmp$rect$left + tmp$rect$w, tmp$text$y, leg, pos = 2,cex=cex.l,
           font=2, col=c("red","grey20",ci.col))
    } else {
      legend(legend.pos,cex=cex.l,bty="n",text.col=c("red","grey20",ci.col),
             text.font=2,legend=leg)
    }
  }
  if (trace) {
    plotInPlot(function() 
      plot(x,fg="grey",bty="n",col="grey",type='l', col.axis="grey",axes=FALSE,
           main=ifelse(acc,paste0("acc: ",acc_rate*100,"%"),""),
           cex.main=.9, col.main='grey20'))
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
  quantile(x,c(a/2,1-a/2))
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

mar.ts <- function() {
  #' @export
  c(0, 5.1, 0, 2.1)
}
oma.ts <- function() {
  #' @export
  c(6, 0, 5, 0)
}
mar.default <- function() {
  #' @export
  c(5.1, 4.1, 4.1, 2.1)
}
oma.default <- function() {
  #' @export
  rep(0,4)
}

