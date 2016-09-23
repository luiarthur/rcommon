plotPosts <-
function(M,cnames=colnames(M),...) {
  customDiag <- function(i,X) 
    plotPost(X[,i],main=cnames[i],float=T,stats=F,yaxt="n",...)

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
