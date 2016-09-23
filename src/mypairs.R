my.pairs <- function(M,digits=3,customDiag=NULL,customLower=NULL,customUpper=NULL) {
  par.orig <- par(no.readonly=TRUE)

  cnames <- colnames(M)
  k <- ncol(M)

  corrs <- 0
  if (is.null(customLower)) {
    corrs <- cor(M)
  }

  mar.orig <- par("mar")
  par(mfrow=c(k,k),mar=c(0,0,0,0)+2)
  for (i in 1:k) {
    if (i>1) {
      for (j in 1:(i-1)) { 
        if (is.null(customLower)) {
          plot(1, type="n", axes=F, xlab="", ylab="",main="")
               #main=paste0("Corr (",cnames[i],", ",cnames[j],")")) # empty plot
          r <- round(corrs[i,j],digits)
          cex.cor <- max(.8/strwidth(format(r)) * abs(r),1)
          text(1,labels=r,cex=cex.cor,col="grey")
        } else {
          customLower(i,j,M)
        }
      }  
    }
    
    if (is.null(customDiag)) {
      hist(M[,i],prob=TRUE,bty="n",fg="grey",col="grey",border="white",xlab="",
           main=cnames[i])
           #main=paste("Histogram of",cnames[i]))
    } else {
      customDiag(i,M)
    }

    if (i<k) {
      for (j in (i+1):k) {
        if (is.null(customUpper)) {
          plot(M[,c(j,i)],xlab=cnames[j],ylab=cnames[i],pch=20,
               bty="n",fg="grey",main="",axes=F)
               #main=paste(cnames[i],"-",cnames[j])_
        } else {
          customUpper(i,j,M)
        }
      }
    }  
  }

  #par(mfrow=c(1,1),mar=mar.orig)
  par(par.orig)
}

#X <- matrix(rnorm(100),ncol=4)
#colnames(X) <- c("I","II","III","IV")
#my.pairs(X)


