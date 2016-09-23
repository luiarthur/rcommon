plotPost <-
function(x,hpd=TRUE,stats=TRUE,trace=TRUE,dig=3,cex.a=1,
                     col.area="cornflowerblue",float=FALSE,...) {
  par.orig <- par(no.readonly=TRUE)
  maj.col <- col.area
  hpd.col <- col.mult(maj.col)
  xbar <- mean(x)
  den.x <- density(x)
  hpd.x <- get.hpd(x,a=.05,len=1e3)
  color.den(den.x,from=min(den.x$x),to=max(den.x$x),col.main="grey20",
            col.area=maj.col,col.den="white",fg="grey",bty="n",xaxt="n",...)
  axis(1,at=c(hpd.x,xbar),labels=round(c(hpd.x,xbar),dig),las=0,fg="grey",
       cex.axis=cex.a)
  color.den(den.x,from=hpd.x[1],to=hpd.x[2],
            col.area=hpd.col,col.den=maj.col,add=TRUE)
  lines(c(xbar,xbar),c(0,bound(xbar,den.x,ret=F)),lwd=2,col="red")
  if (trace) plotInPlot(function() 
                          plot(x,fg="grey",bty="n",col="grey",type='l',
                               col.axis="grey",axes=FALSE))
  if (stats) {
    hpdString <- paste0("(",round(hpd.x[1],dig),", ",round(hpd.x[2],dig),")")
    legend("topleft",bty="n",text.col="grey20",
           legend=paste(c("Mean: ","SD: ","95% HPD: "),
                        c(round(xbar,dig),round(sd(x),dig),hpdString)))
  }
  if (!float) par(par.orig)
}
