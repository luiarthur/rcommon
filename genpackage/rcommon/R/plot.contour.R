plot.contour <-
function(M,...) {
  library(MASS) # filled.contour, kde2d
  J <- kde2d(M[,1],M[,2])
  contour(J,bty="n",fg="grey",...)
}
