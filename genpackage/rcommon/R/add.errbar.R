add.errbar <-
function(ci,transpose=FALSE,x=NULL,...) {
  if (any(is.null(x))) x <- 1:nrow(ci)
  if (!transpose)
    segments(x,ci[,1],x,ci[,2],...)
  else
    segments(ci[,1],x,ci[,2],x,...)
}
