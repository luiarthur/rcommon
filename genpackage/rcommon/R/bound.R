bound <-
function(x, dens, return.x=TRUE){
  # Mickey Warner: 
  # https://github.com/mickwar/r-sandbox/blob/master/mcmc/bayes_functions.R
  # returns the x-value in dens that is closest
  # to the given x
  if (return.x)
    dens$x[which.min(abs(dens$x-x))]
  else 
    dens$y[which.min(abs(dens$x-x))]
}
