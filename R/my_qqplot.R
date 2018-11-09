my.qqplot = function(dat, post_pred, conf_level=.95, qseq=seq(0, 1, len=100), ...) {
  #' @export

  alpha_level = 1 - conf_level
  qy= quantile(dat, qseq)
  qpp = apply(post_pred, 2, quantile, qseq)
  qpp_mean = rowMeans(qpp)
  ci = apply(qpp, 1, quantile, c(alpha_level/2, 1-alpha_level/2))

  plot(qy, qpp_mean, ...)
  color.btwn(qy, ylo=ci[1, ], yhi=ci[2, ],
             from=min(qy), to=max(qy), col.area=rgba("steelblue", .5))
  abline(0, 1, lty=2)
}

