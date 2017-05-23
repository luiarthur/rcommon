post.summary <- function(X, alpha=.05) {
  #' @export
  post.mean <- apply(X, 2, mean)
  post.sd <- apply(X, 2, sd)
  post.ci <- t(apply(X, 2, quantile, c(alpha/2, 1-alpha/2)))

  out <- cbind(post.mean, post.sd, post.ci)
  colnames(out) <- c('mean', 'sd', 'ci.lo', 'ci.hi')
  rownames(out) <- colnames(X)

  out
}

