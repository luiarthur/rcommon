#' hpd_mult (by Mickey Warner)
#'
#' @description
#' Compute the highest density posterior region from a (possibly multi-modal)
#' sample of points.
#'
#' @param x         vector of samples from some distribution
#' @param dens      a density object based on x, defaults to density(x)
#' @param prob      numeric in (0, 1) the probability that the interval should be
#' @param tol       numeric, smaller means longer compute time, but more accurate
#'                  results
#' @param interactive logical, defaults to FALSE. If TRUE, a fun plot is shown
#'                  and the function's process of choosing the region is displayed.
#'                  The user hits enter to proceed through.
#'
#' @details
#' Since hpd_mult relies on a density object, it is possible to be returned a
#' result that is outside the bounds of the data (i.e., an hpd for a beta random
#' variable that has a left end point below 0).
#'
#' A random horizontal line is repeatedly generated and the points along the line
#' that cross dens$y are calculated. These points make up a proposed hpd region
#' whose area is computed. Given this area, a new horizontal line is generated
#' until the area is close to prob (within tol).
#'
#' @seealso hpd_uni
#' @export
#' @examples
#' set.seed(1)
#' x = c(rnorm(100), rnorm(100, 5)
#' hpd = hpd_mult(x)

hpd_mult = function(x, dens, prob = 0.95, tol, interactive = FALSE){
    if (missing(dens))
        dens = density(x)
    max.k = max(dens$y)
    min.k = min(dens$y)
#   k = (max.k - min.k)/2
    k = runif(1, min.k, max.k)
    if (missing(tol))
        tol = max.k / 10000
    count = 0
    if (interactive){
        plot(dens)
        polygon(dens$x, dens$y, col='gray80')
        }
    zeros = function(y, k, return.max.min = FALSE){
        # y is expected to be density(x)$y
        out = NULL
        int = NULL
        for (i in 2:(length(y)-1)){
            # condition to check when the height crosses k
            if ((y[i] > k && y[i-1] < k) || (y[i] < k && y[i-1] > k)){
                # get the x closer to k
                out = c(out, ifelse(which.min(abs(y[c(i,i-1)]-k))==1,
                    i, i-1))
                # -1 if lower interval, +1 if upper
                int = c(int, -sign(y[i] - y[i-1]))
                }
            # check if the height exactly equals k
            if (y[i] == k){
                out = c(out, i)
                # 0 if a maximum or minimum, -1 if lower, +1 if upper
                # y[i] can only be max or min if y[i] = k, so don't
                # check this condition for when height crosses k
                int = c(int, -sign(sign(y[i]-y[i-1]) +
                    sign(y[i+1]-y[i])))
                }
            }
        # ensure that first value is lower end and last is upper end
        if (is.null(int))
            return (NULL)
        if (int[1] == 1){
            int = c(-1, int)
            out = c(1, out)
            }
        if (int[length(int)] == -1){
            int = c(int, 1)
            out = c(out, length(y))
            }
        if (return.max.min)
            return (out)
        return (out[as.logical(int)])
        }
    while (max.k - min.k > tol){
        count = count + 1
        c.prob = 0
        int = zeros(dens$y, k)
        if (is.null(int)){
            int = c(1, length(dens$x))
            }
        # sum the area in the intervals
        for (j in 1:(length(int)/2))
            c.prob = c.prob + mean(x >= dens$x[
                 int[2*j-1]] & x <= dens$x[int[2*j]])
        if (interactive){
            abline(h=k)
            abline(h=c(max.k, min.k), col='blue')
            cat("Probability:",c.prob)
            readline()
            }
        # not a large enough region, lower k
        if (c.prob < prob)
            max.k = k
        # too much region, raise k
        if (c.prob > prob)
            min.k = k
        # right-e-o!
        if (c.prob == prob)
            max.k = min.k
        # pick new height to test at
        k = runif(1, min.k, max.k)
        }
    if (interactive)
        cat("Iterations:",count,"\n")
    return (dens$x[int])
    }
