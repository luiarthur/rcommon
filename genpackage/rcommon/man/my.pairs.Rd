\name{my.pairs}
\alias{my.pairs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
yes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
my.pairs(M, digits = 3, customDiag = NULL, customLower = NULL, customUpper = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
%%     ~~Describe \code{M} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{customDiag}{
%%     ~~Describe \code{customDiag} here~~
}
  \item{customLower}{
%%     ~~Describe \code{customLower} here~~
}
  \item{customUpper}{
%%     ~~Describe \code{customUpper} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (M, digits = 3, customDiag = NULL, customLower = NULL, 
    customUpper = NULL) 
{
    par.orig <- par(no.readonly = TRUE)
    cnames <- colnames(M)
    k <- ncol(M)
    corrs <- 0
    if (is.null(customLower)) {
        corrs <- cor(M)
    }
    mar.orig <- par("mar")
    par(mfrow = c(k, k), mar = c(0, 0, 0, 0) + 2)
    for (i in 1:k) {
        if (i > 1) {
            for (j in 1:(i - 1)) {
                if (is.null(customLower)) {
                  plot(1, type = "n", axes = F, xlab = "", ylab = "", 
                    main = "")
                  r <- round(corrs[i, j], digits)
                  cex.cor <- max(0.8/strwidth(format(r)) * abs(r), 
                    1)
                  text(1, labels = r, cex = cex.cor, col = "grey")
                }
                else {
                  customLower(i, j, M)
                }
            }
        }
        if (is.null(customDiag)) {
            hist(M[, i], prob = TRUE, bty = "n", fg = "grey", 
                col = "grey", border = "white", xlab = "", main = cnames[i])
        }
        else {
            customDiag(i, M)
        }
        if (i < k) {
            for (j in (i + 1):k) {
                if (is.null(customUpper)) {
                  plot(M[, c(j, i)], xlab = cnames[j], ylab = cnames[i], 
                    pch = 20, bty = "n", fg = "grey", main = "", 
                    axes = F)
                }
                else {
                  customUpper(i, j, M)
                }
            }
        }
    }
    par(par.orig)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
