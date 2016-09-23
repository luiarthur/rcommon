\name{plotPosts}
\alias{plotPosts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
yes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotPosts(M, cnames = colnames(M), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{M}{
%%     ~~Describe \code{M} here~~
}
  \item{cnames}{
%%     ~~Describe \code{cnames} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (M, cnames = colnames(M), ...) 
{
    customDiag <- function(i, X) plotPost(X[, i], main = cnames[i], 
        float = T, stats = F, yaxt = "n", ...)
    customUpper <- function(i, j, X) {
        plot(X[, c(j, i)], xlab = cnames[j], ylab = cnames[i], 
            pch = 20, xaxt = "n", yaxt = "n", bty = "n", fg = "grey", 
            main = "", col = "grey", type = "l", lwd = 0.5)
        plot.contour(X[, c(j, i)], col = col.mult("cornflowerblue"), 
            add = TRUE)
    }
    if (NCOL(M) == 1) 
        plotPost(M, ...)
    else {
        par.orig <- par(no.readonly = TRUE)
        my.pairs(M, customDiag = customDiag, customUpper = customUpper)
        par(par.orig)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
