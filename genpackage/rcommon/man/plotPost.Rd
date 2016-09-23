\name{plotPost}
\alias{plotPost}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotPost(x, hpd = TRUE, stats = TRUE, trace = TRUE, dig = 3, cex.a = 1, col.area = "cornflowerblue", float = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{hpd}{
%%     ~~Describe \code{hpd} here~~
}
  \item{stats}{
%%     ~~Describe \code{stats} here~~
}
  \item{trace}{
%%     ~~Describe \code{trace} here~~
}
  \item{dig}{
%%     ~~Describe \code{dig} here~~
}
  \item{cex.a}{
%%     ~~Describe \code{cex.a} here~~
}
  \item{col.area}{
%%     ~~Describe \code{col.area} here~~
}
  \item{float}{
%%     ~~Describe \code{float} here~~
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
function (x, hpd = TRUE, stats = TRUE, trace = TRUE, dig = 3, 
    cex.a = 1, col.area = "cornflowerblue", float = FALSE, ...) 
{
    par.orig <- par(no.readonly = TRUE)
    maj.col <- col.area
    hpd.col <- col.mult(maj.col)
    xbar <- mean(x)
    den.x <- density(x)
    hpd.x <- get.hpd(x, a = 0.05, len = 1000)
    color.den(den.x, from = min(den.x$x), to = max(den.x$x), 
        col.main = "grey20", col.area = maj.col, col.den = "white", 
        fg = "grey", bty = "n", xaxt = "n", ...)
    axis(1, at = c(hpd.x, xbar), labels = round(c(hpd.x, xbar), 
        dig), las = 0, fg = "grey", cex.axis = cex.a)
    color.den(den.x, from = hpd.x[1], to = hpd.x[2], col.area = hpd.col, 
        col.den = maj.col, add = TRUE)
    lines(c(xbar, xbar), c(0, bound(xbar, den.x, ret = F)), lwd = 2, 
        col = "red")
    if (trace) 
        plotInPlot(function() plot(x, fg = "grey", bty = "n", 
            col = "grey", type = "l", col.axis = "grey", axes = FALSE))
    if (stats) {
        hpdString <- paste0("(", round(hpd.x[1], dig), ", ", 
            round(hpd.x[2], dig), ")")
        legend("topleft", bty = "n", text.col = "grey20", legend = paste(c("Mean: ", 
            "SD: ", "95\% HPD: "), c(round(xbar, dig), round(sd(x), 
            dig), hpdString)))
    }
    if (!float) 
        par(par.orig)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
