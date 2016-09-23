\name{plotInPlot}
\alias{plotInPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotInPlot(minor.plot, coords = "topright", scale = 1/3, stay = F, bg.col = rgb(0, 0, 0, 0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{minor.plot}{
%%     ~~Describe \code{minor.plot} here~~
}
  \item{coords}{
%%     ~~Describe \code{coords} here~~
}
  \item{scale}{
%%     ~~Describe \code{scale} here~~
}
  \item{stay}{
%%     ~~Describe \code{stay} here~~
}
  \item{bg.col}{
%%     ~~Describe \code{bg.col} here~~
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
function (minor.plot, coords = "topright", scale = 1/3, stay = F, 
    bg.col = rgb(0, 0, 0, 0)) 
{
    mar <- x1 <- x2 <- y1 <- y2 <- NULL
    rx1 <- rx2 <- ry1 <- ry2 <- NULL
    s <- par("usr")
    if (is.numeric(coords)) {
        x1 <- coords[1]
        x2 <- coords[2]
        y1 <- coords[3]
        y2 <- coords[4]
    }
    else if (coords == "topright") {
        x1 <- s[1] + (s[2] - s[1]) * (1 - scale)
        x2 <- s[2] - (s[2] - s[1]) * 0.01
        y1 <- s[3] + (s[4] - s[3]) * (1 - scale)
        y2 <- s[4]
        mar <- c(0.1, 0.1, 1, 0.1)
    }
    else if (coords == "bottomright") {
        x1 <- s[1] + (s[2] - s[1]) * (1 - scale)
        x2 <- s[2] - (s[2] - s[1]) * 0.01
        y1 <- s[3] + (s[4] - s[3]) * 0.05
        y2 <- y1 + (s[4] - s[3]) * (scale)
        mar <- c(1, 0.1, 1, 0.1)
    }
    else if (coords == "topleft") {
        x1 <- s[1] + (s[2] - s[1]) * 0.05
        x2 <- x1 + (s[2] - s[1]) * (scale)
        y1 <- s[3] + (s[4] - s[3]) * (1 - scale)
        y2 <- s[4]
        mar <- c(0.1, 1, 1, 0.1)
    }
    else if (coords == "bottomleft") {
        x1 <- s[1] + (s[2] - s[1]) * 0.05
        x2 <- x1 + (s[2] - s[1]) * (scale)
        y1 <- s[3] + (s[4] - s[3]) * 0.05
        y2 <- y1 + (s[4] - s[3]) * (scale)
        mar <- c(1, 1, 1, 0.1)
    }
    mfg <- par()$mfg
    opts <- par(no.readonly = T)
    par(fig = c(grconvertX(c(x1, x2), from = "user", to = "ndc"), 
        grconvertY(c(y1, y2), from = "user", to = "ndc")), mar = mar, 
        new = TRUE)
    minor.plot()
    par(opts)
    if (!stay) {
        row.num <- mfg[1]
        col.num <- mfg[2]
        last.row <- mfg[3]
        last.col <- mfg[4]
        if (col.num < last.col) {
            mfg[2] <- mfg[2] + 1
        }
        else {
            if (row.num < last.row) {
                mfg[1] <- mfg[1] + 1
            }
            else {
                mfg[1] <- 1
            }
            mfg[2] <- 1
        }
    }
    par(mfg = mfg)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
