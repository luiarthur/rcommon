\name{color.den}
\alias{color.den}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
yes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
color.den(den, from, to, col.den = "white", col.area = "red", add = F, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{den}{
%%     ~~Describe \code{den} here~~
}
  \item{from}{
%%     ~~Describe \code{from} here~~
}
  \item{to}{
%%     ~~Describe \code{to} here~~
}
  \item{col.den}{
%%     ~~Describe \code{col.den} here~~
}
  \item{col.area}{
%%     ~~Describe \code{col.area} here~~
}
  \item{add}{
%%     ~~Describe \code{add} here~~
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
function (den, from, to, col.den = "white", col.area = "red", 
    add = F, ...) 
{
    if (add) {
        lines(den, col = col.den, ...)
    }
    else {
        plot(den, col = col.den, ...)
    }
    polygon(c(from, den$x[den$x >= from & den$x <= to], to), 
        c(0, den$y[den$x >= from & den$x <= to], 0), col = col.area, 
        border = col.den)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
