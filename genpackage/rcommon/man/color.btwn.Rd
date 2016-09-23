\name{color.btwn}
\alias{color.btwn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
yes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
color.btwn(x, ylo, yhi, from, to, col.area = "grey")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{ylo}{
%%     ~~Describe \code{ylo} here~~
}
  \item{yhi}{
%%     ~~Describe \code{yhi} here~~
}
  \item{from}{
%%     ~~Describe \code{from} here~~
}
  \item{to}{
%%     ~~Describe \code{to} here~~
}
  \item{col.area}{
%%     ~~Describe \code{col.area} here~~
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
function (x, ylo, yhi, from, to, col.area = "grey") 
{
    x <- c(x, rev(x))
    y <- c(yhi, rev(ylo))
    polygon(c(x[x >= from & x <= to]), c(y[x >= from & x <= to]), 
        col = col.area, border = F)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
