\name{col.mult}
\alias{col.mult}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
yes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
col.mult(col1 = 0, col2 = "gray50")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col1}{
%%     ~~Describe \code{col1} here~~
}
  \item{col2}{
%%     ~~Describe \code{col2} here~~
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
function (col1 = 0, col2 = "gray50") 
{
    if (is.character(col1)) 
        val1 <- t(col2rgb(col1)/255)
    if (is.numeric(col1)) 
        val1 <- t(int2rgb(col1)/255)
    if (is.character(col2)) 
        val2 <- t(col2rgb(col2)/255)
    if (is.numeric(col2)) 
        val2 <- t(int2rgb(col2)/255)
    rgb(val1 * val2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
