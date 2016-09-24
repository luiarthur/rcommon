# rcommon
R functions that I use commonly

# Usage

~~~ R
# install.packages("devtools")
devtools::install_github("luiarthur/rcommon")
library(rcommon)

X <- matrix(rnorm(5*100),ncol=5)
my.pairs(X)
~~~
