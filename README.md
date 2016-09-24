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

# For Developing
The `DESCRIPTION` file must exist first. Also, `devtools` and `roxygen` must
be installed in `R`. See *Usage* above.

Make edits in the `R/` dir. Test in `test.R`. Before pushing changes, run the
whole `test.R` script. If no errors occur, the docs and package will be
created. a `man` dir and `NAMESPACE` file will be generated. Do not touch them.
Edit the roxygen comments in `R` instead. Only functions with `#' @export` are
exported.
