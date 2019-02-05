
<!-- README.md is generated from README.Rmd. Please edit that file -->

# checkr

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/kkmann/checkr.svg?branch=master)](https://travis-ci.org/kkmann/checkr)
[![Codecov test
coverage](https://codecov.io/gh/kkmann/checkr/branch/master/graph/badge.svg)](https://codecov.io/gh/kkmann/checkr?branch=master)
<!-- badges: end -->

Object conformance checking made easy.

## Installation

You can install the released version of checkr from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("checkr")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kkmann/checkr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(checkr)

f <- function(positive) {
    
    evaluate(ge(0), positive)
    
    # we need to do something useful (throw errors etc later)
    
}

f(1)
#> NULL
```

``` r
f(-1)
#> [1] "positive = -1.000e+00 < 0.000e+00"
```
