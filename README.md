
<!-- README.md is generated from README.Rmd. Please edit that file -->

# amcat4r

<!-- badges: start -->

[![R-CMD-check](https://github.com/ccs-amsterdam/amcat4r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ccs-amsterdam/amcat4r/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/ccs-amsterdam/amcat4r/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ccs-amsterdam/amcat4r?branch=main)
<!-- badges: end -->

The goal of amcat4r is to provide easy access to
[`amcat4`](https://github.com/ccs-amsterdam/amcat4) from R. Learn more
about AmCAT and amcat4r at <https://amcat-book.netlify.app/>.

## Installation

You can install the development version of amcat4r from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
remotes::install_github("ccs-amsterdam/amcat4r")
```

Note: if you have an amcat4 instance older than 4.0.14, you can use this version of the package:

``` r
remotes::install_github("ccs-amsterdam/amcat4r", ref = "3943ef527315e76205f258b34a3b9d14a67b5f72")
```
