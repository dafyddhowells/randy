
<!-- README.md is generated from README.Rmd. Please edit that file -->

# randy

<!-- badges: start -->
<!-- badges: end -->

The goal of randy is to …

## Installation

You can install the development version of randy from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dafyddhowells/randy")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(randy)
#> Warning: replacing previous import 'data.table::last' by 'dplyr::last' when
#> loading 'randy'
#> Warning: replacing previous import 'data.table::first' by 'dplyr::first' when
#> loading 'randy'
#> Warning: replacing previous import 'data.table::between' by 'dplyr::between'
#> when loading 'randy'
#> Warning: replacing previous import 'data.table::month' by 'lubridate::month'
#> when loading 'randy'
#> Warning: replacing previous import 'data.table::hour' by 'lubridate::hour' when
#> loading 'randy'
#> Warning: replacing previous import 'data.table::quarter' by 'lubridate::quarter'
#> when loading 'randy'
#> Warning: replacing previous import 'data.table::week' by 'lubridate::week' when
#> loading 'randy'
#> Warning: replacing previous import 'data.table::year' by 'lubridate::year' when
#> loading 'randy'
#> Warning: replacing previous import 'data.table::wday' by 'lubridate::wday' when
#> loading 'randy'
#> Warning: replacing previous import 'data.table::second' by 'lubridate::second'
#> when loading 'randy'
#> Warning: replacing previous import 'data.table::minute' by 'lubridate::minute'
#> when loading 'randy'
#> Warning: replacing previous import 'data.table::mday' by 'lubridate::mday' when
#> loading 'randy'
#> Warning: replacing previous import 'data.table::yday' by 'lubridate::yday' when
#> loading 'randy'
#> Warning: replacing previous import 'data.table::isoweek' by 'lubridate::isoweek'
#> when loading 'randy'
#> Warning: replacing previous import 'magrittr::extract' by 'tidyr::extract' when
#> loading 'randy'
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
