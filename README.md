
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dod

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dod)](https://CRAN.R-project.org/package=dod)
[![R-CMD-check](https://github.com/dankelley/dod/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/dankelley/dod/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of dod is to make it easier to download various types of
oceanographic data from common sources such as BATS, NOAA, MEDS, BBMP.
Some similar functions exist in the oce package, but the intention is to
retire them as dod matures, because building oce is difficult for some
users, and CRAN policies dictate against frequent updates.

## Installation

You can install the development version of dod from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dankelley/dod")
```

## Example

The following example shows how to (1) download an index of CTD data
files resulting from observations made as part of the BBMP program in
the present year and then (2) use functions in the oce package to read
and plot the first CTD profile of year 2024.

``` r
library(dod)
#> Loading required package: lubridate
#> 
#> Attaching package: 'lubridate'
#> The following objects are masked from 'package:base':
#> 
#>     date, intersect, setdiff, union
library(oce)
#> Loading required package: gsw
# Note: cannot specify year=2025 because the URL is differently constructed
index <- dod.ctd.bbmp.index(year = "2024")
ctdFile <- dod.ctd.bbmp(year = index$year[1], ID = index$ID[1], direction = "DN")
# Use oce to read, summarize and plot the data.
ctd <- read.netcdf(ctdFile) |>
    rename() |>
    as.ctd()
# Plot some biochemistry variables
par(mfrow = c(2, 2))
plotProfile(ctd, "SA")
plotProfile(ctd, "CT")
plotProfile(ctd, "oxygen")
plotProfile(ctd, "chlorophyllA")
```

<img src="man/figures/README-example-1.png" width="100%" />

PS. This `README.md` file was created on 2025-04-21 by rendering the
`README.Rmd` file with `devtools::build_readme()`.
