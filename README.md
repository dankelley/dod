
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

The following example shows how to (1) download an index of ctd data
files resulting from observations made as part of the BBMP program in
the present year and then (2) use functions in the oce package to read
and plot the last observations in the dataset.

``` r
library(dod)
# 1. Get index
library(dod)
year <- format(Sys.Date(), "%Y")
indexFile <- dod.ctd("BBMP", year, index = TRUE)
index <- read.csv(indexFile, header = TRUE, skip = 2)
# see first few lines to discover column names
head(index, 3)
#>            FILE     START_DATE_TIME
#> 1 D24667001.ODF 2024-01-03 13:01:41
#> 2 D24667002.ODF 2024-01-09 13:10:48
#> 3 D24667003.ODF 2024-01-17 12:55:39
# 2. Get the most recent file and plot the data
item <- tail(index, 1)$FILE
file <- dod.ctd("BBMP", year, item)
library(oce)
#> Loading required package: gsw
d <- read.ctd(file)
# 3. Standard CTD plot
plot(d)
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
# 4. Check also biochemistry variables
par(mfrow = c(2, 2))
plotProfile(d, "SA")
plotProfile(d, "CT")
plotProfile(d, "oxygen")
plotProfile(d, "fluorescence")
```

<img src="man/figures/README-example-2.png" width="100%" />

PS. This `README.md` file was created on 2024-04-02 by rendering the
`README.Rmd` file with `devtools::build_readme()`.
