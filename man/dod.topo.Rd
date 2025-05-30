% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topo.R
\name{dod.topo}
\alias{dod.topo}
\title{Download and Cache a topo File}
\usage{
dod.topo(
  west,
  east,
  south,
  north,
  resolution = 4,
  destdir = ".",
  destfile,
  server = "https://gis.ngdc.noaa.gov",
  debug = getOption("dodDebug", 0L)
)
}
\arguments{
\item{west, east}{numeric values for the limits of the data-selection box, in degrees.
These are converted to the -180 to 180 degree notation, if needed.
Then, \code{west} is rounded down to the nearest 1/100th degree, and \code{east}
is rounded up to the the nearest 1/100th degree. The results of these
operations are used in constructing the query for the NOAA data server.}

\item{south, north}{latitude limits, treated in a way that
corresponds to the longitude limits.}

\item{resolution}{numeric value of grid spacing, in geographical minutes.
The default value is 4 minutes, corresponding to 4 nautical miles (approx. 7.4km)
in the north-south direction, and less in the east-west direction.}

\item{destdir}{a character value indicating the directory
in which to store downloaded files.}

\item{destfile}{optional name of destination file. If not provided,
this function creates a reasonable name.}

\item{server}{character value specifying the base from which a
download URL will be constructed.  It is unlikely that any value
other than the default will work, unless it is a similarly-constructed
mirrored site.}

\item{debug}{an integer value indicating the level of debugging. If
this exceeds 0, then some debugging messages will be printed. This
value is passed down to related functions, but with 1 subtracted
for each pass.}
}
\value{
String indicating the full pathname to the downloaded file.
}
\description{
Topographic data are downloaded from a data server that holds the
ETOPO1 dataset (Amante, C. and B.W. Eakins, 2009), and saved as a
netCDF file whose name specifies the data request, if a file of
that name is not already present on the local file system.  The
return value is the name of the data file, and its typical use is
as the filename for a call to \code{read.topo()} in the \code{oce} package.
Given the rules on
file naming, subsequent calls to \code{\link[=dod.topo]{dod.topo()}} with identical
parameters will simply return the name of the cached file, assuming
the user has not deleted it in the meantime.  Note that
\code{\link[=dod.topo]{dod.topo()}} uses the \code{"terra"} and \code{"ncdf4"} packages,
so an error is reported if they are not available.
}
\details{
The specified longitude and latitude limits are rounded to 2 digits
(corresponding to a footprint of approximately 1km), and these are
used in the server request. If the resultant request would generate
under 1 row or column in the result, \code{\link[=dod.topo]{dod.topo()}} generates an
error message and stops.
}
\section{Historical note relating to NOAA server changes}{


2022 November 13: updated to new NOAA database, with 1/4-minute resolution (a
marked improvement over the previous 1-minute resolution).  The revision was
framed along similar changes to \code{marmap::getNOAAbathy()} made earlier today.
Thanks to Clark Richards for pointing this out!

2020 May 31: updated for a change in the NOAA query structure, taking
hints from \code{marmap::getNOAAbathy()}.
}

\examples{
# Download and plot topographic data
if (interactive()) { # sidestep a pkgdown::build_site() error
    library(oce)
    destdir <- tempdir()
    topoFile <- dod.topo(
        west = -66, east = -60, south = 43, north = 47,
        resolution = 1, destdir = destdir
    )
    topo <- read.topo(topoFile)
    imagep(topo, zlim = c(-400, 400), col = oceColorsTwo, drawTriangles = TRUE)
    if (requireNamespace("ocedata", quietly = TRUE)) {
        data(coastlineWorldFine, package = "ocedata")
        lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
    }
    unlink(destdir, recursive = TRUE)
}

}
\references{
\itemize{
\item Amante, C. and B.W. Eakins, 2009. ETOPO1 1 Arc-Minute Global Relief
Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum
NESDIS NGDC-24. National Geophysical Data Center, NOAA. doi:10.7289/V5C8276M
}
}
\seealso{
Other functions that download files: 
\code{\link{dod.amsr}()},
\code{\link{dod.buoy}()},
\code{\link{dod.coastline}()},
\code{\link{dod.ctd}()},
\code{\link{dod.ctd.bats}()},
\code{\link{dod.ctd.bbmp}()},
\code{\link{dod.ctd.gtspp}()},
\code{\link{dod.ctd.itp}()},
\code{\link{dod.met}()},
\code{\link{dod.tideGauge}()}
}
\author{
Dan Kelley
}
\concept{functions that download files}
