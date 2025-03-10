% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/amsr.R
\name{dod.amsr}
\alias{dod.amsr}
\title{Download Advanced Microwave Scanning Radiometer data}
\usage{
dod.amsr(
  date = Sys.Date() - 4L,
  destdir = ".",
  server = "https://data.remss.com/amsr2/ocean/L3/v08.2",
  type = "3day",
  quiet = FALSE,
  debug = 0
)
}
\arguments{
\item{date}{either a Date object or a character or time object that
can be converted to such an object with \code{\link[=as.Date]{as.Date()}}.  The default
is four days prior to the present date, which is usually the latest
view that can be obtained, if \code{type} is \code{"3day"}, as is its default.}

\item{destdir}{character giving destination directory (defaults to \code{"."},
the present directory).  The directory must exist. The author usually
sets this to \code{"~/data/amsr"}, so that the file will be in a central
location.}

\item{server}{character value indicating the base server location. The
default value ought to be used unless the data provider changes their
web scheme ... but in that case, it is hoped that users will contact
the developers so that the package can be updated.}

\item{type}{character value indicating where to get the data.  This may be
\code{"3day"} (the default), for a composite covering 3 days of observation, which
removes most viewing-path and cloud blanks, \code{"daily"} for a daily reading,
\code{"weekly"} for a composite covering a week, or \code{"monthly"} for a composite
covering a month.  In the \code{"daily"} case, the data arrays are 3D, with the
third dimension representing ascending and descending traces, but in all the
other cases, the arrays are 2D.}

\item{quiet}{a logical value, passed to \code{\link[curl:curl_download]{curl::curl_download()}}, which does
the downloading.  If this is TRUE (the default), then the work is done
quietly. Until version version 0.1.12, this parameter was called \code{silent},
but it was renamed to match the corresponding argument in
\code{\link[curl:curl_download]{curl::curl_download()}}, which may be more familiar to users.}

\item{debug}{an integer value indicating the level of debugging. If
this exceeds 0, then some debugging messages will be printed. This
value is passed down to related functions, but with 1 subtracted
for each pass.}
}
\value{
\code{dod.amsr} returns a character value holding the full pathname
of the downloaded file.
}
\description{
This function downloads AMSR data.
}
\details{
This works by constructing URLs based on the arguments provided.  The author
is unaware of any documentation that specifies the directory structure on
the server, and so the construction is based on examining the server
with a web browser.  Obviously, this is a fragile approach that will
lead to failed downloads if the remote directory structure changes.
}
\section{Historical notes}{

\itemize{
\item Until September 2024 \code{\link[=dod.amsr]{dod.amsr()}} required 3 parameters to specify
a time (\code{year}, \code{month} and \code{day}). This was difficult to
use and also led to messy coding, so these 3 parameters were replaced
with \code{date}.
\item Until July 2023, \code{\link[=dod.amsr]{dod.amsr()}} worked by calling \code{\link[oce:download.amsr]{oce::download.amsr()}}.
However, at that time, the author noticed changes in both the directory
structure of the remote server, and the format of the data files.
The new directory structure was addressed by a complete rewrite
of the code within \code{dod}, and a severing of the connection to the
\code{oce} function.
}
}

\examples{
if (interactive()) { # sidestep a pkgdown::build_site() error
    library("oce")
    # Get temporary space (to obey CRAN rules)
    destdir <- tempdir()
    file <- dod.amsr(destdir = destdir)
    natl <- read.amsr(file) |>
        subset(-90 < longitude & longitude < 0) |>
        subset(20 < latitude & latitude < 70)
    plot(natl)
    # Clean up space
    unlink(destdir, recursive = TRUE)
}

}
\seealso{
Other functions that download files: 
\code{\link{dod.buoy}()},
\code{\link{dod.coastline}()},
\code{\link{dod.ctd}()},
\code{\link{dod.ctd.bats}()},
\code{\link{dod.ctd.bbmp}()},
\code{\link{dod.ctd.gtspp}()},
\code{\link{dod.ctd.itp}()},
\code{\link{dod.met}()},
\code{\link{dod.tideGauge}()},
\code{\link{dod.topo}()}
}
\author{
Dan Kelley
}
\concept{functions that download files}
