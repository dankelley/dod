% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/met.R
\name{dod.met}
\alias{dod.met}
\title{Download meteorological timeseries data}
\usage{
dod.met(
  id,
  year,
  month,
  deltat,
  type = "xml",
  destdir = ".",
  destfile,
  age = 0,
  quiet = FALSE,
  debug = 0
)
}
\arguments{
\item{id}{A number giving the "Station ID" of the station of interest. If not
provided, \code{id} defaults to 6358, for Halifax International Airport. See
\dQuote{Details}.}

\item{year}{A number giving the year of interest. Ignored unless \code{deltat}
is \code{"hour"}. If \code{year} is not given, it defaults to the present year.}

\item{month}{A number giving the month of interest. Ignored unless \code{deltat}
is \code{"hour"}. If \code{month} is not given, it defaults to the present
month.}

\item{deltat}{Optional character string indicating the time step of the
desired dataset. This may be \code{"hour"} or \code{"month"}.
If \code{deltat} is not given, it defaults to \code{"hour"}.}

\item{type}{String indicating which type of file to download, either
\code{"xml"} (the default) for an XML file or \code{"csv"} for a CSV file.}

\item{destdir}{a character value indicating the directory
in which to store downloaded files.}

\item{destfile}{optional name of destination file. If not provided,
this function creates a reasonable name.}

\item{age}{a numerical value indicating a time interval, in days.
If the file to be downloaded from the server already exists locally,
and was created less than \code{age} days in the past, it will not be
downloaded again.  Setting \code{age=0} (which is the default)
forces a download, so that existing files will always be updated.
By contrast, setting \code{age} to a negative number prevents the
updating of files that already exist locally.}

\item{quiet}{Logical value passed to \code{\link[=download.file]{download.file()}}; a \code{TRUE} value
silences output.}

\item{debug}{an integer value indicating the level of debugging. If
this exceeds 0, then some debugging messages will be printed. This
value is passed down to related functions, but with 1 subtracted
for each pass.}
}
\value{
\code{\link[=dod.met]{dod.met()}} returns a character value holding the full
pathname of the downloaded file.
}
\description{
\code{\link[=dod.met]{dod.met()}} attempts to download data from Environment Canada's
historical-data website, and to cache the files locally. Lacking a published
API, this function must rely on reverse-engineering of queries handled by
that web server.  For that reason, it is brittle.
}
\details{
If this function fails, users might try using Gavin Simpson's \code{canadaHCD}
package (reference 2). That package maintains a copy of the Environment
Canada listing of stations, and its \code{find_station()} function provides an easy
way to determine Station IDs.  After that, its \code{hcd_hourly} function (and
related functions) make it easy to read data. These data can then be
converted to the \code{met} class with \code{as.met()} in the \code{oce} package,
although doing so leaves many important metadata blank.
}
\examples{
\dontrun{
library(dod)
# Download data for Halifax International Airport, in September
# of 2003. This dataset is used for data(met) provided with oce.
# Note that requests for data after 2012 month 10 yield all
# missing values, for reasons unknown to the author.
metFile <- dod.met(6358, 2003, 9, destdir=".")
}

}
\references{
\enumerate{
\item Environment Canada website for Historical Climate Data
\verb{https://climate.weather.gc.ca/index_e.html}
\item Gavin Simpson's \code{canadaHCD} package on GitHub
\verb{https://github.com/gavinsimpson/canadaHCD}
}
}
\seealso{
Other functions that download files: 
\code{\link{dod.coastline}()},
\code{\link{dod.topo}()}
}
\author{
Dan Kelley
}
\concept{functions that download files}
\concept{things related to met data}
