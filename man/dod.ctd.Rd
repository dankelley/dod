% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctd.R
\name{dod.ctd}
\alias{dod.ctd}
\title{Download CTD data}
\usage{
dod.ctd(program = NULL, ...)
}
\arguments{
\item{program}{character value naming the program (one of listthem).}

\item{...}{extra arguments passed to \code{\link[=dod.ctd.bats]{dod.ctd.bats()}}, \code{\link[=dod.ctd.bbmp]{dod.ctd.bbmp()}} or
\code{\link[=dod.ctd.gtspp]{dod.ctd.gtspp()}}.}
}
\description{
This function downloads CTD data from various programs.
}
\details{
\tabular{lll}{
   \strong{Project} \tab \strong{Program} \tab \strong{Function Called} \cr
   Bedford Basin Mooring Project \tab \code{BBMP} \tab \code{\link[=dod.ctd.bbmp]{dod.ctd.bbmp()}} \cr
   Bermuda Atlantic Time Series \tab \code{BATS} \tab \code{\link[=dod.ctd.bats]{dod.ctd.bats()}} \cr
   Global Temperature and Salinity Profile Programme \tab \code{GTSPP} \tab \code{\link[=dod.ctd.gtspp]{dod.ctd.gtspp()}} \cr
   Ice-Tethered Profile \tab \code{GTSPP} \tab \code{\link[=dod.ctd.itp]{dod.ctd.itp()}} \cr
}
}
\seealso{
Other functions that download files: 
\code{\link{dod.amsr}()},
\code{\link{dod.buoy}()},
\code{\link{dod.coastline}()},
\code{\link{dod.ctd.bats}()},
\code{\link{dod.ctd.bbmp}()},
\code{\link{dod.ctd.gtspp}()},
\code{\link{dod.ctd.itp}()},
\code{\link{dod.met}()},
\code{\link{dod.tideGauge}()},
\code{\link{dod.topo}()}
}
\concept{functions that download files}
