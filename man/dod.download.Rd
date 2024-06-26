% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/download.R
\name{dod.download}
\alias{dod.download}
\title{Download a file with error checking}
\usage{
dod.download(
  url = NULL,
  file = NULL,
  destdir = ".",
  age = 0,
  silent = TRUE,
  debug = 0
)
}
\arguments{
\item{url}{character value giving the web address of a file to be
downloaded. This has different values for different file types.}

\item{file}{character value giving the name to be used for
the downloaded file.}

\item{destdir}{a character value indicating the directory
in which to store downloaded files.}

\item{age}{a numerical value indicating a time interval, in days.
If the file to be downloaded from the server already exists
locally, and was created less than \code{age} days in the past, it will
not be downloaded again.  Setting \code{age=0} forces a download, so
that existing files will always be updated. By contrast, setting
\code{age} to a negative number prevents the updating of files that
already exist locally, regardless of their age.}

\item{silent}{logical value passed to \code{\link[=download.file]{download.file()}}, which
does the downloading.  The default, TRUE, indicates not to show
a progress indicator.}

\item{debug}{an integer value indicating the level of debugging. If
this exceeds 0, then some debugging messages will be printed. This
value is passed down to related functions, but with 1 subtracted
for each pass.}
}
\value{
\code{dod.download} returns a character value holding the full
name of the file, including the path to \code{destdir}.
}
\description{
\code{dod.download()} is mainly intended for use by other functions
in the \code{dod} package.
}
