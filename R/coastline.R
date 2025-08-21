#' Download a coastline File
#'
#' Constructs a query to the NaturalEarth server (see reference 1) to download coastline
#' data (or lake data, river data, etc) in any of three resolutions.
#'
#' @param resolution A character value specifying the desired resolution. The permitted
#' choices are `"10m"` (for 1:10M resolution, the most detailed),
#' `"50m"` (for 1:50M resolution)
#' and `"110m"` (for 1:110M resolution). If `resolution` is not supplied,
#' `"50m"` will be used.
#'
#' @param item A character value indicating the quantity to be downloaded.
#' This is normally one of `"coastline"`, `"land"`, `"ocean"`,
#' `"rivers_lakes_centerlines"`, or `"lakes"`, but the NaturalEarth
#' server has other types, and advanced users can discover their names by inspecting
#' the URLs of links on the NaturalEarth site, and use them for `item`.
#' If `item` is not supplied, it defaults to `"coastline"`.
#'
#' @template destdirTemplate
#'
#' @param destfile optional name of destination file. If not provided,
#' this function creates a reasonable name.
#'
#' @param server A character value specifying the server that is to supply
#' the data. At the moment, the only permitted value is `"naturalearth"`,
#' which is the default if `server` is not supplied.
#'
#' @template debugTemplate
#'
#' @return A character value indicating the (ziip) filename of the result,
#' or an empty string, if there is a problem.
#'
#' @examples
#'
#' # Download, unzip, read file, and then erase (as per CRAN policies)
#' if (interactive()) { # pkgdown::build_site() cannot handle downloads
#'     library(dod)
#'     # NOTE: data file is removed at end, to pass CRAN checks
#'     destdir <- tempdir(check = TRUE)
##     cat("\ndestdir: ", destdir, "\n", file = stderr())
#'     zip <- dod.coastline(destdir = destdir, debug = 3)
#'     unzip(zip, exdir = destdir)
#'     list.files(destdir) # note the .shp file
#'     shpfile <- list.files(destdir, ".shp$", full.names = TRUE)
##     cat("\nshpfile: ", shpfile, "\n", file = stderr())
#'     if (requireNamespace("oce", quietly = TRUE)) {
##         cat("\nDAN 1\n", file = stderr())
#'         library(oce)
##         cat("\nDAN 2\n", file = stderr())
#'         cl <- read.coastline(shpfile, type = "shapefile")
##         cat("\nDAN 3\n", file = stderr())
#'         plot(cl)
##         cat(class(cl), "\n", file = stderr())
##         cat("\nDAN 4\n", file = stderr())
#'     }
##     cat("\nDAN 5\n", file = stderr())
#'     unlink(destdir, recursive = TRUE, force = TRUE)
##     #file.remove(destdir)
##     cat("\nDAN 6\n", file = stderr())
#' }
#'
#' @references
#' 1. The NaturalEarth website is at `https://www.naturalearthdata.com` but the
#' files are available at e.g.
#' <https://naturalearth.s3.amazonaws.com/50m_physical/ne_50m_coastline.zip>
#'
#' @family functions that download files
#' @export
#' @author Dan Kelley
dod.coastline <- function(resolution, item = "coastline", destdir = ".", destfile, server = "naturalearth", debug = 0) {
    if (missing(resolution)) {
        resolution <- "50m"
    }
    resolutionChoices <- c("10m", "50m", "110m")
    if (!(resolution %in% resolutionChoices)) {
        stop("'resolution' must be one of: '", paste(resolutionChoices, collapse = "' '"), "'")
    }
    if (server == "naturalearth") {
        urlBase <- "https://naturalearth.s3.amazonaws.com/"
    } else {
        stop("the only server that works is naturalearth")
    }
    filename <- paste("ne_", resolution, "_", item, ".zip", sep = "")
    if (missing(destfile)) {
        destfile <- filename
    }
    # https://naturalearth.s3.amazonaws.com/50m_physical/ne_50m_coastline.zip
    url <- paste0(urlBase, resolution, "_physical/ne_", resolution, "_", item, ".zip")
    dodDebug(debug, "attempting to download ", url, "\n", sep = "")
    destination <- paste(destdir, destfile, sep = "/")
    if (1 == length(list.files(path = destdir, pattern = paste("^", destfile, "$", sep = "")))) {
        dodDebug(debug, "Not downloading", destfile, "because it is already present in", destdir, "\n")
    } else {
        download.file(url, destination)
        dodDebug(debug, "Downloaded file stored as '", destination, "'\n", sep = "")
    }
    # The following is a sample URL, from which I reverse-engineered the URL construction.
    #    https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_lakes.zip
    destination
}
