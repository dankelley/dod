#' Download CTD data from the Ice-Tethered Profiler program
#'
#' [dod.ctd.itp()] downloads CTD data from the Woods Hole Oceanographic
#' Institutions Ice-Thethered Profile program
#' <https://www2.whoi.edu/site/itp/>.  Note that this
#' server does not provide a searchable index, so users will need to visit the website
#' <https://www2.whoi.edu/site/itp/data/> to determine IDs of interest. Please
#' take note of the \sQuote{Historical update} section, regarding whether to
#' supply `ID` (with or without `info`) or to supply `url`.
#'
#' @param ID an integer specifying the ID of the profiler that is of interest.
#'
#' @param info a logical value.  If `info` is FALSE, which is the default, then
#' [dod.ctd.itp()] downloads the zip-format file that can be expanded into
#' a directory that holds a data file for each profile made by the
#' profiler with the given `ID`.  The name of the zip file is returned.
#' (This file must be unzipped to access the data; [unzip()] is a convenient
#' way to do this.)
#' On the other hand, if `info` is TRUE, then
#' nothing is downloaded, and [dod.ctd.itp()] returns a
#' URL holding information about the profiler.
#'
#' @param url optional character value supplying the full URL for the sought-after
#' file.  If this is supplied, then both `ID` and `info` are ignored.  See
#' \dQuote{Historical notes} to learn more about this parameter.
#'
#' @param file character value giving the name to be used for the downloaded
#' file.  If `file` is NULL, which is the default, then the filename on the
#' server is used.
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template quietTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv
#'
#' @return [dod.ctd.itp()] returns the full pathname of the
#' downloaded zip file, or a URL of an information site,
#' depending on the value of `info`.
#'
#' @section Historical notes:
#'
#' * 2025-08-19. The `url` parameter was added, when it was discovered that the
#' organizational structure of the remote-server directories had changed
#' compared with that upon which the code for `ID` and `info` was written.
#' It is possible that the code that handles `ID` and `info` will be updated
#' for the new organizational structure, but, even if it is, users
#' may be better-off by exploring the server website and isolating files
#' of interest that way. Supplying `url` is also advantageous because it
#' lets the user control what version of data are to be downloaded,
#' as in the \dQuote{Worked Example}.
#'
#' @section Worked Example:
#'
#' Note that the following code is not run during the package build
#' process, as it downloads a large file. It was last run, and the
#' resultant PNG created, on 2025-08-19.
#'
#' ```r
#' library(dod)
#' library(oce)
#' url <- paste0(
#'     "https://scienceweb.whoi.edu/itp/data/",
#'     "itpsys137/itp137_level2.nc"
#' )
#' nc <- dod.ctd.itp(url = url, age = 1)
#' d <- oce::read.netcdf(nc)
#' time <- as.POSIXct("1970-01-01") + 86400 * d[["profile_info/time"]]
#' lat <- d[["profile_info/latitude"]]
#' lon <- d[["profile_info/longitude"]]
#' png("itp.png")
#' data(coastlineWorld)
#' par(mar = c(2, 2, 2, 1))
#' mapPlot(coastlineWorld,
#'     longitudelim = c(-180, 180), latitudelim = c(70, 90),
#'     projection = "+proj=stere +lat_0=90 +lat_ts=90 +lon_0=-130",
#'     col = "lightgray"
#' )
#' col <- cut(time, "year")
#' mapPoints(lon, lat, cex = 0.3, pch = 20, col = col)
#' mapScalebar("topleft", length = 1000)
#' ```
#'
#' \if{html}{\figure{itp.png}{options: width="400" alt="Figure: itp.png"}}
#'
#' @export
#'
#' @family functions that download files
#'
#' @author Dan Kelley
dod.ctd.itp <- function(ID, info = FALSE, url,
                        file = NULL, destdir = ".", age = 0, quiet = FALSE, debug = 0) {
    # For ITP 135, info is at
    # https://www2.whoi.edu/site/itp/data/active-systems/itp-135/
    # and data is at
    # https://scienceweb.whoi.edu/itp/data/itp135grddata.zip
    #
    # dod.ctd.itp(url="https://scienceweb.whoi.edu/itp/data/itpsys137/itp137_level2.nc", debug=1)
    dodDebug(debug, "dod.ctd.itp() START\n")
    if (!missing(url)) {
        dodDebug(debug, "  user-supplied url=\"", url, "\"\n", sep = "")
        if (is.null(file)) {
            file <- gsub(".*/", "", url)
        }
        dodDebug(debug, "  file=\"", file, "\"\n", sep = "")
        filename <- try(
            dod.download(
                url = url, destdir = destdir,
                file = file, age = age, quiet = quiet, debug = incrementDebug(debug)
            ),
            silent = quiet
        )
        return(filename)
    }
    if (missing(ID)) {
        stop("Either 'ID' or 'ur' must be supplied")
    }
    owarn <- options("warn")$warn
    options(warn = -1)
    IDorig <- ID
    if (!is.null(url)) {
        ID <- as.integer(ID)
    }
    options(warn = owarn)
    if (is.na(ID)) {
        stop("ID=", IDorig, " is neither an integer nor a string denoting an integer")
    }
    infoURL <- sprintf("https://www2.whoi.edu/site/itp/data/active-systems/itp-%s", ID)
    if (info) {
        return(infoURL)
    }
    dataURL <- sprintf("https://scienceweb.whoi.edu/itp/data/itp%sgrddata.zip", ID)
    dodDebug(debug, "infoURL=\"", infoURL, "\"\n", sep = "")
    dodDebug(debug, "dataURL=\"", dataURL, "\"\n", sep = "")
    if (is.null(file)) {
        file <- gsub(".*/", "", dataURL)
    }
    # Try e.g. b50056_ctd.txt and if that fails, try b50056_ctd_QC.txt
    filename <- try(dod.download(url = dataURL, destdir = destdir, file = file, age = age, quiet = quiet, debug = incrementDebug(debug)),
        silent = quiet
    )
    if (inherits(filename, "try-error")) {
        stop("Unable to download \"", dataURL, "\" to \"", filename, "\"")
    }
    dodDebug(debug, "  END dod.ctd.itp()\n")
    return(filename)
}
