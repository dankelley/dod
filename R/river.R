#' Acquire a directory of river water-level files
#'
#' For examples, see [riverData()].
#'
#' @param url character value indicating the URL of a station listing.
#' If not provided, this defaults to
#' `https://hpfx.collab.science.gc.ca/today/hydrometric/doc/hydrometric_StationList.csv`
#' and this is the only URL on which the code is expected to work.
#'
#' @param station character value indicating the name of the station for
#' which information is sought.  This may be a regular expression. If not
#' provided, `"sackville.*bedford"` is used. Set `station` to NULL to
#' get a listing for all stations.
#'
#' @param debug integer indicating the level of debugging information
#' that is printed during processing.  The default, `debug=0`, means
#' to work quietly.
#'
#' @return a data frame holding columns named
#' `"id"`, `"name"`, `"latitude"`, `"region"` and `"tz", with
#' one row for each selected station.
#'
#' @export
#'
#' @author Dan Kelley
riverDirectory <- function(url, station, debug = 0) {
    if (missing(url)) {
        url <- "https://hpfx.collab.science.gc.ca/today/hydrometric/doc/hydrometric_StationList.csv"
    }
    if (missing(station)) {
        station <- "sackville.*bedford"
    }
    dodDebug(debug, "url=\"", url, "\"\n")
    if (length(url) != 1) stop("length of 'url' must be 1")
    if (length(station) != 1) stop("length of 'station' must be 1")
    dir <- read.csv(url, header = TRUE)
    # Change names to simpler values (no caps, no dots, no slashes, etc)
    names <- names(dir)
    names[grep("ID", names)] <- "id"
    names[grep("Name", names)] <- "name"
    names[grep("Latitude", names)] <- "latitude"
    names[grep("Longitude", names)] <- "longitude"
    names[grep("Prov", names)] <- "region"
    names[grep("Timez", names)] <- "tz"
    names(dir) <- names
    if (!is.null(station)) {
        dir[grep(station, dir$name, ignore.case = TRUE), ]
    } else {
        dir
    }
} # riverDirectory

#' Download and read a river water-level file
#'
#' If `saveFile` is TRUE, then the file is saved for later use.  Its
#' first line will be a header. A column named `"Date"` may be decoded
#' into a POSIX time value using [lubridate::ymd_hms()].
#'
#' @param id character value indicating the ID of the desired station. This
#' may be discovered by using [riverDirectory()] first. This
#' defaults to `"01EJ001"`, for the Sackville River at Bedford.
#'
#' @param region character value indicating the province or territory
#' in which the river gauge is sited. This defaults to `"NS`.
#'
#' @param interval character value, either `"daily"` or `"hourly",
#' indicating the time interval desired. (The first seems to yield
#' data in the current month, and the second seems to yield data
#' since over the last 1 or 2 days.)

#' @param saveFile logical value indicating whether to save the file for later
#' use. This can be handy because the server does provide archived data. The
#' filename will be as on the server, but with the main part of the filename
#' ending with a timestamp. This file name is printed during processing, and it
#' may be read later with [read.csv()], with its time column being decoded with
#' [lubridate::ymd_hms()]; perhaps the server has information on the meanings
#' of the other columns, or perhaps you will be able to guess.

#' @param debug integer indicating the level of debugging information
#' that is printed during processing.  The default, `debug=0`, means
#' to work quietly.
#'
#' @return a data frame containing the data, with columns named `"time"` and
#' `"level"` in m, and `"discharge"` in m^3/s. (Note that files contain
#' other columns; if you want them, save the file and read it
#' as explained in \sQuote{Details}.)
#'
#' @examples
#' library(oce)
#' library(dod)
#' dir <- riverDirectory() # defaults to Sackville River at Bedford
#' data <- riverData(id = dir$id)
#' # This 3-panel layout might be useful to river experts
#' layout(matrix(c(1, 3, 2, 3), 2, 2, byrow = TRUE), width = c(0.6, 0.4))
#' oce.plot.ts(data$time, data$level,
#'     xaxs = "i",
#'     xlab = "", ylab = "Level [m]",
#'     drawTimeRange = FALSE, grid = TRUE
#' )
#' mtext(sprintf("%s (%.4fN %.4fE)", dir$name, dir$latitude, dir$longitude),
#'     line = 0.2, cex = 0.7 * par("cex")
#' )
#' oce.plot.ts(data$time, data$discharge,
#'     xaxs = "i",
#'     xlab = "", ylab = expression("Discharge [" * m^3 / s * "]"),
#'     drawTimeRange = FALSE, grid = TRUE
#' )
#' with(data, plot(level, discharge, type = "l"))
#'
#' @export
#'
#' @importFrom lubridate ymd_hms
#'
#' @author Dan Kelley
riverData <- function(id = "01EJ001", region = "NS", interval = "daily",
                      saveFile = FALSE, debug = 0) {
    if (length(id) != 1L) stop("length of 'id' must be 1")
    if (length(region) != 1L) stop("length of 'region' must be 1")
    if (length(interval) != 1L) stop("length of 'interval' must be 1")
    url <- paste0(
        "https://hpfx.collab.science.gc.ca/today/hydrometric/csv/",
        region,
        "/",
        interval,
        "/",
        region,
        "_",
        id,
        "_",
        interval,
        "_hydrometric.csv"
    )
    dodDebug(debug, "url = \"", url, "\"\n")
    if (saveFile) {
        file <- paste0(gsub(".*/(.*).csv", "\\1", url), "_", Sys.Date(), ".csv")
        download.file(url, file)
        message("saved file to \"", file, "\"")
    } else {
        file <- url
    }
    data <- read.csv(file, header = TRUE)
    data.frame(
        time = lubridate::ymd_hms(data[, 2]), level = data[, 3],
        discharge = data[, 7]
    )
} # riverData
