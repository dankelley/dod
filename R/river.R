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
#'
#' @param saveFile logical value indicating whether to save the file for later
#' use. This can be handy if the goal is to archive data, since the
#' server does not seem to provide archived information.  Please examine
#' the code of this function to see how to read the file, which is a bit
#' tricky.
#'
#' @param debug integer indicating the level of debugging information
#' that is printed during processing.  The default, `debug=0`, means
#' to work quietly.
#'
#' @return a data frame containing the data, with columns named `"time"` and
#' `"level"`.
#'
#' @examples
#' library(oce)
#' library(dod)
#' dir <- riverDirectory()
#' data <- riverData(id = dir$id)
#' oce.plot.ts(data$time, data$level,
#'     xaxs = "i",
#'     xlab = "", ylab = "Level [m]", lwd = 2,
#'     drawTimeRange = FALSE, grid = TRUE
#' )
#' mtext(sprintf("%s (%.4fN %.4fE)", dir$name, dir$latitude, dir$longitude),
#'     line = 0.2, cex = 0.7 * par("cex")
#' )
#'
#' @export
#'
#' @author Dan Kelley
riverData <- function(id = "01EJ001", region = "NS", interval = "daily", saveFile = FALSE, debug = 0) {
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
    file <- gsub(".*/", "", url)
    dodDebug(debug, "file = \"", file, "\"\n")
    if (saveFile) {
        download.file(url, file)
        message("saved file to \"", file, "\"")
    } else {
        file <- url
    }
    data <- read.csv(file, header = TRUE)
    timeString <- gsub("^(.*)([+-][0-9][0-9]:[0-9][0-9])$", "\\1", data[, 2])
    # Decode tz offset (maybe lubridate has a way to do this...)
    tzString <- gsub("^(.*)([+-][0-9][0-9]:[0-9][0-9])$", "\\2", data[, 2])
    stopifnot(all(tzString == tzString[1])) # don't permit daylight-saving shifts (unlikely)
    tzSign <- ifelse(substr(tzString, 1, 1) == "-", -1, 1)
    tmp <- strsplit(substr(tzString, 2, 6), ":")
    hour <- as.numeric(sapply(tmp, \(x) x[1]))
    minute <- as.numeric(sapply(tmp, \(x) x[2]))
    offset <- -tzSign * (hour + minute / 60) * 3600
    time <- as.POSIXct(timeString, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") + offset
    level <- data[, 3]
    data.frame(time = time, level = level)
} # riverData
