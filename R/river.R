#' Acquire a directory of river water-level files
#'
#' For examples, see [dod.river()].
#'
#' @param url character value indicating the URL of a station listing.
#' If not provided, this defaults to
#' `https://hpfx.collab.science.gc.ca/today/hydrometric/doc/hydrometric_StationList.csv`
#' and this is the only URL on which the code is expected to work.
#'
#' @param name character value indicating the name of the station for
#' which information is sought.  This may be a regular expression. If not
#' provided, `"sackville.*bedford"` is used. Set `name` to NULL to
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
dod.river.index <- function(url, name, debug = 0) {
    if (missing(url)) {
        url <- "https://hpfx.collab.science.gc.ca/today/hydrometric/doc/hydrometric_StationList.csv"
    }
    if (missing(name)) {
        name <- "sackville.*bedford"
    }
    dodDebug(debug, "url=\"", url, "\"\n")
    if (length(url) != 1) stop("length of 'url' must be 1")
    if (length(name) != 1) stop("length of 'name' must be 1")
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
    if (!is.null(name)) {
        dir[grep(name, dir$name, ignore.case = TRUE), ]
    } else {
        dir
    }
} # dod.river.index

#' Download and read a river water-level file
#'
#' If `saveFile` is TRUE, then the file is saved for later use.  Its
#' first line will be a header. A column named `"Date"` may be decoded
#' into a POSIX time value using [lubridate::ymd_hms()].
#'
#' @param id character value indicating the ID of the desired station. This
#' may be discovered by using [dod.river.index()] first. This
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
#' use. This can be handy because the server does provide archived data. The
#' filename will be as on the server, but with the main part of the filename
#' ending with a timestamp. This file name is printed during processing, and it
#' may be read later with [read.csv()], with its time column being decoded with
#' [lubridate::ymd_hms()]; perhaps the server has information on the meanings
#' of the other columns, or perhaps you will be able to guess.
#'
#' @param debug integer indicating the level of debugging information
#' that is printed during processing.  The default, `debug=0`, means
#' to work quietly.
#'
#' @return a data frame containing the data, with columns named `"time"` and
#' `"level"` (m), `"grade"` and `"discharge"` (m^3/s). (Note that files contain
#' other columns; if you want them, save the file and read it
#' as explained in \sQuote{Details}.)
#'
#' @examples
#' library(oce)
#' library(dod)
#' dir <- dod.river.index() # defaults to Sackville River at Bedford
#' data <- dod.river(id = dir$id)
#'
#' # Plot a 3-panel summary graph; see e.g. Gore and Banning (2017) for more
#' # information on discharge.
#'
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
#' plot(data$discharge, data$level,
#'     type = "l",
#'     xlab = expression("Discharge [" * m^3 / s * "]"), ylab = "Level [m]"
#' )
#'
#' @references
#'
#' Gore, James A., and James Banning. “Chapter 3 - Discharge Measurements and
#' Streamflow Analysis.” In Methods in Stream Ecology, Volume 1 (Third
#' Edition), edited by F. Richard Hauer and Gary A. Lamberti, 49–70. Boston:
#' Academic Press, 2017. https://doi.org/10.1016/B978-0-12-416558-8.00003-2.
#'
#' @export
#'
## @importFrom lubridate ymd_hms
#'
#' @author Dan Kelley
dod.river <- function(id = "01EJ001", region = "NS", interval = "daily",
                      saveFile = FALSE, debug = 0) {
    if (length(id) != 1L) stop("length of 'id' must be 1")
    if (length(region) != 1L) stop("length of 'region' must be 1")
    if (length(interval) != 1L) stop("length of 'interval' must be 1")
    if (!requireNamespace("lubridate", quietly = TRUE)) {
        stop("must install.packages(\"lubridate\") before using dod.river()")
    }

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
        grade = data[, 4], discharge = data[, 7]
    )
} # dod.river.

#' Download American river-gauge data from USGS
#'
#' @param id character value giving the numeric code for the gauge. This
#' defaults to a river in Ohio.
#'
#' @param start an indication of the start time of the requested data window.
#' If provided, this may be in a (non-ambiguous) character form or, better,
#' in a POSIXt object. The "UTC" timezone is assumed.  If not provided,
#' `start` defaults to 1 week before the present time.
#'
#' @param end as for `start`, but instead for the end date.
#'
#' @examples
#' library(dod)
#' file <- dod.river.usgs(start = "2025-08-01", end = "2025-08-08")
#' lines <- readLines(file)
#' skip <- 1 + grep("agency_cd", lines)
#' data <- read.delim(text = lines, skip = skip, sep = "\t", header = FALSE)
#' # Note that the data at this site, if downloaded during daylight-savings
#' # time, are offset from UTC by 4 hours. I'm not sure how to make the server
#' # return in UTC.
#' time <- as.POSIXct(data$V3, tz = "UTC") + 4 * 3600
#' # Convert from feet to metres
#' height <- 0.3048 * data$V5
#' plot(time, height, type = "l")
#' mtext(paste("Station", data$V2[1]))
#' file.remove(file) # needed for tests on CRAN
#'
#' @export
#'
#' @author Dan Kelley
dod.river.usgs <- function(id = "03242350", start = NULL, end = NULL) {
    if (length(id) != 1L) stop("length of 'id' must be 1")
    if (xor(is.null(end), is.null(start))) {
        stop("if either 'start' or 'end' is NULL, then both must be NULL")
    }
    if (is.null(start)) {
        end <- as.POSIXlt(Sys.time(), tz = "UTC")
        start <- end - 7 * 86400
    }
    start <- format(as.POSIXct(start), "%Y-%m-%dT%H:%M:%S")
    end <- format(as.POSIXct(end), "%Y-%m-%dT%H:%M:%S")
    url <- paste0(
        "https://waterservices.usgs.gov/nwis/iv/?",
        "sites=", id, "&",
        "agencyCd=USGS&",
        # "startDT=", start, "-", sprintf("%02d", hourOffset), "&", # 2025-07-31T18:35:50.593-04:00&",
        "startDT=", start, "&", # 2025-07-31T18:35:50.593-04:00&",
        # "endDT=", end, "-", sprintf("%02d", hourOffset), "&", # 2025-07-31T18:35:50.593-04:00&",
        "endDT=", end, "&", # 2025-07-31T18:35:50.593-04:00&",
        "parameterCd=00065&format=rdb"
    )
    file <- paste0("river_usgs_", id, "_", start, "_", end, ".csv")
    download.file(url, file)
    file
}
