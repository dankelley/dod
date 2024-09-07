# vim:textwidth=200:expandtab:shiftwidth=4:softtabstop=4

#' Download Advanced Microwave Scanning Radiometer data
#'
#' This function downloads AMSR data.
#'
#' This works by constructing URLs based on the arguments provided.  The author
#' is unaware of any documentation that specifies the directory structure on
#' the server, and so the construction is based on examining the server
#' with a web browser.  Obviously, this is a fragile approach that will
#' lead to failed downloads if the remote directory structure changes.  Indeed,
#' [dod.amsr()] was completely rewritten in July 2023, because a previous version
#' was seen to fail on that date.  Users are asked to report any failed downloads
#' they encounter.  Careful inspection of the documentation for `year`, `month`
#' and `day` is, of course, the first step in debugging errors.
#'
#' @param year,month,day integer values indicating the desired observation time.
#' If `year` is a Date object then `month` and `day` are ignored, because they will be
#' inferred automatically. (The default is to set `year` to [Sys.Date()]-4,
#' meaning 4 days in the past, so that a 3day type will work.)
#' Otherwise, the appropriate `year`, `month` and `day`
#' parameters depend on the `type`.  All three must be given (as integers)
#' if `type` is `"3day"`, `"daily"` or `"weekly"`.  But only the first two are
#' to be given if `type` is `"monthly"`.  Note that the server changes
#' without notice, and so `dod.amsr()` needs modification from time to time,
#' usually in the meanings of these three parameters, and of `type`.
#'
#' @param destdir character giving destination directory (defaults to `"."`, the present
#' directory).  The directory must exist.  (The author uses `"~/data/amsr"`.)
#'
#' @param server character value indicating the base server location. The
#' default value ought to be used unless the data provider changes their
#' web scheme ... but in that case, it is hoped that users will contact
#' the developers so that the package can be updated.
#'
#' @param type character value indicating where to get the data.  This may be
#' `"3day"` (the default), for a composite covering 3 days of observation, which
#' removes most viewing-path and cloud blanks, `"daily"` for a daily reading,
#' `"weekly"` for a composite covering a week, or `"monthly"` for a composite
#' covering a month.  In the `"daily"` case, the data arrays are 3D, with the
#' third dimension representing ascending and descending traces, but in all the
#' other cases, the arrays are 2D.
#'
#' @template quietTemplate
#'
#' @template debugTemplate
#'
#' @return `dod.amsr` returns a character value holding the full pathname
#' of the downloaded file.
#'
#' @section Historical note:
#' Until July 2023, [dod.amsr()] worked by calling `download.amsr()`
#' from the `oce` package.
#' However, at that time, the author noticed changes in both
#' the directory structure of the remote server, and the format of the
#' data files. The new directory structure was addressed by a complete
#' rewrite of the code within `dod`, and a severing of the connection
#' to the `oce` function.
#'
#' @examples
#' # This example is not run because it downloads a 12Mb file.
#' \dontrun{
#' if (dir.exists("~/data/amsr")) {
#'     library(dod)
#'     library(oce)
#'     library(ncdf4)
#'     file <- dod.amsr(destdir = "~/data/amsr")
#'     nc <- nc_open(file)
#'     lon <- ncvar_get(nc, "lon")
#'     lat <- ncvar_get(nc, "lat")
#'     SST <- ncvar_get(nc, "SST")
#'     U <- ncvar_get(nc, "wind_speed_AW")
#'     par(mfrow = c(2, 1))
#'     imagep(lon, lat, SST, asp = 1, col = oceColorsTurbo, xaxs = "i")
#'     mtext("SST [degC]")
#'     imagep(lon, lat, U, asp = 1, zlim = c(0, 15), col = oceColorsTurbo, xaxs = "i")
#'     mtext("Wind [m/s]")
#'     nc_close(nc)
#' }
#' }
#'
#' @export
#'
#' @author Dan Kelley
dod.amsr <- function(
    year = Sys.Date() - 4, month, day, destdir = ".",
    server = "https://data.remss.com/amsr2/ocean/L3/v08.2",
    type = "3day",
    quiet = FALSE,
    debug = 0) {
    dodDebug(debug, "dod.amsr(type=\"", type, "\", ...) {\n", sep = "")
    if (!type %in% c("3day", "daily", "weekly", "monthly")) {
        stop("type='", type, "' not permitted; try '3day', 'daily', 'weekly' or 'monthly'")
    }
    if (is.character(year)) {
        year <- as.Date(year)
    }
    if (inherits(year, "Date")) {
        t <- as.POSIXlt(year)
        year <- 1900 + t$year
        month <- 1 + t$mon
        day <- t$mday
    }
    # If year, month, day not given, default to 3 days ago.
    if (missing(month)) {
        stop("month must be provided, unless year is a date")
    }
    if (type %in% c("3day", "daily") && missing(day)) {
        stop("day must be provided for type of '3day' or 'daily'")
    }
    # convert to integers (needed for formatting URLs, below)
    year <- as.integer(year)
    month <- as.integer(month)
    day <- as.integer(day)
    if (type %in% c("3day", "daily")) {
        # https://data.remss.com/amsr2/ocean/L3/v08.2/3day/2023/RSS_AMSR2_ocean_L3_3day_2023-07-24_v08.2.nc
        # ^                                           ^    ^                       ^    ^    ^  ^
        # server                                      type year                  type year month day
        url <- sprintf(
            "%s/%s/%d/RSS_AMSR2_ocean_L3_%s_%04d-%02d-%02d_v08.2.nc",
            server, type, year, type, year, month, day
        )
    } else if (identical(type, "weekly")) {
        ymd <- sprintf("%4d-%02d-%02d", year, month, day)
        dodDebug(debug, "user-provided ymd=\"", ymd, "\"\n")
        # https://data.remss.com/amsr2/ocean/L3/v08.2/weekly/RSS_AMSR2_ocean_L3_weekly_2023-07-15_v08.2.nc
        # ^                                           ^                            ^    ^
        # server                                      type                       type   ymd
        url <- sprintf(
            "%s/%s/RSS_AMSR2_ocean_L3_%s_%s_v08.2.nc",
            server, type, type, ymd
        )
    } else if (identical(type, "monthly")) {
        # https://data.remss.com/amsr2/ocean/L3/v08.2/monthly/RSS_AMSR2_ocean_L3_monthly_2023-05_v08.2.nc
        # ^                                           ^                            ^    ^    ^
        # server                                      type                       type year month
        # use the month previous to the previous month
        url <- sprintf(
            "%s/%s/RSS_AMSR2_ocean_L3_%s_%04d-%02d_v08.2.nc",
            server, type, type, year, month
        )
    } else {
        # check again (but should not be able to get here)
        stop("type='", type, "' not permitted; try '3day', 'daily', 'weekly' or 'monthly'")
    }
    file <- gsub(".*/", "", url)
    dodDebug(debug, "url=\"", url, "\"\n", sep = "")
    dodDebug(debug, "file=\"", file, "\"\n", sep = "")
    rval <- dod.download(url, destdir = destdir, file = file, age = -1, debug = debug - 1, quiet = quiet)
    dodDebug(debug, "", sep = "")
    rval
}
