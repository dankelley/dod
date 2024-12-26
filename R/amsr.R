# vim:textwidth=200:expandtab:shiftwidth=4:softtabstop=4

#' Download Advanced Microwave Scanning Radiometer data
#'
#' This function downloads AMSR data.
#'
#' This works by constructing URLs based on the arguments provided.  The author
#' is unaware of any documentation that specifies the directory structure on
#' the server, and so the construction is based on examining the server
#' with a web browser.  Obviously, this is a fragile approach that will
#' lead to failed downloads if the remote directory structure changes.
#'
#'
#' @param date either a Date object or a character or time object that
#' can be converted to such an object with [as.Date()].  The default
#' is four days prior to the present date, which is usually the latest
#' view that can be obtained, if `type` is `"3day"`, as is its default.
#'
#' @param destdir character giving destination directory (defaults to `"."`,
#' the present directory).  The directory must exist. The author usually
#' sets this to `"~/data/amsr"`, so that the file will be in a central
#' location.
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
#' @section Historical notes:
#'
#' * Until September 2024 [dod.amsr()] required 3 parameters to specify
#' a time (`year`, `month` and `day`). This was difficult to
#' use and also led to messy coding, so these 3 parameters were replaced
#' with `date`.
#'
#' * Until July 2023, [dod.amsr()] worked by calling [oce::download.amsr()].
#' However, at that time, the author noticed changes in both the directory
#' structure of the remote server, and the format of the data files.
#' The new directory structure was addressed by a complete rewrite
#' of the code within `dod`, and a severing of the connection to the
#' `oce` function.
#'
#' @examples
#' if (interactive()) { # sidestep a pkgdown::build_site() error
#'     library("oce")
#'     # Get temporary space (to obey CRAN rules)
#'     destdir <- tempdir()
#'     file <- dod.amsr(destdir = destdir)
#'     natl <- read.amsr(file) |>
#'         subset(-90 < longitude & longitude < 0) |>
#'         subset(20 < latitude & latitude < 70)
#'     plot(natl)
#'     # Clean up space
#'     unlink(destdir, recursive = TRUE)
#' }
#'
#' @export
#' @family functions that download files
#' @author Dan Kelley
dod.amsr <- function(
    date = Sys.Date() - 4L,
    destdir = ".",
    server = "https://data.remss.com/amsr2/ocean/L3/v08.2",
    type = "3day",
    quiet = FALSE,
    debug = 0) {
    dodDebug(debug, "dod.amsr(..., type=\"", type, "\", ...) START\n", sep = "")
    if (!type %in% c("3day", "daily", "weekly", "monthly")) {
        stop("type='", type, "' not permitted; try '3day', 'daily', 'weekly' or 'monthly'")
    }
    if (is.character(date)) {
        date <- as.Date(date)
    }
    date <- as.POSIXlt(date)
    day <- date$mday
    month <- 1 + date$mon
    year <- 1900 + date$year
    dodDebug(debug, "    inferred year=", year, ", month=", month, ", day=", day, "\n", sep = "")
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
        dodDebug(debug, "    user-provided ymd=\"", ymd, "\"\n")
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
    rval <- dod.download(url = url, destdir = destdir, file = file, age = -1, quiet = quiet, debug = debug)
    dodDebug(debug, "    END dod.amsr()\n")
    rval
}
