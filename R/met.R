#' Download meteorological timeseries data
#'
#' [dod.met()] attempts to download data from Environment Canada's
#' historical-data website, and to cache the files locally. Lacking a published
#' API, this function must rely on reverse-engineering of queries handled by
#' that web server.  For that reason, it is brittle.
#'
#' If this function fails, users might try using Gavin Simpson's `canadaHCD`
#' package (reference 2) or the weathercan package (reference 3).
#'
#' @param id a number giving the "Station ID" of the station of interest. If not
#' provided, `id` defaults to 43405, for Halifax Dockyard. (Previously,
#' the default was 6358, for Halifax International Airport, but in March
#' 2024 it was noticed that 6358 held no data.)
#' See \dQuote{Details}.
#'
#' @param year a number giving the year of interest. Ignored unless `deltat`
#' is `"hour"`. If `year` is not given, it defaults to the present year.
#'
#' @param month a number giving the month of interest. Ignored unless `deltat`
#' is `"hour"`. If `month` is not given, it defaults to the present
#' month.  As a special case, if neither `year` nor `month` is given,
#' and it is the first day of the month, [dod.met()] goes back one month,
#' to avoid returning a file with no data.
#'
#' @param deltat an optional character string indicating the time step of the
#' desired dataset. This may be `"hour"` or `"month"`.
#' If `deltat` is not given, it defaults to `"hour"`.
#'
#' @param type a character value indicating which type of file to download, either
#' `"xml"` (the default) for an XML file or `"csv"` for a CSV file.
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template debugTemplate
#'
#' @return [dod.met()] returns a character value holding the full
#' pathname of the downloaded file.
#'
#' @examples
#' # Meteorological data for Halifax, Nova Scotia.
#' if (interactive()) { # pkgdown::build_site() cannot handle downloads
#'     library(dod)
#'     destdir <- tempdir(check = TRUE)
#'     metFile <- dod.met(43405, destdir = destdir)
#'     if (requireNamespace("oce", quietly = TRUE) &&
#'         requireNamespace("XML", quietly = TRUE)) {
#'         library(oce)
#'         met <- read.met(metFile)
#'         t <- met[["time"]]
#'         p <- met[["pressure"]]
#'         oce.plot.ts(t, p, ylab = "Atm. Pressure [Pa]")
#'     }
#'     unlink(destdir, recursive = TRUE)
#' }
#'
#' @references
#' 1. Environment Canada website for Historical Climate Data
#' `https://climate.weather.gc.ca/index_e.html`
#'
#' 2. Gavin Simpson's `canadaHCD` package on GitHub
#' `https://github.com/gavinsimpson/canadaHCD`
#'
#' 3. Weathercan package `https://github.com/ropensci/weathercan/tree/main`
#'
#' @importFrom utils capture.output
#'
#' @export
#'
#' @family functions that download files
#'
#' @author Dan Kelley
dod.met <- function(id, year, month, deltat, type = "xml", destdir = ".", age = 1, debug = 0) {
    dodDebug(debug, "dod.met() START\n")
    if (missing(id)) {
        id <- 43405 # was 6358 until 2024-03-16
        dodDebug(debug, "    defaulting id to ", id, "\n")
    }
    id <- as.integer(id)
    if (missing(deltat)) {
        deltat <- "hour"
        dodDebug(debug, "    defaulting deltat to ", deltat, "\n")
    }
    yearGiven <- !missing(year)
    monthGiven <- !missing(month)
    deltatChoices <- c("hour", "month") # FIXME: add "day"
    deltatIndex <- pmatch(deltat, deltatChoices)
    if (!(type %in% c("csv", "xml"))) {
        stop("type '", type, "' not permitted; try 'csv' or 'xml'")
    }
    if (is.na(deltatIndex)) {
        stop("deltat=\"", deltat, "\" is not supported; try \"hour\" or \"month\"")
    }
    today <- as.POSIXlt(Sys.time(), tz = "UTC")
    thisMonth <- today$mon + 1 # it has Jan=0
    thisDay <- today$mday # day of month
    thisYear <- today$year + 1900
    if (!yearGiven) {
        year <- thisYear
    }
    if (!monthGiven) {
        month <- thisMonth
    }
    monthOrig <- month
    month <- monthNumberFromName(monthOrig)
    if (is.na(month)) {
        stop("unknown month \"", monthOrig, "\"")
    }
    # go back 1 month if defaulting to current month on day 1,
    # when the data will be missing.
    if (thisDay == 1 && !yearGiven && !monthGiven) {
        month <- month - 1
        # Handle month wrapping
        if (month < 0) {
            year <- year - 1
            month <- 12
        }
    }
    dodDebug(debug, "    using year=", year, " month=", month, "\n", sep = "")
    deltat <- deltatChoices[deltatIndex]
    if (deltat == "hour") {
        #<2024-04-01> today <- as.POSIXlt(Sys.time(), tz = "UTC")
        #<2024-04-01> mday <- today$mday
        #<2024-04-01> if (mday == 1) {
        #<2024-04-01>     month <- today$mon - 1
        #<2024-04-01> }
        #<2024-04-01> if (missing(year)) {
        #<2024-04-01>     year <- today$year + 1900
        #<2024-04-01> }
        #<2024-04-01> if (missing(month)) {
        #<2024-04-01>     month <- today$mon + 1 # so 1=jan etc
        #<2024-04-01>     # ERROR month <- month - 1 # we want *previous* month, which should have data
        #<2024-04-01>     if (month == 1) {
        #<2024-04-01>         year <- year - 1
        #<2024-04-01>         month <- 12
        #<2024-04-01>     }
        #<2024-04-01>     cat("next is today\n")
        #<2024-04-01>     dodDebug(debug, "  today$mday=", today$mday, "\n", sep = "")
        #<2024-04-01>     dodDebug(debug, "  defaulting to month=", month, " and year=", year, "\n", sep = "")
        #<2024-04-01> }
        # Next line is an example that worked on 2017-02-02
        # Still works on 2024-04-01
        # http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=6358&Year=2003&Month=9&timeframe=1&submit=Download+Data
        url <- paste("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?",
            "format=", type,
            "&stationID=", id,
            "&Year=", year,
            "&Month=", month,
            "&timeframe=1&submit=Download+Data",
            sep = ""
        )
        file <- sprintf(
            "met_%d_hourly_%04d_%02d_%02d.%s",
            id, year, month, 1, type
        )
    } else if (deltat == "month") {
        # Next line reverse engineered from monthly data at Resolute. I don't imagine we
        # need Year and Month and Day.
        url <- paste("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?stationID=",
            id, "&format=", type, "&timeframe=3&submit=Download+Data",
            sep = ""
        )
        # id, "&Year=2000&Month=1&Day=14&format=csv&timeframe=3&submit=%20Download+Data", sep="")
        file <- sprintf("met_%d_monthly.%s", id, type)
    } else {
        stop("deltat must be \"hour\" or \"month\"")
    }
    rval <- dod.download(
        url = url, file = file, age = age,
        destdir = destdir, debug = debug - 1
    )
    dodDebug(debug, "dod.met() END\n")
    rval
} # dod.met

#' Download sounding data
#'
#' Download an atmospheric sounding file from the University of Wyoming
#' Department of Atmospheric science website at
#' <https://weather.uwyo.edu/upperair/sounding.html>. This server has
#' changed over time, and the present code for `dod.met.sounding()` reflects
#' its file and URL-query structure as inferred on 2025-08-21.
#'
#' @param id character value indicating the station identifier.  The
#' default is for a station at Yarmouth, Nova Scotia.
#'
#' @param time the time for which data are sought, in `POSIXct`, `POSIXlt`
#' or `Date` format.  The default is the previous day.
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template debugTemplate
#'
#' @return `dod.met.sounding()` returns the name of the downloadeded file.
#'
#' @examples
#' if (interactive()) { # pkgdown::build_site() cannot handle downloads
#'     # Get yesterday's data at Yarmouth, Nova Scotia.
#'     destdir <- tempdir(check = TRUE) # removed at end to pass CRAN checks
#'     file <- dod.met.sounding(destdir = destdir)
#'     data <- read.csv(file)
#'     names(data) # discover names of data items
#'     plot(data$temperature_C, data$pressure_hPa,
#'         type = "l",
#'         xlab = "Temperature [C]", ylab = "Pressure [hPa]", ylim = rev(range(data$pressure_hPa))
#'     )
#'     unlink(destdir, recursive = TRUE)
#' }
#'
#' @export
#' @author Dan Kelley
dod.met.sounding <- function(id = "71603", time = Sys.Date() - 1, destdir = ".", age = 0, debug = 0) {
    dodDebug(debug, "dod.met.sounding() START\n")
    if (!inherits(time, "Date") && !inherits(time, "POSIXt")) {
        stop("'time' must be of class Date, POSIXlt, or POSIXct")
    }
    time <- as.POSIXlt(time)
    datetime <- format(time, "%Y-%m-%d")
    dodDebug("datetime: \"", datetime, "\"\n", sep = "")
    # As of 2025-08:
    urlExample <- "https://weather.uwyo.edu/wsgi/sounding?datetime=2025-08-21%200:00:00&id=72518&src=BUFR&type=TEXT:CSV"
    url <- sprintf(
        "https://weather.uwyo.edu/wsgi/sounding?datetime=%s%%200:00:00&id=%s&src=BUFR&type=TEXT:CSV",
        datetime, id
    )
    dodDebug(debug, "url: \"", url, "\"\n", sep = "")
    dodDebug(debug, "urlExample: \"", sample, "\"\n", sep = "")
    file <- paste0("sounding", "_", id, "_", datetime, ".csv")
    rval <- dod.download(url, destdir = destdir, file = file, age = age, debug = debug)
    dodDebug(debug, "dod.met.sounding() END\n")
    rval
} # dod.met.sounding

#' Get Index of Canadian Meteorological Stations
#'
#' Look up information on a Canadian meteorological station, by
#' a fuzzy search on information stored on a gc.ca website. The main
#' use is to find a station ID, to be used with [dod.met()].
#'
#' The source file is at the web location named by the `url`
#' parameter. (Please contact the author, if the default value fails.
#' It was known to work in May of 2024, but this agency might change
#' the file location or format at any time.) The results are a data
#' frame with information that ought to be easy to interpret.  For use
#' with [dod.met()], the ID field that is of use is named
#' `"Climate.ID"`.  See \dQuote{Examples} for what might be of
#' interest for hourly data.
#'
#' @param name character value use in a name search. The search is
#' done using [agrep()] with the supplied `max.distance` value
#' and with `ignore.case` set to TRUE.
#'
#' @param max.distance numerical value passed to [agrep()] in
#' the station-name search.
#'
#' @param url the URL of the source file.  The default value has
#' changed over time, owing to changes with the Government of Canada
#' website. For example, from 2024-05-06, the link at Reference 1 worked. However,
#' the author noticed on 2025-07-23 that this URL did not provide
#' files that held a "Climate ID" column, and so the default was changed
#' to Reference 2 on that date. Users are asked to post an issue
#' on the `github.com/dankelley/dod/issues` website, if they find
#' that [dod.met.index()] either reports errors, or if the data
#' frame returned by this function lacks a column labelled `Climate ID`,
#' since values in that column are what is needed as the `id` parameter
#' by [dod.met()].
#'
#' @template quietTemplate
#'
#' @template debugTemplate
#'
#' @examples
#' if (interactive()) { # pkgdown::build_site() cannot handle downloads
#'     # Get index of meteorological data for Halifax, N.S.
#'     library(dod)
#'     i <- dod.met.index("halifax")
#'     names(i) # see what's in these files
#'     i[, c("Province", "Name", "Climate.ID")]
#'     # focus on the ones in Nova Scotia
#'     i <- i[grep("nova scotia", i$Province, ignore.case = TRUE), ]
#'     # Show start/end times for hourly data.
#'     i[, c("Name", "Station.ID", "HLY.First.Year", "HLY.Last.Year")]
#'     #>                           Name Station.ID HLY.First.Year HLY.Last.Year
#'     #> 8196                   HALIFAX       6355             NA            NA
#'     #> 8197                   HALIFAX       6356           1953          1963
#'     #> 8198           HALIFAX CITADEL       6357             NA            NA
#'     #> 8199           HALIFAX COMMONS      49128           2010          2011
#'     #> 8200          HALIFAX DOCKYARD      43405           2004          2025
#'     #> 8201 HALIFAX STANFIELD INT\'L A      53938           2019          2025
#'     #> 8202 HALIFAX STANFIELD INT\'L A       6358           1961          2012
#'     #> 8203 HALIFAX STANFIELD INT\'L A      50620           2012          2025
#'     #> 8204          HALIFAX KOOTENAY      43124           2004          2025
#'     #> 8205      HALIFAX WINDSOR PARK      43403           2004          2025
#' }
#'
#' @export
#'
#' @references
#'
#' 1. Default `url` from 2024-05-06 to 2025-07-23:
#' `https://dd.weather.gc.ca/climate/observations/climate_station_list.csv`.
#' 2. Default `url` from 2025-07-23 onward:
#' paste0("https://collaboration.cmc.ec.gc.ca/cmc/climate/",
#'     "Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv).
#'
#' @author Dan Kelley
dod.met.index <- function(name,
                          max.distance = 0.1,
                          url = paste0(
                              "https://collaboration.cmc.ec.gc.ca/cmc/climate/",
                              "Get_More_Data_Plus_de_donnees/Station%20Inventory%20EN.csv"
                          ),
                          quiet = FALSE,
                          debug = 0) {
    dodDebug(debug, "dod.met.index() START\n")
    dodDebug(debug, "    max.distance=", max.distance, "\n", sep = "")
    dodDebug(debug, "    url=\"", url, "\"\n", sep = "")
    destfile <- tempfile("met", fileext = ".csv")
    dodDebug(debug, "    destfile=\"", destfile, "\"\n", sep = "")
    t <- try(
        # download.file(url = url, destfile = destfile, quiet = quiet, mode = "wb")
        curl::curl_download(url = url, destfile = destfile, quiet = quiet, mode = "wb"),
        silent = quiet
    )
    if (inherits(t, "try-error")) {
        stop("could not download ", url)
    }
    dodDebug(debug, "    about to read downloaded file\n")
    d <- read.csv(destfile, header = TRUE, skip = 3)
    # "Name","Province","Climate ID","Station ID","WMO ID","TC ID","Latitude (Decimal Degrees)","Longitude (Decimal Degrees)","Latitude","Longitude","Elevation (m)","First Year","Last Year","HLY First Year","HLY Last Year","DLY First Year","DLY Last Year","MLY First Year","MLY Last Year"
    dodDebug(debug, "    read ", nrow(d), " lines\n")
    # until 2025-07-23, the station name was in d$Station.name
    w <- agrep(name, d$Name, max.distance = max.distance, ignore.case = TRUE)
    dodDebug(debug, "    found ", length(w), " matches for \"", name, "\" with max.distance=", max.distance, "\n")
    d <- d[w, ]
    if (FALSE) {
        dodDebug(debug, "    removing temporary file '", destfile, "'\n")
        file.remove(destfile)
    }
    message(destfile)
    dodDebug(debug, "END dod.met.index()\n")
    d
}
