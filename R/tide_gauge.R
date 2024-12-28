#' Download tide-gauge data
#'
#' [dod.tideGauge()] downloads tide-gauge data from either
#' Canadian (CHS) or American (NOAA) tide gauges.
#'
#' Downloads are done from for either the Canadian Hydrographic Service (CHS) or
#' from the American National Oceanographic and Atmospheric
#' Agency (NOAA), respectively (see References 1 and 2). The resultant data are saved in
#' either constructed filenames, or filenames provided by the user;
#' in either case, if a recent file already exists with the indicated name,
#' then no data are downloaded.
#'
#' For NOAA files, water level and predictions are provided on the same
#' time sequence, but for CHS files, this is not the case, e.g. predictions
#' (in the author's tests) are on 15 minute intervals, starting at an
#' hour marker, but observations may be at a variety of times, depending
#' `start` and `end`.  Therefore, numerical comparision with CHS data
#' will require interpolation (see \sQuote{Examples}).
#'
#' @param ID a character (or possibly integer) value specifying the
#' numeric identifier of the tide gauge. For Canadian data, this
#' is either the station number or the station name (e.g. number 491
#' corresponds to the "Bedford Institute" station).  For American
#' data, it is a numerical code.
#'
#' @param agency a character value indicating the agency from which data will be
#' sought. Use `"CHS" for Canadian tide gauges (the default), or `"NOAA"` for
#' American tide gauges.
#'
#' @param start,end POSIXt times or character values that can be converted
#' to such times (via [as.POSIXct()] with `tz="UTC"`) that indicate
#' the time interval for the requested data.  If `end` is not specified,
#' then the present time is used.  If `start` is not provided, then
#' it is set to the present time minus 7 days.  If `start` is a
#' numeric value, then it is interpreted as the number of days
#' to go back in time from the `end` time.
#'
#' @param resolution character value indicating the resolution, only used
#' if `agency` is `"CHS"`.  The choices are `"ONE_MINUTE",
#' `"THREE_MINUTES"` (the default),
#' `"FIVE_MINUTES"`,
#' `"FIFTEEN_MINUTES"`, and
#' `"SIXTY_MINUTES"`.  The server website indicates that
#' `"Not all resolution are available for every station and every time series."`
#' It also indicates that
#' "The time frame available increases with the resolution, it is equal
#' to a week (7 days) multiplied by the resolution, but the maximum
#' available is 31 days. (a month)".  This second statement perhaps
#' means that it is possible to download at most 1 week of data for
#' 1-minute resolution.
#'
#' @param variable a character value indicating the name of the variable to
#' be downloaded.  This defaults to `"water_level"` for observed water
#' level (called `"wlo"` on the CHS server and `"water_level"` on the NOAA server).
#' Another permitted choice is `"predictions" (called `"wlp"` by CHS and `"predictions"`
#' by NOAA). In either of these two cases, `time` and `QC` are also stored alongside
#' the variable.  But there is a third case: if `variable` is `"metadata"`,
#' then *no* file is saved; instead, the return value is a list containing
#' information about the station, such as its code number, its official
#' name, its datum, etc.
#'
#' @param file a character value indicating the name to be used for the
#' downloaded data.  If not provided, this is constructed as e.g.
#' `"tide_A_N_S_E_R_V.csv"` where `A` is the value of the agency argument, `N` is
#' the station ID number, `S` and `E` are the start and end dates written in
#' 8-digit format (i.e. sans the `"-"` characters), `R` is the resolution in
#' minutes, and `V` is the variable name.
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template quietTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv write.csv
## @importFrom jsonlite fromJSON
#'
#' @return [dod.tideGauge()] returns a list, if `value` is `"metadata"`
#' and `agency` is `"CHS"`; otherwise it returns a file name (with
#' full path included).  For the CHS case this is a constructed filename,
#' since the CHS server provides data, not files.  For the
#' NOAA case, it is a downloaded file.
#'
#' @examples
#' # Download and plot tide-guage data for Halifax Harbour
#' if (interactive()) { # sidestep a pkgdown::build_site() error
#'     # NOTE: data file is removed at end, to pass CRAN checks
#'     library(dod)
#'     library(oce)
#'     destdir <- tempdir()
#'     ofile <- dod.tideGauge(491, destdir = destdir)
#'     pfile <- dod.tideGauge(491, "predictions", destdir = destdir)
#'     O <- read.csv(ofile)
#'     O$time <- as.POSIXct(O$Date.Time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
#'     P <- read.csv(pfile)
#'     P$time <- as.POSIXct(P$Date.Time, "%Y-%m-%d %H:%M:%S", tz = "UTC")
#'     # Top panel: observation (black) and prediction (gray)
#'     par(mfrow = c(2, 1))
#'     oce.plot.ts(O$time, O$Water.Level, ylab = "Water Level [m]", xaxs = "i")
#'     lines(P$time, P$Predictions, col = "gray", type = "l")
#'     # Bottom panel: misfit. Note the interpolation to observation time.
#'     misfit <- O$Water.Level - approx(P$time, P$Predictions, O$time)$y
#'     oce.plot.ts(O$time, misfit, ylab = "Deviation [m]", xaxs = "i")
#'     unlink(destdir, recursive = TRUE)
#' }
#'
#' @references
#'
## 1. https://api-iwls.dfo-mpo.gc.ca/swagger-ui/index.html
#' 1. https://api.iwls-sine.azure.cloud-nuage.dfo-mpo.gc.ca/swagger-ui/index.html
#'
#' 2. https://api.tidesandcurrents.noaa.gov/api/prod/datagetter
#'
#' @family functions that download files
#' @export
#' @author Dan Kelley
dod.tideGauge <- function(
    ID = NULL, variable = "water_level", agency = "CHS",
    start = NULL, end = NULL, resolution = NULL,
    file = NULL, destdir = ".", age = 0, quiet = FALSE, debug = 0) {
    dodDebug(debug, "dod.tideGauge() START\n", unindent = 1)
    rval <- ""
    if (!dir.exists(destdir)) {
        stop("destdir \"", destdir, "\" does not exist")
    }
    if (is.null(ID)) {
        stop("ID must be supplied")
    }
    if (!is.character(ID)) {
        ID <- as.character(ID)
    }
    if (!is.character(agency)) {
        stop("'agency' must be a character value")
    }
    if (!agency %in% c("CHS", "NOAA")) {
        stop("'agency' must be either \"CHS\" or \"NOAA\"")
    }
    if (length(variable) != 1L) {
        stop("'variable' must be of length 1")
    }
    if (!variable %in% c("water_level", "predictions", "metadata")) {
        stop("variable must be \"water_level\", \"predictions\" or \"metadata\", but it is \"", variable, "\"")
    }
    if (is.null(end)) {
        end <- as.POSIXct(Sys.time(), tz = "UTC")
    }
    if (is.null(start)) {
        start <- end - 7 * 86400
    } else if (is.numeric(start)) {
        start <- end - start * 86400
    }
    start <- as.POSIXct(start, tz = "UTC")
    end <- as.POSIXct(end, tz = "UTC")
    if (end < start) {
        stop("'start' time (", format(start), " is not prior to 'end' time (", format(end), ")")
    }
    dodDebug(debug, "start: \"", format(start), "\"\n", sep = "")
    dodDebug(debug, "end: \"", format(end), "\"\n", sep = "")
    if (identical(agency, "CHS")) {
        if (is.null(resolution)) {
            resolution <- "THREE_MINUTES"
        }
        if (!is.character(resolution)) {
            stop("'resolution' must be a character value")
        }
        resolutionNumeric <- switch(resolution,
            "ONE_MINUTE" = 1,
            "THREE_MINUTES" = 3,
            "FIVE_MINUTES" = 5,
            "FIFTEEN_MINUTES" = 15,
            "SIXTY_MINUTES" = 60
        )
    } else if (identical(agency, "NOAA")) { # Is this right?
        if (is.null(resolution)) {
            resolution <- 60
            resolutionNumeric <- 60
        }
    }
    dodDebug(debug, "resolution=", resolution, ", resolutionNumeric=", resolutionNumeric, "\n")
    if (is.null(file)) {
        file <- paste0(paste("tide_gauge", agency, ID,
            format(start, "%Y%m%dT%H%M"),
            format(end, "%Y%m%dT%H%M"),
            paste0(resolutionNumeric, "min"),
            variable,
            sep = "_"
        ), ".csv")
    }
    dodDebug(debug, "file=\"", file, "\"\n", sep = "")
    # startDigits <- gsub("-", "", start)
    # endDigits <- gsub("-", "", end)
    dodDebug(debug, "destdir=\"", destdir, "\"\n", sep = "")
    if (agency == "CHS") {
        fullfilename <- paste0(destdir, "/", file)
        if (file.exists(fullfilename)) {
            ctime <- file.info(fullfilename)$ctime
            now <- Sys.time()
            fileAge <- (as.numeric(now) - as.numeric(ctime)) / 86400
            if (fileAge < age) {
                dodDebug(debug, "the existing file is recent enough to skip downloading\n")
                dodDebug(debug, "dod.tideGauge() END\n", unindent = 1)
                return(fullfilename)
            }
        }
        if (!requireNamespace("jsonlite", quietly = TRUE)) {
            stop("must install.packages(\"jsonlite\") before using dod.tideGauge() with agency=\"CHS\"")
        }
        # Find station ID.  This is a string like 5cebf1e23d0f4a073c4bbfac,
        # which is a value matching officialName "Bedford Institute",
        # or code "00491".  (Whether the ID will change over time for
        # a given station is unknown to the author of this function,
        # so no attempt is made to save known names.)
        url <- "https://api-iwls.dfo-mpo.gc.ca/api/v1/stations"
        s <- readLines(url, warn = FALSE)
        d <- jsonlite::fromJSON(s)
        if (grepl("a-zA-Z", ID)) {
            dodDebug(debug, "looking up station by name ", ID, "\n")
            # e.g. "Bedford Institute"
            w <- which(sapply(d, \(s) grepl(ID, s$officialName)))
            if (length(w) == 0L) {
                stop("cannot find API code for ID=\"", ID, "\" (interpreted as a name)")
            }
        } else {
            dodDebug(debug, "looking up station by ID number ", ID, "\n")
            # e.g. "00491"
            w <- which(as.integer(ID) == as.integer(d$code))
            if (length(w) == 0L) {
                stop("cannot find API code for ID=\"", ID, "\" (interpreted as a number)")
            }
            if (length(w) > 1L) {
                stop("multiple matches for ID=\"", ID, "\" at indices ", paste(w, collapse = " "))
            }
            dodDebug(debug, "got this ID, at entry ", w, " of the data\n")
        }
        # ds <- d$timeSeries[[w]]
        stationID <- d$id[w]
        dodDebug(debug, "calculated CHS station ID code to be \"", stationID, "\"\n", sep = "")
        # return metadata, if requested
        url <- "https://api-iwls.dfo-mpo.gc.ca/api/v1/stations"
        dodDebug(debug, "at the start, url = \"", url, "\"\n", sep = "")
        dodDebug(debug, "at the start, variable = \"", variable, "\"\n", sep = "")
        if (identical(variable, "metadata")) {
            url <- sprintf("https://api-iwls.dfo-mpo.gc.ca/api/v1/stations/%s/metadata", stationID)
            dodDebug(debug, "for metadata, using url = \"", url, "\"\n", sep = "")
            s <- readLines(url, warn = FALSE)
            dodDebug(debug, "returning metadata, not a file name\n")
            if (!requireNamespace("jsonlite", quietly = TRUE)) {
                stop("must install.packages(\"jsonlite\") before using dod.tideGauge() with agency=\"CHS\"")
            }
            return(jsonlite::fromJSON(s))
        }
        # OK, now we know the user wants data
        variableRemote <- variable
        variableRemote <- gsub("^water_level$", "wlo", variableRemote)
        variableRemote <- gsub("^predictions$", "wlp", variableRemote)
        dodDebug(debug, "after name translation, variableRemote = \"", variableRemote, "\"\n", sep = "")
        url <- sprintf(
            "api-iwls.dfo-mpo.gc.ca/api/v1/stations/%s/data?time-series-code=%s&from=%s&to=%s&resolution=%s",
            stationID,
            variableRemote,
            format(start, "%Y-%m-%dT%H:%M:00Z"),
            format(end, "%Y-%m-%dT%H:%M:00Z"),
            resolution
        )
        dodDebug(debug, "url for data (step 1): \"", url, "\"\n", sep = "")
        url <- paste0("https://", gsub(":", "%3A", url))
        dodDebug(debug, "url for data (step 2): \"", url, "\"\n", sep = "")
        # https://api.iwls-sine.azure.cloud-nuage.dfo-mpo.gc.ca/api/v1/stations/5cebf1e23d0f4a073c4bbfac/data\?time-series-code\=wlo\&from\=2024-12-10T01%3A01%3A01Z\&to\=2024-12-15T01%3A01%3A01Z\&resolution\=THREE_MINUTES # nolint: line_length_linter.
        s <- readLines(url, warn = FALSE)
        d <- jsonlite::fromJSON(s)
        time <- as.POSIXct(d$eventDate, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        QC <- as.integer(d$qcFlagCode)
        var <- d$value
        if (variable == "water_level") {
            res <- data.frame("Date.Time" = time, "Water.Level" = var, "QC" = QC)
        } else if (variable == "predictions") {
            res <- data.frame("Date.Time" = time, "Predictions" = var, "QC" = QC)
        }
        write.csv(res, file = fullfilename, row.names = FALSE)
        dodDebug(debug, "saving \"", fullfilename, "\"\n", sep = "")
        dodDebug(debug, "dod.tideGauge() END\n", unindent = 1)
        return(file)
    } else if (agency == "NOAA") {
        # https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?product=predictions&application=NOS.COOPS.TAC.WL&begin_date=20230801&end_date=20230830&datum=MLLW&station=8727520&time_zone=GMT&units=metric&interval=&format=CSV # nolint: line_length_linter.
        server <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
        # Rename variable
        variableRemote <- variable
        url <- sprintf(
            "%s?product=%s&application=NOS.COOPS.TAC.WL&begin_date=%s&end_date=%s&datum=MLLW&station=%s&time_zone=GMT&units=metric&interval=&format=CSV",
            server, variableRemote, start, end, ID
        )
    } else {
        # we already checked the agency, but retain this for code clarity
        stop("unknown agency \"", agency, "\"; try either \"CHS\" or \"NOAA\"")
    }
    dodDebug(debug, "about to download \"", url, "\"\n")
    fullfilename <- paste0(destdir, "/", file)
    if (file.exists(fullfilename)) {
        ctime <- file.info(fullfilename)$ctime
        now <- Sys.time()
        fileAge <- (as.numeric(now) - as.numeric(ctime)) / 86400
        if (fileAge < age) {
            dodDebug(debug, "the existing file is recent enough to skip downloading\n")
            dodDebug(debug, "dod.tideGauge() END\n", unindent = 1)
            return(fullfilename)
        }
    }
    rval <- try(
        dod.download(url = url, destdir = destdir, file = file, age = age, quiet = quiet, debug = debug - 1L),
        silent = TRUE
    )
    if (inherits(rval, "try-error")) {
        stop("cannot download \"", file, "\" from \"", url, "\"")
    }
    dodDebug(debug, "downloaded \"", fullfilename, "\"\n")
    dodDebug(debug, "dod.tideGauge() END\n", unindent = 1)
    return(rval)
}
