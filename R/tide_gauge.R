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
#' @template debugTemplate
#'
#' @importFrom utils read.csv write.csv
## @importFrom rjson fromJSON
#'
#' @return [dod.tideGauge()] returns a list, if `value` is `"metadata"`
#' and `agency` is `"CHS"`; otherwise it returns a file name (with
#' full path included).  For the CHS case this is a constructed filename,
#' since the CHS server provides data, not files.  For the
#' NOAA case, it is a downloaded file.
#'
#' @examples
#' \dontrun{
#' library(dod)
#' library(oce)
#' ofile <- dod.tideGauge(491)
#' pfile <- dod.tideGauge(491, "predictions")
#' O <- read.csv(ofile)
#' O$time <- as.POSIXct(O$Date.Time, tz = "UTC")
#' P <- read.csv(pfile)
#' P$time <- as.POSIXct(P$Date.Time, tz = "UTC")
#' # Top panel: observation (black) and prediction (gray)
#' par(mfrow = c(2, 1))
#' oce.plot.ts(O$time, O$Water.Level, ylab = "Water Level [m]")
#' lines(P$time, P$Predictions, col = "gray", type = "l")
#' # Bottom panel: misfit. Note the interpolation to observation time.
#' misfit <- O$Water.Level - approx(P$time, P$Predictions, O$time)$y
#' oce.plot.ts(O$time, misfit, ylab = "Deviation [m]")
#' }
#'
#' @references
#'
#' 1. https://api-iwls.dfo-mpo.gc.ca/swagger-ui/index.html
#'
#' 2. https://api.tidesandcurrents.noaa.gov/api/prod/datagetter
#'
#' @export
dod.tideGauge <- function(
    ID = NULL, variable = "water_level", agency = "CHS",
    start = NULL, end = NULL, resolution = NULL,
    file = NULL, destdir = ".", age = 0, debug = 0) {
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
    dodDebug(debug, "after conversions etc, start: \"", start, "\"\n", sep = "")
    dodDebug(debug, "after conversions etc, end:   \"", end, "\"\n", sep = "")
    if (end < start) {
        stop("'start' time (", format(start), " is not prior to 'end' time (", format(end), ")")
    }
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
    } else if (identical(agency, "NOAA")) { # FIXME: is this right? (is anything still right?)
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
    startDigits <- gsub("-", "", start)
    endDigits <- gsub("-", "", end)
    dodDebug(debug, "destdir=\"", destdir, "\"\n", sep = "")
    if (agency == "CHS") {
        if (!requireNamespace("rjson", quietly = TRUE)) {
            stop("must install.packages(\"rjson\") before using dod.tideGauge() with agency=\"CHS\"")
        }
        dodDebug(debug, "about to try to find code for ID=\"", ID, "\"\n", sep = "")
        # Find station ID.  This is a string like 5cebf1e23d0f4a073c4bbfac,
        # which is a value matching officialName "Bedford Institute",
        # or code "00491".  (Whether the ID will change over time for
        # a given station is unknown to the author of this function,
        # so no attempt is made to save known names.)
        url <- "https://api-iwls.dfo-mpo.gc.ca/api/v1/stations"
        s <- readLines(url, warn = FALSE)
        d <- rjson::fromJSON(s)
        if (grepl("a-zA-Z", ID)) {
            dodDebug(debug, "looking up station by name", ID, "\n")
            # e.g. "Bedford Institute"
            w <- which(sapply(d, \(s) grepl(ID, s$officialName)))
            if (length(w) == 0L) {
                stop("cannot find API code for ID=\"", ID, "\" (interpreted as a name)")
            }
        } else {
            dodDebug(debug, "looking up station by ID number", ID, "\n")
            # e.g. "00491"
            ID <- as.integer(ID)
            w <- which(sapply(d, \(s) identical(ID, as.integer(s$code))))
            if (length(w) == 0L) {
                stop("cannot find API code for ID=\"", ID, "\" (interpreted as a number)")
            }
        }
        ds <- d[[w]]
        stationID <- ds$id
        dodDebug(debug, "calculated CHS station ID code to be \"", stationID, "\"\n", sep = "")
        # return metadata, if requested
        url <- "https://api-iwls.dfo-mpo.gc.ca/api/v1/stations"
        if (identical(variable, "metadata")) {
            url <- sprintf("https://api-iwls.dfo-mpo.gc.ca/api/v1/stations/%s/metadata", stationID)
            s <- readLines(url, warn = FALSE)
            dodDebug(debug, "returning metadata, not a file name\n")
            return(rjson::fromJSON(s))
        }
        # OK, now we know the user wants data
        variableRemote <- variable
        variableRemote <- gsub("^water_level$", "wlo", variableRemote)
        variableRemote <- gsub("^predictions$", "wlp", variableRemote)
        url <- sprintf(
            "api-iwls.dfo-mpo.gc.ca/api/v1/stations/%s/data?time-series-code=%s&from=%s&to=%s&resolution=%s",
            stationID,
            variableRemote,
            format(start, "%Y-%m-%dT%H:%M:00Z"),
            format(end, "%Y-%m-%dT%H:%M:00Z"),
            resolution
        )
        url <- paste0("https://", gsub(":", "%3A", url))
        dodDebug(debug, "url: \"", url, "\"\n", sep = "")
        s <- readLines(url, warn = FALSE)
        d <- rjson::fromJSON(s)
        time <- sapply(d, \(x) x$eventDate)
        time <- gsub("Z$", "", gsub("T", " ", time)) # for output
        QC <- sapply(d, \(x) x$qcFlagCode)
        var <- sapply(d, \(x) x$value)
        if (variable == "water_level") {
            res <- data.frame("Date.Time" = time, "Water.Level" = var, "QC" = QC)
        } else if (variable == "predictions") {
            res <- data.frame("Date.Time" = time, "Predictions" = var, "QC" = QC)
        }
        dodDebug(debug, "about to save data in \"", file, "\"\n", sep = "")
        write.csv(res, file = file, row.names = FALSE)
        return(file)
    } else if (agency == "NOAA") {
        # https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?product=predictions&application=NOS.COOPS.TAC.WL&begin_date=20230801&end_date=20230830&datum=MLLW&station=8727520&time_zone=GMT&units=metric&interval=&format=CSV
        server <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
        # Rename variable
        variableRemote <- variable
        url <- sprintf(
            "%s?product=%s&application=NOS.COOPS.TAC.WL&begin_date=%s&end_date=%s&datum=MLLW&station=%s&time_zone=GMT&units=metric&interval=&format=CSV",
            server, variableRemote, start, end, ID
        )
    } else {
        stop("unknown agency \"", agency, "\"; try either \"CHS\" or \"NOAA\"")
    }
    dodDebug(debug, "url: ", url, "\n", sep = "")
    filename <- try(
        dod.download(url = url, destdir = destdir, file = file, age = age, debug = debug - 1L),
        silent = TRUE
    )
    if (inherits(filename, "try-error")) {
        stop("cannot download \"", file, "\" from \"", url, "\"")
    }
    return(filename)
}
