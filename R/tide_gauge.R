#' Download tide-gauge data
#'
#' [dod.tideGauge()] downloads tide-gauge data from either
#' Canadian or American tide gauges.
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
#' the time interval for the requested data.
#' If either `start` or `end` is not supplied, then a default
#' of the past week is used.
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
#' be downloaded.  This defaults to `"height"` for observed height (called `"wlo"`
#' on the CHS server and `"water_level"` on the NOAA server).  The other
#' choice is `"heightPredicted" (called `"wlp"` by CHS and `"predictions"`
#' by NOAA). In either case, `time` and `QC` are also stored alongside
#' the variable.
#'
#' @param file a character value indicating the name to be used for the
#' downloaded data.  If not provided, this is constructed as e.g.
#' `"tide_A_N_S_E_V.csv"` where `A` is the value of the agency argument, `N` is
#' the station ID number, `S` and `E` are the start and end dates written in
#' 8-digit format (i.e. sans the `"-"` characters), and `V` is the variable
#' name.
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv write.csv
#' @importFrom rjson fromJSON
#'
#' @return [dod.tideGauge()] returns the full pathname of the
#' constructed file (in the CHS case) or the downloaded
#' file (in the NOAA case). This is a comma-separated file, with
#' first column named `"Date Time"`, and second column named either
#' `"Water Level"` or `"Predictions"`, according to the specified
#' value of `variable`. (The names in the output file derive
#' from the names in files returned by the NOAA server.)
#'
#' @examples
#'\dontrun{
#' library(dod)
#' library(oce)
#' ofile <- dod.tideGauge(491)
#' pfile <- dod.tideGauge(491, "heightPredicted")
#' O <- read.csv(ofile)
#' O$time <- as.POSIXct(O$Date.Time, tz="UTC")
#' P <- read.csv(pfile)
#' P$time <- as.POSIXct(P$Date.Time, tz="UTC")
#' # Top panel: observation (black) and prediction (gray)
#' par(mfrow=c(2, 1))
#' oce.plot.ts(O$time, O$Water.Level, ylab="Water Level [m]")
#' lines(P$time, P$Predictions, col="gray", type="l")
#' # Bottom panel: misfit
#' Pinterp <- approxfun(P$time, P$Predictions)
#' oce.plot.ts(O$time, O$Water.Level-Pinterp(O$time), ylab="Deviation [m]")
#'}
#'
#' @references
#'
#' 1. https://api-iwls.dfo-mpo.gc.ca/swagger-ui/index.html
#'
#' 2. https://api.tidesandcurrents.noaa.gov/api/prod/datagetter
#'
#' @export
dod.tideGauge <- function(ID=NULL, variable="height", agency="CHS",
    start=NULL, end=NULL, resolution=NULL, file=NULL, destdir=".", age=1, debug=0)
{
    if (is.null(ID))
        stop("ID must be supplied")
    if (!is.character(ID))
        ID <- as.character(ID)
    if (!is.character(agency))
        agency <- as.character(agency)
    if (length(variable) != 1L)
        stop("'variable' must be of length 1")
    if (!variable %in% c("height", "heightPredicted"))
        stop("variable must be \"height\" or \"heightPredicted\", but it is \"", variable, "\"")
    if (is.null(start) || is.null(end)) {
        end <- as.POSIXct(Sys.time(), tz="UTC")
        start <- end - 7*86400
    }
    start <- as.POSIXct(start, tz="UTC")
    end <- as.POSIXct(end, tz="UTC")
    if (end < start)
        stop("'start' time (", format(start), " is not prior to 'end' time (", format(end), ")")
    if (is.null(resolution))
        resolution <- "THREE_MINUTES"
    if (is.null(file))
        file <- paste0("tide_gauge_", agency, "_", ID, "_",
            format(start, "%Y%m%dT%H%M"),
            "_",
            format(end, "%Y%m%dT%H%M"),
            "_",
            resolution,
            "_", variable, ".csv")
    dodDebug(debug, "file=\"", file, "\"\n", sep="")
    dodDebug(debug, "start: \"", start, "\"\n", sep="")
    dodDebug(debug, "end: \"", end, "\"\n", sep="")
    startDigits <- gsub("-", "", start)
    endDigits <- gsub("-", "", end)
    dodDebug(debug, "destdir=\"", destdir, "\"\n", sep="")
    if (agency == "CHS") {
        if (!requireNamespace("rjson", quietly=TRUE))
            stop("must install.packages(\"rjson\") before using download.tideGauge() with agency=\"CHS\"")
        url <- "https://api-iwls.dfo-mpo.gc.ca/api/v1/stations"
        s <- readLines(url, warn=FALSE)
        d <- rjson::fromJSON(s)
        dodDebug(debug, "about to try to find code for ID=\"", ID, "\"\n", sep="")
        if (grepl("a-zA-Z", ID)) {
            dodDebug(debug, "looking up station by name", ID, "\n")
            # officialName: chr "Bedford Institute"
            w <- which(sapply(d, \(s) grepl(ID, s$officialName)))
            if (length(w) == 0L)
                stop("cannot find API code for ID=\"", ID, "\" (interpreted as a name)")
        } else {
            dodDebug(debug, "looking up station by ID number", ID, "\n")
            # code        : chr "00491"
            ID <- as.integer(ID)
            w <- which(sapply(d, \(s) identical(ID, as.integer(s$code))))
            if (length(w) == 0L)
                stop("cannot find API code for ID=\"", ID, "\" (interpreted as a number)")
        }
        ds <- d[[w]]
        stationID <- ds$id
        dodDebug(debug, "calculated CHS station ID code to be \"", stationID, "\"\n", sep="")
        # Rename variables
        variableRemote <- variable
        variableRemote <- gsub("^height$", "wlo", variableRemote)
        variableRemote <- gsub("^heightPredicted$", "wlp", variableRemote)
        url <- sprintf("api-iwls.dfo-mpo.gc.ca/api/v1/stations/%s/data?time-series-code=%s&from=%s&to=%s&resolution=%s",
            stationID,
            variableRemote,
            format(start, "%Y-%m-%dT%H:%M:00Z"),
            format(end, "%Y-%m-%dT%H:%M:00Z"),
            resolution)
        url <- paste0("https://", gsub(":", "%3A", url))
        dodDebug(debug, "url: \"", url, "\"\n", sep="")
        s <- readLines(url, warn=FALSE)
        d <- rjson::fromJSON(s)
        time <- sapply(d, \(x) x$eventDate)
        time <- gsub("Z$", "", gsub("T", " ", time)) # for output
        #qc <- sapply(d, \(x) x$qcFlagCode) # FIXME: unused
        var  <- sapply(d, \(x) x$value)
        if (variable == "height") {
            res <- data.frame("Date Time"=time, "Water Level"=var)
        } else {
            res <- data.frame("Date Time"=time, "Predictions"=var)
        }
        dodDebug(debug, "about to save in file \"", file, "\"\n", sep="")
        write.csv(res, file=file, row.names=FALSE)
        dodDebug(debug, "save data in file \"", file, "\"\n", sep="")
    } else if (agency == "NOAA") {
        #https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?product=predictions&application=NOS.COOPS.TAC.WL&begin_date=20230801&end_date=20230830&datum=MLLW&station=8727520&time_zone=GMT&units=metric&interval=&format=CSV
        server <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
        # Rename variable
        variableRemote <- variable
        variableRemote <- gsub("^height$", "water_level", variableRemote)
        variableRemote <- gsub("^heightPredicted$", "predictions", variableRemote)
        url <- sprintf("%s?product=%s&application=NOS.COOPS.TAC.WL&begin_date=%s&end_date=%s&datum=MLLW&station=%s&time_zone=GMT&units=metric&interval=&format=CSV",
            server, variableRemote, start, end, ID)
    } else {
        stop("unknown agency \"", agency, "\"; try either \"CHS\" or \"NOAA\"")
    }
    dodDebug(debug, "url: ", url, "\n", sep="")
    filename <- try(
        dod.download(url=url, destdir=destdir, file=file, age=age, debug=debug-1L),
        silent=TRUE)
    if (inherits(filename, "try-error"))
        stop("cannot download \"", file, "\" from \"", url, "\"")
    return(filename)
}

if (FALSE) {

}
