#' Download tide-gauge data
#'
#' [dod.tideGauge()] downloads tide-gauge data from NOAA, returning
#' the name of the downloade file.
#'
#' @param ID a character (or possibly integer) value specifying the
#' numeric identifier of the tide gauge.  This is not the name of
#' the station; for example, the station at Cedar Key in Florida is
#' queried by using `ID=8727520`.
#'
#' @param start,end the start and end days of the desired observations.
#' This may either be a Date object, or character value that can be
#' converted to one by [as.Date()], e.g. "2023-08-30".
#' If not provided, `start` is set to 30 days before the present day,
#' and `end` is set to the present day.
#'
#' @param variable a character value indicating the name of the
#' variable for which data is to be downloaded.  At present, this
#' must be either `"water_level"` or `"predictions"`, with the
#' former being the default.
#'
#' @param file a character value indicating the name to be used
#' for the downloaded data.  If not provided, this is constructed
#' as e.g. `"tide_N_S_E_V.csv"` where `N` is the station number,
#' `S` and `E` are the start and end dates written in 8-digit format
#' (i.e. sans the `"-"` characters), and `V` is the variable
#' name.
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv
#'
#' @return [dod.tideGauge()] returns the full pathname of the
#' downloaded file.
#'
#' @export
dod.tideGauge <- function(ID=NULL, start=Sys.Date()-30,
    end=Sys.Date(), variable="water_level", file=NULL, destdir=".", age=1, debug=0)
{
    if (is.null(ID))
        stop("ID must be supplied")
    if (!is.character(ID))
        ID <- as.character(ID)
    if (is.null(file))
        file <- paste0("tide_gauge_", ID, "_", gsub("-", "", start), "_", gsub("-", "", end),
            "_", variable, ".csv")
    dodDebug(debug, "file=\"", file, "\"\n", sep="")
    dodDebug(debug, "start: \"", start, "\"\n", sep="")
    dodDebug(debug, "end: \"", end, "\"\n", sep="")
    start <- gsub("-", "", start)
    end <- gsub("-", "", end)
    dodDebug(debug, "destdir=\"", destdir, "\"\n", sep="")
    #https://api.tidesandcurrents.noaa.gov/api/prod/datagetter?product=predictions&application=NOS.COOPS.TAC.WL&begin_date=20230801&end_date=20230830&datum=MLLW&station=8727520&time_zone=GMT&units=metric&interval=&format=CSV
    server <- "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter"
    url <- sprintf("%s?product=%s&application=NOS.COOPS.TAC.WL&begin_date=%s&end_date=%s&datum=MLLW&station=%s&time_zone=GMT&units=metric&interval=&format=CSV",
        server, variable, start, end, ID)
    dodDebug(debug, "url: ", url, "\n", sep="")
    filename <- try(
        dod.download(url=url, destdir=destdir, file=file, age=age, debug=debug-1L),
        silent=TRUE)
    if (inherits(filename, "try-error"))
        stop("cannot download \"", file, "\" from \"", url, "\"")
    return(filename)
}
