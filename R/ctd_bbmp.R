#' Download CTD data from the Bedford Basin Mooring Project
#'
#' [dod.ctd.bbmp] can retrieve both index files and data files from the Bedford
#' Basin Mooring Project (BBMP). Since the naming convention of the data files
#' may alter from year to year, it is important to start by downloading an
#' index first, using [dod.ctd.bbmp.index] first, to determine a file of
#' interest; see \sQuote{Examples}.
#'
#' @param year a numeric or character value specifying the year of interest.
#' If this is not provided, it defaults to the current year.
#'
#' @param ID an integer value giving the sequence number of the CTD
#' cast in the given year.
#'
#' @param direction a character value indicating the direction
#' of the cast, either `"DN"` (the default) or `"UP"`.
#'
#' @param server character value indicating the base name of the server.
#' This may change over time.
#'
#' @param file character value giving the name to be used for
#' the downloaded file.  If this is NULL (which is the default) then
#' the filename is as on the remote data server.
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template quietTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv
#'
#' @return [dod.ctd.bbmp] returns a character value naming the file that was
#' retrieved. See the \sQuote{Examples} section for an example of a typical
#' workflow.
#'
#' @section Historical Note:
#'
#' Sometime in prior to April 2025, the DFO server and the format
#' of the data files both changed, and this necessitated changes
#' to [dod.ctd.bbmp()]. Also, for clarity of use, the new
#' function [dod.ctd.bbmp.index()] was added then.
#'
#' @examples
#' # Download and study first BBMP CTD file of year 2024
#' if (interactive()) { # sidestep a pkgdown::build_site() error
#'     library(dod)
#'     # Note: cannot specify year=2025 because the URL is differently constructed
#'     index <- dod.ctd.bbmp.index(year = "2024")
#'     destdir <- tempdir() # get temporary storage
#'     ctdFile <- dod.ctd("BBMP",
#'         year = index$year[1], ID = index$id[1],
#'         direction = "DN", destdir = destdir
#'     )
#'     # Use oce to read, summarize and plot the data.
#'     library(oce)
#'     ctd <- read.netcdf(ctdFile) |>
#'         rename() |>
#'         as.ctd()
#'     summary(ctd)
#'     plot(ctd)
#'     unlink(destdir, recursive = TRUE) # clean up temporary storage
#' }
#'
#' @family functions that download files
#'
#' @export
#'
#' @author Dan Kelley
dod.ctd.bbmp <- function(year, ID = NULL, direction = "DN",
                         server = paste0(
                             "https://cioosatlantic.ca/erddap/files/",
                             "bio_atlantic_zone_monitoring_program_ctd/",
                             "Bedford%20Basin%20Monitoring%20Program"
                         ),
                         file = NULL, destdir = ".",
                         age = 0, quiet = FALSE, debug = 0) {
    if (missing(year)) {
        year <- format(Sys.Date(), "%Y")
    }
    if (is.null(ID)) {
        stop("ID must be supplied")
    }
    # server <- "https://cioosatlantic.ca/erddap/files/bio_atlantic_zone_monitoring_program_ctd/Bedford%20Basin%20Monitoring%20Program"
    dodDebug(debug, "  dod.ctd.bbmp(year=", year,
        ", ID=", ID, ", file=\"", file, "\", destdir=\"", destdir, "\"",
        ", age=", age, ", debug=", debug, ")\n  BEGIN\n",
        sep = ""
    )
    # https://cioosatlantic.ca/erddap/files/bio_atlantic_zone_monitoring_program_ctd/Bedford%20Basin%20Monitoring%20Program/2024/CTD_BCD2024667_001_1_DN.ODF.nc
    if (!direction %in% c("DN", "UP")) {
        stop("'direction' must be either \"DN\" or \"UP\"")
    }
    url <- paste0(
        server, "/", year, "/CTD_BCD", year, "667_",
        sprintf("%03d", as.integer(ID)), "_1_",
        direction,
        ".ODF.nc"
    )
    dodDebug(debug, "will try downloading from \"", url, "\"\n")
    file <- gsub(".*/", "", url)
    file <- dod.download(url = url, file = file, destdir = destdir, age = age, quiet = quiet, debug = debug - 1)
    dodDebug(debug, "END dod.ctd.bbmp()\n", sep = "")
    return(file)
} # dod.ctd.bbmp

#' Download an index of BBMP files
#'
#' Download an index of BBMP files available in a specified year. This is done
#' in analysing the webpage of the data server, and so it will likely fail if
#' the format of the webpage is changed. Please inform the developers if you
#' find such a failure, so they can try to make changes.
#'
#' @param year integer or character value indicating the year of interest.
#' Note that the year 2025 will not work, because the server supplies
#' information for that year in a pattern that does not match other years. Since
#' the pattern appears to be temporary, the present code is not set up
#' to handle it.
#'
#' @param server character value indicating the base name of the server.
#' This may change over time.
#'
#' @template debugTemplate
#'
#' @export
#'
#' @author Dan Kelley
dod.ctd.bbmp.index <- function(year,
                               server = paste0(
                                   "https://cioosatlantic.ca/erddap/files/",
                                   "bio_atlantic_zone_monitoring_program_ctd/",
                                   "Bedford%20Basin%20Monitoring%20Program"
                               ), debug = 0) {
    dodDebug(debug, "dod.ctd.bbmp.index(year=", year, ", ...) START\n")
    url <- paste0(server, "/", year, "/")
    l <- try(readLines(url), silent = TRUE) # takes 0.01s user, 0.002s system, but 1.2s elapsed
    if (inherits(l, "try-error")) {
        stop("cannot download \"", url, "\"")
    }
    dodDebug(debug, "  step 1 (read URL) complete\n")
    l <- l[grep("CTD.*BCD", l)]
    dodDebug(debug, "  step 2 (isolate CTD lines) complete\n")
    l <- gsub("\".*", "", gsub("^.*href=\"", "", l))
    dodDebug(debug, "  step 3 (further isolate href lines) complete\n")
    l <- gsub("&#x2e;", ".", gsub("&#x5f;", "_", l))
    dodDebug(debug, "  step 4 (translate underline characters) complete\n")
    id <- gsub("CTD_[^_]*_([^_]*).*", "\\1", l)
    dodDebug(debug, "  step 5 (isolate id) complete\n")
    direction <- gsub(".*_(DN|UP).*", "\\1", l)
    dodDebug(debug, "  step 6 (isolate direction) complete\n")
    rval <- data.frame(year = year, id = id, direction = direction)
    dodDebug(debug, "END dod.ctd.bbmp.index(); returning an index with ", nrow(rval), " rows\n")
    rval
} # dod.ctd.bbmp.index
