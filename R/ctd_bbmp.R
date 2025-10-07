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
#' @param ID a character value indicating the station name. This is
#' usually in a form like `001_1`, but for some years also have some
#' profiles with IDs like `001_01`, as well as `001_1`. Using
#' [dod.ctd.bbmp.index()] to discover IDs usually works, but
#' if that fails, explore the webserver defined by the `server`
#' parameter, to check.
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
#' if (interactive()) { # pkgdown::build_site() cannot handle downloads
#'     library(dod)
#'     # Note: cannot specify year=2025 because the URL is differently constructed
#'     index <- dod.ctd.bbmp.index(year = "2024")
#'     destdir <- tempdir(check = TRUE) # get temporary storage
#'     ctdFile <- dod.ctd("BBMP",
#'         year = index$year[1], ID = index$ID[1],
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
    if (is.null(ID)) stop("ID must be supplied")
    if (!is.character(ID)) stop("ID must be a character value")
    if (length(year) > 1) stop("year must be of length 1")
    if (length(ID) > 1) stop("ID must be of length 1")
    if (length(direction) > 1) stop("direction must be of length 1")
    # server <- "https://cioosatlantic.ca/erddap/files/bio_atlantic_zone_monitoring_program_ctd/Bedford%20Basin%20Monitoring%20Program"
    dodDebug(debug, "  dod.ctd.bbmp(year=", year,
        ", ID=", ID, ", file=\"", file, "\", destdir=\"", destdir, "\"",
        ", age=", age, ", debug=", debug, ")\n  START\n",
        sep = ""
    )
    # https://cioosatlantic.ca/erddap/files/bio_atlantic_zone_monitoring_program_ctd/Bedford%20Basin%20Monitoring%20Program/2024/CTD_BCD2024667_001_1_DN.ODF.nc
    if (!direction %in% c("DN", "UP")) {
        stop("'direction' must be either \"DN\" or \"UP\"")
    }
    url <- paste0(
        server, "/", year, "/CTD_BCD", year, "667_",
        ID, "_",
        direction,
        ".ODF.nc"
    )
    file <- gsub(".*/", "", url)
    dodDebug(debug, "will try downloading from \"", url, "\"\n")
    rval <- try(
        dod.download(
            url = url, file = file,
            destdir = destdir, age = age, quiet = quiet, debug = incrementDebug(debug)
        ),
        silent = TRUE
    )
    if (inherits(rval, "try-error")) {
        stop(
            "Cannot determine the file. Try visiting \"",
            paste0(
                "https://cioosatlantic.ca/erddap/files/",
                "bio_atlantic_zone_monitoring_program_ctd/",
                "Bedford%20Basin%20Monitoring%20Program/"
            ),
            " and descending to the desired year, then dowload a file directly"
        )
    }
    dodDebug(debug, "dod.ctd.bbmp() END (data successfully downloaded to file \"", file, "\")\n")
    rval
} # dod.ctd.bbmp

#' Download an index of BBMP files
#'
#' Download an index of BBMP files available in a specified year. This is done
#' in analysing the webpage of the data server, and so it will likely fail if
#' the format of the webpage is changed. Please inform the developers if you
#' find such a failure, so they can try to make changes.
#'
#' @return `dod.ctd.bbmp.index` returns a data frame with columns
#' named `year`, `ID`, and `direction`. These elements may be supplied
#' as parameters of the same name to [dod.ctd.bbmp()].
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
    dodDebug(debug, "  step 1 (construct url) complete\n")
    # Try to download a URL from which the index will be inferred.
    # If this fails for 2025 (as it does on 2025-04-21), tell the
    # user where to try to manually download the file.  I tried
    # altering the code to try the "provisional" webpage, but the
    # files also have names that don't match the old names, so the
    # code I'm using to scrape id, etc., fails.
    l <- try(readLines(url), silent = TRUE) # takes 0.01s user, 0.002s system, but 1.2s elapsed
    if (inherits(l, "try-error")) {
        stop(
            "Cannot determine an index. Try visiting \"",
            paste0(
                "https://cioosatlantic.ca/erddap/files/",
                "bio_atlantic_zone_monitoring_program_ctd/",
                "Bedford%20Basin%20Monitoring%20Program/"
            ),
            " and dowloading files directly"
        )
    }
    dodDebug(debug, "  step 2 (read URL) complete\n")
    l <- l[grep("D.*ODF.*nc", l)]
    dodDebug(debug, "  step 3 (isolate CTD lines) complete\n")
    l <- gsub("\".*", "", gsub("^.*href=\"", "", l))
    dodDebug(debug, "  step 4 (isolate href lines) complete\n")
    l <- gsub("&#x2e;", ".", gsub("&#x5f;", "_", l))
    dodDebug(debug, "  step 5 (translate underline characters) complete\n")
    #id <- gsub("CTD_[^_]*_([^_]*).*", "\\1", l)
    ID <- gsub("CTD_[^_]*_([^_]*_[^_]*).*", "\\1", l)
    dodDebug(debug, "  step 6 (isolate id) complete (first value=", ID[1], ")\n")
    direction <- gsub(".*_(DN|UP).*", "\\1", l)
    dodDebug(debug, "  step 7 (isolate direction) complete (first value=", direction[1], ")\n")
    rval <- data.frame(year = year, ID = ID, direction = direction)
    dodDebug(debug, "END dod.ctd.bbmp.index(); returning an index with ", nrow(rval), " rows\n")
    rval
} # dod.ctd.bbmp.index
