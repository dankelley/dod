#' Download CTD data from the Bedford Basin Mooring Project
#'
#' [dod.ctd.bbmp] can retrieve both index files and data files
#' from the Bedford Basin Mooring Project (BBMP). Since the naming
#' convention of the data files may alter from year to year, it is
#' important to start by downloading an index first, to ascertain
#' the name of a file of interest; see \sQuote{Examples}.
#'
#' @param year a numeric or character value specifying the year of interest.
#' If this is not provided, it defaults to the current year.
#'
#' @param index a boolean value indicating whether to return a data frame with
#' an index of available files for the given year.  Note that this is done in
#' analysing the webpage of the data server, and so it will likely fail if the
#' format of the webpage is changed. Please inform the developers if you find
#' such a failure, so they can try to make changes.
#'
#' @param ID an integer value giving the sequence number of the CTD
#' cast in the given year.
#'
#' @param direction a character value indicating the direction
#' of the cast, either `"DN"` (the default) or `"UP"`.
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
#' @return [dod.ctd.bbmp] returns one of two things: (a) if `index` is TRUE, it
#' returns a data frame with columns named `year`, `id` and `direction` or (b)
#' a character value naming the file that was retrieved. See the
#' \sQuote{Examples} section for an example of a typical workflow.
#'
#' @section Historical Note:
#'
#' Sometime in prior to April 2025, the DFO server and the format
#' of the data files both changed, and this necessitated changes
#' to `dod.ctd.bbmp()`. Note that the server no longer
#' supplies index files, so the present code tries to discover
#' information by scraping the website ... making it very sensitive
#' to changes and thus brittle.
#'
#' @examples
#' # Download and study first BBMP CTD file of year 2024
#' if (interactive()) { # sidestep a pkgdown::build_site() error
#'     library(dod)
#'     # Note: cannot specify year=2025 because the URL is differently constructed
#'     index <- dod.ctd("BBMP", year = "2024", index = TRUE)
#'     destdir <- tempdir() # get temporary storage
#'     ctdFile <- dod.ctd("BBMP", year = index$year[1], ID = index$id[1], destdir = destdir)
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
dod.ctd.bbmp <- function(year, index = FALSE,
                         ID = NULL, direction = "DN", file = NULL,
                         destdir = ".", age = 0, quiet = FALSE, debug = 0) {
    if (missing(year)) {
        year <- format(Sys.Date(), "%Y")
    }
    if (!is.logical(index)) {
        stop("'index' must be a logical value")
    }
    if (is.null(ID) && !index) {
        stop("ID must be supplied")
    }
    server <- "https://cioosatlantic.ca/erddap/files/bio_atlantic_zone_monitoring_program_ctd/Bedford%20Basin%20Monitoring%20Program"
    dodDebug(debug, "  dod.ctd.bbmp(year=", year,
        ", ID=", ID, ", file=\"", file, "\", destdir=\"", destdir, "\"",
        ", age=", age, ", debug=", debug, ")\n  BEGIN\n",
        sep = ""
    )
    # https://cioosatlantic.ca/erddap/files/bio_atlantic_zone_monitoring_program_ctd/Bedford%20Basin%20Monitoring%20Program/2024/CTD_BCD2024667_001_1_DN.ODF.nc
    if (index) {
        base <- paste0(
            "https://cioosatlantic.ca/erddap/files/",
            "bio_atlantic_zone_monitoring_program_ctd/",
            "Bedford%20Basin%20Monitoring%20Program/"
        )
        url <- paste0(base, year, "/")
        l <- readLines(url) # takes 0.01s user, 0.002s system, but 1.2s elapsed
        l <- l[grep("CTD.*BCD", l)]
        l <- gsub("\".*", "", gsub("^.*href=\"", "", l))
        l <- gsub("&#x2e;", ".", gsub("&#x5f;", "_", l))
        id <- gsub("CTD_[^_]*_([^_]*).*", "\\1", l)
        direction <- gsub(".*_(DN|UP).*", "\\1", l)
        return(data.frame(year = year, id = id, direction = direction))
    }
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
