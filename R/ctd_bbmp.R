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
## @param index a boolean value. If this is TRUE, then [dod.ctd.bbmp]
## downloads an index file that provides the names of files containing CTD data
## along with the sampling dates.  The value of `ID` is ignored in this case.
## By contrast, if `index` is FALSE, then [dod.ctd.bbmp] downloads
## the CTD file with the specified `ID`.  See \sQuote{Examples}.
#'
#' @param ID an integer value giving the sequence number of the CTD
#' cast in the given year.
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
#' @return [dod.ctd.bbmp] returns a character value naming the file that
#' was retrieved. This may be either an index file or a data file; see
#' the \sQuote{Examples} section for an example of a typical workflow.
#'
#' @section Historical Note:
#'
#' Sometime in prior to April 2025, the DFO server and the format
#' of the data files both changed, and this necessitated changes
#' to both `dod.ctd.bbmp()`. Note that the server no longer
#' supplies index files, so a parameter of the same name
#' was deleted on 2024-04-19.
#'
#' @examples
#' # Download and study first BBMP CTD file of year 2024
#' if (interactive()) { # sidestep a pkgdown::build_site() error
#'     # NOTE: data file is removed at end, to pass CRAN checks
#'     library(dod)
#'     library(oce)
#'     destdir <- tempdir()
##     if (FALSE) { # this fails since the ERDAPP does not serve an index file
##         indexFile <- dod.ctd("BBMP", index = TRUE, destdir = destdir)
##         index <- read.csv(indexFile, header = FALSE, col.names = c("file", "time"), skip = 3)
##         # Download the first file in the index
##         ctdFile <- dod.ctd("BBMP", ID = index$file[1], destdir = destdir)
##     }
##     # https://cioosatlantic.ca/erddap/files/bio_atlantic_zone_monitoring_program_ctd/Bedford%20Basin%20Monitoring%20Program/2024/CTD_BCD2024667_001_1_DN.ODF.nc
#'     # Note: cannot specify year=2025 as of 2025-04-19 because of URL oddity
#'     ctdFile <- dod.ctd("BBMP", year = 2024, ID = 1, destdir = destdir)
#'     # Use oce to read, summarize and plot the data.
#'     ctd <- read.netcdf(ctdFile) |>
#'         rename() |>
#'         as.ctd()
#'     summary(ctd)
#'     plot(ctd)
#'     unlink(destdir, recursive = TRUE)
#'}
#'
#' @family functions that download files
#' @export
## dod.ctd.bbmp <- function(year, ID = NULL, index = FALSE, file = NULL, destdir = ".", age = 0, quiet = FALSE, debug = 0) {
dod.ctd.bbmp <- function(year, ID = NULL, file = NULL, destdir = ".", age = 0, quiet = FALSE, debug = 0) {
    #server <- "ftp://ftp.dfo-mpo.gc.ca/BIOWebMaster/BBMP/ODF"
    if (missing(year)) {
        year <- format(Sys.Date(), "%Y")
    }
    if (is.null(ID)) {
        stop("ID must be supplied")
    }
    server <- "https://cioosatlantic.ca/erddap/files/bio_atlantic_zone_monitoring_program_ctd/Bedford%20Basin%20Monitoring%20Program"
    dodDebug(debug, "  dod.ctd.bbmp(year=", year,
        ", ID=", ID, ", file=\"", file, "\", destdir=\"", destdir, "\"",
        ", age=", age, ", debug=", debug, ")\n  BEGIN\n",
        sep = ""
    )
    #https://cioosatlantic.ca/erddap/files/bio_atlantic_zone_monitoring_program_ctd/Bedford%20Basin%20Monitoring%20Program/2024/CTD_BCD2024667_001_1_DN.ODF.nc
    url <- paste0(server, "/", year, "/CTD_BCD", year, "667_", sprintf("%03d", ID), "_1_DN.ODF.nc")
    dodDebug(debug, "will try downloading from \"", url, "\"\n")
    # if (index) {
    #     file <- if (is.null(file)) paste0(year, "667ODFSUMMARY.tsv") else paste0(file, ".tsv")
    #     url <- paste0(server, "/", paste0(year, "667ODFSUMMARY.tsv"))
    #     dodDebug(debug, "  will download index from ", url, "\n", sep = "")
    #     dod.download(url = url, file = file, destdir = destdir, age = age, quiet = quiet, debug = debug - 1)
    #     url <- paste0(server, "/", file)
    #     file <- paste0(destdir, "/", file)
    #     dodDebug(debug, "  downloaded index to local file \"", file, "\"\n", sep = "")
    #     dodDebug(debug, "  END dod.ctd.bbmp()\n", sep = "")
    #     return(file)
    # }
    # if (is.null(file)) {
    #     file <- if (grepl("ODF", ID == TRUE)) gsub("\\.ODF", "", ID) else ID
    # }
    # https://cioosatlantic.ca/erddap/files/bio_atlantic_zone_monitoring_program_ctd/Bedford%20Basin%20Monitoring%20Program/2024/CTD_BCD2024667_001_1_DN.ODF.nc
    #url <- paste0(server, "/", ID)
    file <- gsub(".*/", "", url)
    #message(oce::vectorShow(url))
    #message(oce::vectorShow(file))
    file <- dod.download(url = url, file = file, destdir = destdir, age = age, quiet = quiet, debug = debug - 1)
    dodDebug(debug, "END dod.ctd.bbmp()\n", sep = "")
    return(file)
} # dod.ctd.bbmp
