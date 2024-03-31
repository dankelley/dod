#' Download buoy data
#'
#' This function downloads buoy data from various programs
#' including, as listed in \sQuote{Details}.
#'
#' If `program` is `"MEDS"', then the choices for ID are as follows.
#'
#' * `"Banquereau Bank"`
#' * `"East Scotian Slope"`
#' * `"Halifax"`
#' * `"Halifax DISCUS TriAx"`
#' * `"Halifax Harbour"`
#' * `"Laurentian Fan"`
#' * `"Minas Basin"`
#' * `"Port Hope"`
#' * `"Prince Edward Point"`
#' * `"Tail of the Bank"`
#'
#' If `program` is `"smartatlantic"', then the choices, all for
#' buoys in the Atlantic Provinces, are as follows.
#'
#' * `"h1"` for a buoy near Duncan Reef, Nova Scotia
#' (see <https://www.smartatlantic.ca/station_alt.html?id=halifax_h1>)
#'
#' * `"halifax"` for a buoy in Herring Cove, Nova Scotia
#' (see <https://www.smartatlantic.ca/station_alt.html?id=halifax>)
#'
#' * `"hkb"` for a buoy near Meagher's Beach, Nova Scotia
#' (see <https://www.smartatlantic.ca/station_alt.html?id=halifax_hk4>)
#'
#' * `"saint_john"` for a buoy in the Bay of Fundy, near St John, New Brunswick
#' (see <https://www.smartatlantic.ca/station_alt.html?id=saintjohn>)
#'
#' * `"saint_johns"` for a buoy near St John's Harbour, Newfoundland
#' (see <https://www.smartatlantic.ca/station_alt.html?id=stjohns>)
#'
#' @param program argument specifying the desired oceanographic
#' program to download buoy data from. This must be either
#' `"MEDS"` or `"smartatlantic"`.
#'
#' @param ID a character value indicating the ID of the instrument (see \sQuote{Details})
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template debugTemplate
#'
## @importFrom curl curl_download
#' @importFrom utils unzip
#'
#' @return If `index` is TRUE, and `program` is `"BBMP"` or `"BATS"`,
#' return a data frame.  Otherwise, return the name of the
#' downloaded file.
#'
#' @examples
#' # Show significant wave height in Halifax Harbour over past 28 days.
#' # Note that a temporary directory is used, in case
#' # the package is later submitted to CRAN, which does not
#' # permit downloads to the working directory.
#' library(dod)
#' tmpdir <- tempfile() # temporary directory, removed at the end
#' dir.create(tmpdir)
#' file <- dod.buoy("smartatlantic", "h1", destdir = tmpdir)
#' col.names <- strsplit(readLines(file, 1), ",")[[1]]
#' d <- read.csv(file, skip = 2, col.names = col.names)
#' d$t <- as.POSIXct(d$time, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")
#' look <- d$t > (max(d$t, na.rm = TRUE) - 28 * 86400)
#' plot(d$t[look], d$wave_ht_sig[look],
#'     type = "l", xaxs = "i",
#'     xlab = "", ylab = "Sig. Wave Ht. [m]"
#' )
#' unlink(tmpdir, recursive = TRUE) # remove temporary directory
#'
#' @export
#'
#' @author Dan Kelley
# https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData/c44258_csv.zip
# https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData/c44139_csv.zip
dod.buoy <- function(program, ID = NULL, destdir = ".", age = 1, debug = 0) {
    programAllowed <- c("MEDS", "smartatlantic")
    if (missing(program)) {
        stop("must supply 'program'; try one of: \"", paste(programAllowed, collapse = "\" \""), "\"")
    }
    if (is.null(ID)) {
        stop("Must provide an ID argument")
    }
    dodDebug(debug, "dod.buoy(program=\"", program, "\", ID=\"", ID, "\", ...)\n", sep = "")
    if (program == "MEDS") {
        loc <- list(
            "East Scotian Slope" = "44137",
            "Banquereau Bank" = "44139", "Halifax Harbour" = "44258",
            "Halifax" = "44172", "Halifax DISCUS TriAx" = "44299",
            "Tail of the Bank" = "44140", "Laurentian Fan" = "44141",
            "Port Hope" = "45135", "Prince Edward Point" = "45135",
            "Minas Basin" = "MEDS027"
        )
        dodDebug(debug, "Initial ID=", ID, "\n")
        if (ID %in% names(loc)) {
            ID <- loc[[ID]]
        }
        dodDebug(debug, "Final ID=", ID, "\n")
        server <- "https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData"
        url <- paste0(server, "/c", ID, "_csv.zip")
        zipfile <- paste0("c", ID, "_csv.zip")
        dodDebug(debug, url)
        dod.download(url = url, file = zipfile, age = age, debug = debug - 1)
        unzip(zipfile)
        unlink(zipfile)
        # NOTE: we should delete the zipfile too; see ?unlink
        rval <- paste0("C", ID, ".CSV")
        dodDebug(debug, "downloaded \"", rval, "\"\n", sep = "")
        return(rval)
    } else if (program == "smartatlantic") {
        IDabbrev <- list(
            "h1" = "SmartAtlantic_XEOS_h1_buoy",
            "hkb" = "SmartAtlantic_XEOS_hkb_buoy",
            "halifax" = "SMA_halifax",
            "saint_john" = "SMA_saint_john",
            "saint_johns" = "SMA_saint_johns"
        )
        IDallowed <- names(IDabbrev)
        if (!ID %in% IDallowed) {
            stop(
                "ID \"", ID, "\" not permitted; try one of \"",
                paste(IDallowed, collapse = "\" \""), "\""
            )
        }
        # Access file directly (will this always work, though?)
        base <- "https://www.smartatlantic.ca/erddap/files/"
        if (ID == "h1") {
            #https://www.smartatlantic.ca/erddap/files/SmartAtlantic_XEOS_h1_buoy/smb_h1_data.csv
            #dodDebug(debug, "handling \"", ID, "\" as a direct download\n", sep = "")
            file <- "smb_h1_data.csv"
            url <- paste0(base, "SmartAtlantic_XEOS_h1_buoy/", file)
            return(dod.download(url = url, file = file, age = age, destdir = destdir, debug = debug - 1))
        }
        if (ID == "halifax") {
            #https://www.smartatlantic.ca/erddap/files/SMA_halifax/smb_halifax.csv
            #dodDebug(debug, "handling \"", ID, "\" as a direct download\n", sep = "")
            file <- "smb_halifax.csv"
            url <- paste0(base, "SMA_halifax/", file)
            return(dod.download(url = url, file = file, age = age, destdir = destdir, debug = debug - 1))
        }
        if (ID == "hkb") {
            # https://www.smartatlantic.ca/erddap/files/SmartAtlantic_XEOS_hkb_buoy/smb_hkb_data.csv
            #dodDebug(debug, "handling \"", ID, "\" as a direct download\n", sep = "")
            file <- "smb_hkb_data.csv"
            url <- paste0(base, "SmartAtlantic_XEOS_hkb_buoy/", file)
            return(dod.download(url = url, file = file, age = age, destdir = destdir, debug = debug - 1))
        }
        if (ID == "saint_john") {
            #dodDebug(debug, "handling \"", ID, "\" as a direct download\n", sep = "")
            file <- "smb_saint_john.csv"
            url <- paste0(base, "SMA_saint_john/", file)
            return(dod.download(url = url, file = file, age = age, destdir = destdir, debug = debug - 1))
        }
        if (ID == "saint_johns") {
            #https://www.smartatlantic.ca/erddap/files/SMA_st_johns/smb_st_johns.csv
            #dodDebug(debug, "handling \"", ID, "\" as a direct download\n", sep = "")
            file <- "smb_st_johns.csv"
            url <- paste0(base, "SMA_st_johns/", file)
            return(dod.download(url = url, file = file, age = age, destdir = destdir, debug = debug - 1))
        }
        stop("Unknown ID \"", ID, "\" (programming error: please reort this)")
        #<old> IDfull <- IDabbrev[ID]
        #<old> dodDebug(debug, "handling \"", ID, "\", i.e. \"", IDfull, "\"\n")
        #<old> file <- paste0(IDfull, ".csv")
        #<old> url <- paste0(
        #<old>     "https://www.smartatlantic.ca/erddap/tabledap/",
        #<old>     file,
        #<old>     "?",
        #<old>     "time,",
        #<old>     "latitude,",
        #<old>     "longitude,",
        #<old>     "temperature,",
        #<old>     "wave_ht_sig,",
        #<old>     "wave_period_max,",
        #<old>     "wave_ht_max,",
        #<old>     "wave_dir_avg,",
        #<old>     "wave_spread_avg,",
        #<old>     "sea_surface_wave_mean_period,",
        #<old>     "sample_quality"
        #<old>     # "&time>=2024-03-22T00:00:00Z",
        #<old>     # "&time<=2024-03-29T22:18:00Z"
        #<old> )
        #<old> filename <- paste0(destdir, "/", file)
        #<old> # The next two lines showed that the curl:: method is 20%
        #<old> # faster in elapsed time, although 2.5X faster in user time;
        #<old> # Frankly, either would be fine, because elapsed time is
        #<old> # what the user sees. I will stick with download.file()
        #<old> # because it does not force a dependence on another
        #<old> # library.
        #<old> #<SPEED TEST> print(system.time(curl::curl_download(url, filename)))
        #<old> #<SPEED TEST> print(system.time(download.file(url, filename)))
        #<old> # curl::curl_download(url, filename)
        #<old> download.file(url, filename)
        #<old> dodDebug(debug, "downloaded \"", filename, "\"\n", sep = "")
        #<old> return(filename)
    } else {
        stop("unrecognized program=\"", program, "\"; try one of: \"", paste(programAllowed, collapse = "\" \""), "\"")
    }
}
