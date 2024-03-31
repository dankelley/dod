#' Download buoy data
#'
#' This function downloads buoy data from various programs
#' including...
#'
#' @param program argument specifying the desired oceanographic
#' program to download buoy data from. If this is not supplied,
#' `dod.buoy()` will print a list of possibilities.
#'
#' @param ID a character value indicating the ID of the instrument.
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
#' return a data frame.  Otherwise, return the name of the downloaded file.
#'
#' @examples
#' # Show recent variation of significant wave height in Halifax
#' # Harbour.
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
#' plot(d$t, d$wave_ht_sig, type = "l",
#'     xlab = "", ylab = "Sig. Wave Ht. [m]"
#' )
#' unlink(tmpdir, recursive = TRUE) # remove temporary directory
#'
#' @export
#'
#' @author Dan Kelley
# https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData/c44258_csv.zip
# https://www.meds-sdmm.dfo-mpo.gc.ca/alphapro/wave/waveshare/csvData/c44139_csv.zip
dod.buoy <- function(program, ID = NULL, destdir = ".", age = age, debug = 0) {
    programAllowed <- c("MEDS", "smartatlantic")
    if (missing(program)) {
        stop("must supply 'program'; try one of: \"", paste(programAllowed, collapse = "\" \""), "\"")
    }
    if (is.null(ID)) {
        stop("Must provide an ID argument")
    }
    dodDebug(debug, "dod.buoy(program=\"", program, "\", ID=\"", ID, "\", ...)\n", sep = "")
    if (program == "MEDS") {
        dodDebug(debug, "handling program \"MEDS\"\n")
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
        dodDebug(debug, "handling program \"smartlatlantic\"\n")
        if (grepl("h1", ID)) {
            dodDebug(debug, "handling \"h1_buoy\"\n")
            file <- "XEOS_h1_buoy.csv"
            url <- paste0(
                "https://www.smartatlantic.ca/",
                "erddap/tabledap/SmartAtlantic_",
                file,
                "?",
                "time,",
                "latitude,",
                "longitude,",
                "temperature,",
                "wave_ht_sig,",
                "wave_period_max,",
                "wave_ht_max,",
                "wave_dir_avg,",
                "wave_spread_avg,",
                "sea_surface_wave_mean_period,",
                "sample_quality"
                #"&time>=2024-03-22T00:00:00Z",
                #"&time<=2024-03-29T22:18:00Z"
            )
            filename <- paste0(destdir, "/", file)
            # The next two lines showed that the curl:: method is 20%
            # faster in elapsed time, although 2.5X faster in user time;
            # Frankly, either would be fine, because elapsed time is
            # what the user sees. I will stick with download.file()
            # because it does not force a dependence on another
            # library.
            #<SPEED TEST> print(system.time(curl::curl_download(url, filename)))
            #<SPEED TEST> print(system.time(download.file(url, filename)))
            #curl::curl_download(url, filename)
            download.file(url, filename)
            dodDebug(debug, "downloaded \"", filename, "\"\n", sep = "")
            return(filename)
        } else {
            stop("ID=\"", ID, "\" not recognized for program=\"smartatlantic\"; try \"h1\"")
        }
    } else {
        stop("unrecognized program=\"", program, "\"; try one of: \"", paste(programAllowed, collapse = "\" \""), "\"")
    }
}
