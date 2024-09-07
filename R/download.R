#' Download a file with error checking
#'
#' `dod.download()` is mainly intended for use by other functions
#' in the `dod` package.
#'
#' @param url character value giving the web address of a file to be
#' downloaded. This has different values for different file types.
#'
#' @param file character value giving the name to be used for
#' the downloaded file.
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template quietTemplate
#'
#' @template debugTemplate
#'
#' @return `dod.download` returns a character value holding the full
#' name of the file, including the path to `destdir`.
#'
#' @importFrom utils download.file
#'
#' @export
dod.download <- function(url = NULL, file = NULL, destdir = ".", age = 0, quiet = TRUE, debug = 0) {
    if (is.null(url)) {
        stop("url must not be NULL")
    }
    if (!dir.exists(destdir)) {
        stop("destdir \"", destdir, "\" does not exist; please create it first.")
    }
    dodDebug(debug, "    dod.download(",
        "url=\"", url, "\", ",
        "file=\"", file, "\", ",
        "destdir=\"", destdir, "\", ",
        "age=", age, ", quiet=", quiet, ", debug=", debug, ")\n    {\n",
        sep = ""
    )
    filepath <- file.path(destdir, file)
    # For negative age, set timer to 1000 years
    if (age < 0) {
        age <- 1000 * 365
    }
    if (file.exists(filepath)) {
        mtime <- file.info(filepath)$mtime
        fileAge <- (as.numeric(Sys.time()) - as.numeric(mtime)) / 86400
        fileAgeMsg <- if (fileAge < 1 / 24) {
            sprintf("%.1f minutes", fileAge * 24 * 60)
        } else if (fileAge < 1) {
            sprintf("%.1f hours", fileAge * 24)
        } else if (fileAge < 3 * 28) {
            sprintf("%.1f days", fileAge)
        } else {
            sprintf("%.1f months", fileAge / 28)
        }
        if (fileAge < age) {
            dodDebug(debug, "        an existing file is ", fileAgeMsg,
                " old, so it will not be downloaded\n    } # dod.download()\n",
                sep = ""
            )
            return(filepath)
        } else {
            dodDebug(debug, "      an existing file is ", fileAgeMsg, " old, so must download a newer version\n", sep = "")
        }
    } else {
        dodDebug(debug, "      new file, so must download\n")
    }
    owarn <- options("warn")$warn
    options(warn = -1)
    # t <- try(download.file(url = url, destfile = filepath, quiet = silent), silent = silent)
    t <- try(curl::curl_download(url = url, destfile = filepath, quiet = quiet, mode = "wb"),
        silent = quiet
    )
    options(warn = owarn)
    if (inherits(t, "try-error")) {
        stop("An error occured when trying to download \"", url, "\"")
    }
    dodDebug(debug, "    } # dod.download()\n")
    filepath
}
