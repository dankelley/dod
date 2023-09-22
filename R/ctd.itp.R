#' Download CTD data from the Ice-Tethered Profiler program
#'
#' [dod.ctd.itp()] downloads CTD data from the Woods Hole Oceanographic
#' Institutions Ice-Thethered Profile program
#' <https://www2.whoi.edu/site/itp/>.  Note that this
#' server does not provide a searchable index, so users will need to visit the website
#' <https://www2.whoi.edu/site/itp/data/> to determine IDs of interest.
#'
#' @param ID an integer specifying the ID of the profiler that is of interest.
#'
#' @param info a logical value.  If `info` is FALSE, which is the default, then
#' [dod.ctd.itp()] downloads the zip-format file that can be expanded into
#' a directory that holds a data file for each profile made by the
#' profiler with the given `ID`.  The name of the zip file is returned.
#' (This file must be unzipped to access the data; [unzip()] is a convenient
#' way to do this.)
#' On the other hand, if `info` is TRUE, then
#' nothing is downloaded, and [dod.ctd.itp()] returns a
#' URL holding information about the profiler.
#'
#' @param file character value giving the name to be used for the downloaded
#' file.  If `file` is NULL, which is the default, then the filename on the
#' server is used.
#'
#' @template destdirTemplate
#'
#' @template ageTemplate
#'
#' @template debugTemplate
#'
#' @importFrom utils read.csv
#'
#' @return [dod.ctd.itp()] returns the full pathname of the
#' downloaded zip file, or a URL of an information site,
#' depending on the value of `info`.
#'
#' @family functions that download CTD data
#'
#' @export
#'
#' @author Dan Kelley
dod.ctd.itp <- function(ID, info=FALSE, file=NULL, destdir=".", age=0, debug=0)
{
    # For ITP 135, info is at
    # https://www2.whoi.edu/site/itp/data/active-systems/itp-135/
    # and data is at
    # https://scienceweb.whoi.edu/itp/data/itp135grddata.zip
    if (missing(ID))
        stop("ID must be supplied")
    owarn <- options("warn")$warn
    options(warn=-1)
    IDorig <- ID
    ID <- as.integer(ID)
    options(warn=owarn)
    if (is.na(ID))
        stop("ID=", IDorig, " is neither an integer nor a string denoting an integer")
    infoURL <- sprintf("https://www2.whoi.edu/site/itp/data/active-systems/itp-%s", ID)
    if (info)
        return(infoURL)
    dataURL <- sprintf("https://scienceweb.whoi.edu/itp/data/itp%sgrddata.zip", ID)
    dodDebug(debug, "infoURL=\"", infoURL, "\"\n", sep="")
    dodDebug(debug, "dataURL=\"", dataURL, "\"\n", sep="")
    if (is.null(file))
        file <- gsub(".*/", "", dataURL)
    # Try e.g. b50056_ctd.txt and if that fails, try b50056_ctd_QC.txt
    filename <- try(dod.download(url=dataURL, destdir=destdir, file=file, age=age, debug=debug-1),
        silent=TRUE)
    if (inherits(filename, "try-error"))
        stop("Unable to download \"", dataURL, "\" to \"", filename, "\"")
    return(filename)
}
