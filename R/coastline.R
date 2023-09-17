#' Download a coastline File
#'
#' Constructs a query to the NaturalEarth server (see reference 1) to download coastline
#' data (or lake data, river data, etc) in any of three resolutions.
#'
#' @param resolution A character value specifying the desired resolution. The permitted
#' choices are `"10m"` (for 1:10M resolution, the most detailed),
#' `"50m"` (for 1:50M resolution)
#' and `"110m"` (for 1:110M resolution). If `resolution` is not supplied,
#' `"50m"` will be used.
#'
#' @param item A character value indicating the quantity to be downloaded.
#' This is normally one of `"coastline"`, `"land"`, `"ocean"`,
#' `"rivers_lakes_centerlines"`, or `"lakes"`, but the NaturalEarth
#' server has other types, and advanced users can discover their names by inspecting
#' the URLs of links on the NaturalEarth site, and use them for `item`.
#' If `item` is not supplied, it defaults to `"coastline"`.
#'
#' @template destdirTemplate
#'
#' @param destfile optional name of destination file. If not provided,
#' this function creates a reasonable name.
#'
#' @param server A character value specifying the server that is to supply
#' the data. At the moment, the only permitted value is `"naturalearth"`,
#' which is the default if `server` is not supplied.
#'
#' @template debugTemplate
#'
#' @return A character value indicating the filename of the result; if
#' there is a problem of any kind, the result will be the empty
#' string.
#'
#' @seealso The work is done with [utils::download.file()].
#'
#' @examples
#'\dontrun{
#' library(oce)
#' # User must create directory ~/data/coastline first.
#' # As of September 2016, the downloaded file, named
#' # "ne_50m_coastline.zip", occupies 443K bytes.
#' filename <- dod.coastline(destdir="~/data/coastline")
#' coastline <- read.coastline(filename)
#' plot(coastline)
#'}
#'
#' @references
#' 1. The NaturalEarth server is at `https://www.naturalearthdata.com`
#'
#' @family functions that download files
#' @family things related to coastline data
#'
#' @export
#'
#' @author Dan Kelley
dod.coastline <- function(resolution, item="coastline", destdir=".", destfile, server="naturalearth", debug=0)
{
    if (missing(resolution))
        resolution <- "50m"
    resolutionChoices <- c("10m", "50m", "110m")
    if (!(resolution %in% resolutionChoices))
        stop("'resolution' must be one of: '", paste(resolutionChoices, collapse="' '"), "'")
    if (server == "naturalearth") {
        urlBase <- "https://www.naturalearthdata.com/downloads"
    } else {
        stop("the only server that works is naturalearth")
    }
    filename <- paste("ne_", resolution, "_", item, ".zip", sep="")
    if (missing(destfile))
        destfile <- filename
    url <- paste(urlBase, "/", resolution, "/physical/", filename, sep="")
    destination <- paste(destdir, destfile, sep="/")
    if (1 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
        dodDebug(debug, "Not downloading", destfile, "because it is already present in", destdir, "\n")
    } else {
        download.file(url, destination)
        dodDebug(debug, "Downloaded file stored as '", destination, "'\n", sep="")
    }
    # The following is a sample URL, from which I reverse-engineered the URL construction.
    #    https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_lakes.zip
    destination
}
