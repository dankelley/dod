#' Download and Cache a topo File
#'
#' Topographic data are downloaded from a data server that holds the
#' ETOPO1 dataset (Amante, C. and B.W. Eakins, 2009), and saved as a
#' netCDF file whose name specifies the data request, if a file of
#' that name is not already present on the local file system.  The
#' return value is the name of the data file, and its typical use is
#' as the filename for a call to `read.topo()` in the `oce` package.
#' Given the rules on
#' file naming, subsequent calls to [dod.topo()] with identical
#' parameters will simply return the name of the cached file, assuming
#' the user has not deleted it in the meantime.  Note that
#' [dod.topo()] uses the `"terra"` and `"ncdf4"` packages,
#' so an error is reported if they are not available.
#'
#' The specified longitude and latitude limits are rounded to 2 digits
#' (corresponding to a footprint of approximately 1km), and these are
#' used in the server request. If the resultant request would generate
#' under 1 row or column in the result, [dod.topo()] generates an
#' error message and stops.
#'
#' @section Historical note relating to NOAA server changes:
#'
#' 2022 November 13: updated to new NOAA database, with 1/4-minute resolution (a
#' marked improvement over the previous 1-minute resolution).  The revision was
#' framed along similar changes to `marmap::getNOAAbathy()` made earlier today.
#' Thanks to Clark Richards for pointing this out!
#'
#' 2020 May 31: updated for a change in the NOAA query structure, taking
#' hints from `marmap::getNOAAbathy()`.
#'
#' @param west,east numeric values for the limits of the data-selection box, in degrees.
#' These are converted to the -180 to 180 degree notation, if needed.
#' Then, `west` is rounded down to the nearest 1/100th degree, and `east`
#' is rounded up to the the nearest 1/100th degree. The results of these
#' operations are used in constructing the query for the NOAA data server.
#'
#' @param south,north latitude limits, treated in a way that
#' corresponds to the longitude limits.
#'
#' @param resolution numeric value of grid spacing, in geographical minutes.
#' The default value is 4 minutes, corresponding to 4 nautical miles (approx. 7.4km)
#' in the north-south direction, and less in the east-west direction.
#'
#' @template destdirTemplate
#'
#' @param destfile optional name of destination file. If not provided,
#' this function creates a reasonable name.
#'
#' @param server character value specifying the base from which a
#' download URL will be constructed.  It is unlikely that any value
#' other than the default will work, unless it is a similarly-constructed
#' mirrored site.
#'
#' @template debugTemplate
#'
#'
#' @examples
#' # Download and plot topographic data
#' if (interactive()) { # sidestep a pkgdown::build_site() error
#'     library(oce)
#'     destdir <- tempdir()
#'     topoFile <- dod.topo(
#'         west = -66, east = -60, south = 43, north = 47,
#'         resolution = 1, destdir = destdir
#'     )
#'     topo <- read.topo(topoFile)
#'     imagep(topo, zlim = c(-400, 400), col = oceColorsTwo, drawTriangles = TRUE)
#'     if (requireNamespace("ocedata", quietly = TRUE)) {
#'         data(coastlineWorldFine, package = "ocedata")
#'         lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
#'     }
#'     unlink(destdir, recursive = TRUE)
#' }
#'
#' @references
#' * Amante, C. and B.W. Eakins, 2009. ETOPO1 1 Arc-Minute Global Relief
#' Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum
#' NESDIS NGDC-24. National Geophysical Data Center, NOAA. doi:10.7289/V5C8276M
#'
## @importFrom terra as.matrix flip rast xmin xmax ymin ymax
#'
#' @return String indicating the full pathname to the downloaded file.
#'
#' @export
#' @family functions that download files
#' @author Dan Kelley
dod.topo <- function(
    west, east, south, north, resolution = 4,
    destdir = ".", destfile, server = "https://gis.ngdc.noaa.gov",
    debug = getOption("dodDebug", 0L)) {
    dodDebug(
        debug, "dod.topo(west=", west,
        ", east=", east,
        ", south=", south,
        ", north=", north,
        ", resolution=", resolution,
        ", destdir='", destdir, "'",
        ", server='", server, "') ...\n"
    )
    # Code derived from marmap:getNOAAbathy() {
    if (resolution < 0.5) {
        resolution <- 0.25
    } else if (resolution < 1.0) {
        resolution <- 0.50
    }
    database <- if (resolution == 0.25) {
        "27ETOPO_2022_v1_15s_bed_elev"
    } else if (resolution == 0.50) {
        "27ETOPO_2022_v1_30s_bed"
    } else {
        "27ETOPO_2022_v1_60s_bed"
    }
    # } end of marmap-derived code
    dodDebug(debug, "resolution set to ", resolution, " for web query\n")
    dodDebug(debug, "database set to '", database, "' for web query\n")
    # The +-0.005 is to get rounding down for west and south, and rounding up for east and north.
    east <- round(east + 0.005, 2)
    west <- round(west - 0.005, 2)
    south <- round(south - 0.005, 2)
    north <- round(north + 0.005, 2)
    if (west > 180) {
        west <- west - 360
    }
    if (east > 180) {
        east <- east - 360
    }
    wName <- paste(abs(west), if (west <= 0) "W" else "E", sep = "")
    eName <- paste(abs(east), if (east <= 0) "W" else "E", sep = "")
    sName <- paste(abs(south), if (south <= 0) "S" else "N", sep = "")
    nName <- paste(abs(north), if (north <= 0) "S" else "N", sep = "")
    resolutionName <- paste(resolution, "min", sep = "")
    if (missing(destfile)) {
        destfile <- paste0(paste("topo", wName, eName, sName, nName, resolutionName, sep = "_"), ".nc")
    }
    destination <- paste0(destdir, "/", destfile)
    dodDebug(debug, "destination='", destination, "'\n", sep = "")
    if (file.exists(destination)) {
        dodDebug(debug, "using existing file \"", destination, "\"\n", sep = "")
        dodDebug(debug, "} # dod.topo\n", sep = "", style = "bold", unindent = 1)
        return(destination)
    }
    nlon <- as.integer((east - west) * 60.0 / resolution)
    if (nlon < 1L) {
        stop("Cannot download topo file, since east-west (=", east - west, " deg) is less than resolution (=", resolution, " min)")
    }
    nlat <- as.integer((north - south) * 60.0 / resolution)
    if (nlat < 1L) {
        stop("Cannot download topo file, since north-south(=", north - south, " deg) is less than resolution (=", resolution, " min)")
    }
    urlOLD <- paste0(
        server, "/arcgis/rest/services/DEM_mosaics/ETOPO1_bedrock/ImageServer/exportImage",
        "?bbox=", west, ",", south, ",", east, ",", north,
        "&bboxSR=4326",
        "&size=", nlon, ",", nlat,
        "&imageSR=4326",
        "&format=tiff",
        "&pixelType=S16",
        "&interpolation=+RSP_NearestNeighbor",
        "&compression=LZW",
        "&f=image"
    )
    dodDebug(debug, "OLD url: \"", urlOLD, "\"\n", sep = "")
    # Test on 2022-11-13 with NOAA interface (Halifax Harbour region)
    # https://gis.ngdc.noaa.gov
    # /arcgis/rest/services/
    # DEM_mosaics/DEM_all/ImageServer/exportImage
    # ?bbox=-65.00000,44.00000,-63.00000,45.00000
    # &bboxSR=4326
    # &size=480,240
    # &imageSR=4326
    # &format=tiff
    # &pixelType=F32
    # &interpolation=+RSP_NearestNeighbor
    # &compression=LZ77
    # &renderingRule={%22rasterFunction%22:%22none%22}&mosaicRule={%22where%22:%22Name=%27ETOPO_2022_v1_15s_bed_elev%27%22}
    # &f=image
    url <- paste0(
        server, "/arcgis/rest/services/",
        "DEM_mosaics/DEM_all/ImageServer/exportImage",
        "?bbox=", west, ",", south, ",", east, ",", north,
        "&bboxSR=4326",
        "&size=", nlon, ",", nlat,
        "&imageSR=4326",
        "&format=tiff",
        "&pixelType=S32",
        "&interpolation=+RSP_NearestNeighbor",
        "&compression=LZ77",
        "renderingRule={%22rasterFunction%22:%22none%22}&mosaicRule={%22where%22:%22Name=%",
        database,
        "%27%22}",
        "&f=image"
    )
    dodDebug(debug, "querying \"", url, "\"\n", sep = "")
    if (!requireNamespace("terra", quietly = TRUE)) {
        stop("must install.packages(\"terra\") before using dod.topo()")
    }
    if (!requireNamespace("ncdf4", quietly = TRUE)) {
        stop("must install.packages(\"ncdf4\") before using dod.topo()")
    }
    r <- terra::rast(x = url)
    dodDebug(debug, "converting data\n", sep = "")
    longitude <- seq(terra::xmin(r), terra::xmax(r), length.out = ncol(r))
    latitude <- seq(terra::ymin(r), terra::ymax(r), length.out = nrow(r))
    z <- t(terra::as.matrix(terra::flip(r, direction = "vertical"), wide = TRUE))
    dodDebug(debug, "saving to \"", destination, "\"\n", sep = "")
    # create netcdf file
    # dimensions
    # side <- ncdf4::ncdim_def("side", units="", vals=2.0)
    fillvalue <- 1e32
    lonDim <- ncdf4::ncdim_def("lon", "degrees_east", as.double(longitude))
    latDim <- ncdf4::ncdim_def("lat", "degrees_north", as.double(latitude))
    Band1 <- ncdf4::ncvar_def("Band1", "m", list(lonDim, latDim), fillvalue, "elevation m", prec = "double")
    nc <- ncdf4::nc_create(destination, list(Band1))
    ncdf4::ncvar_put(nc, "Band1", z)
    ncdf4::nc_close(nc)
    dodDebug(debug, "... dod.topo()\n")
    destination
}
