#' @param age a numerical value indicating a time interval, in days.
#' If the file to be downloaded from the server already exists
#' locally, and was created less than `age` days in the past, it will
#' not be downloaded again.  Setting `age=0` forces a download, so
#' that existing files will always be updated. By contrast, setting
#' `age` to a negative number prevents the updating of files that
#' already exist locally, regardless of their age.
