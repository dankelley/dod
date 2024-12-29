#' Debug dod package functions
#'
#' This function is used throughout the `dod` package to provide
#' information on how processing is being done.
#'
#' @param debug integer valaue indicating level of debugging. If
#' this exceeds zero, then `...` is passed to [cat()], for printing.
#'
#' @param ... content passed to [cat()].
#'
#' @param unindent integer, for the left-indent level. Set this to X
#' to indent 2 space to the left.
#'
#' @param sep separation character, passed to [cat()] in displaying debugging
#' messages.
#'
#' @export
dodDebug <- function(debug = 0, ..., unindent = 0, sep = "") {
    debug <- if (debug > 4) 4 else max(0, floor(debug + 0.5))
    unindent <- max(0, min(3, unindent))
    if (debug > 0) {
        n <- 3 - debug - unindent
        if (n > 0) {
            cat(paste(rep("  ", n), collapse = "", sep = ""))
        }
        cat(..., sep = sep)
    }
}
