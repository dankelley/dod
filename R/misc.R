#' Convert month name to number, returning NA if no match
#'
#' @param name character value naming a month, with enough
#' letters to distinguish it from other months.
#'
#' @return integer giving month number, e.g. "Jan" yields 1.
#'
#' @export
#'
#' @author Dan Kelley
monthNumberFromName <- function(name) {
    months <- c(
        "january", "february", "march", "april", "may", "june",
        "july", "august", "september", "october", "november",
        "december"
    )
    pmatch(tolower(name), months)
}
