readIndex <- function(year = "2024") {
    base <- paste0(
        "https://cioosatlantic.ca/erddap/files/",
        "bio_atlantic_zone_monitoring_program_ctd/",
        "Bedford%20Basin%20Monitoring%20Program/"
    )
    url <- paste0(base, year, "/")
    l <- readLines(url) # takes 0.01s user, 0.002s system, but 1.2s elapsed
    l <- l[grep("CTD.*BCD", l)]
    l <- gsub("\".*", "", gsub("^.*href=\"", "", l))
    l <- gsub("&#x2e;", ".", gsub("&#x5f;", "_", l))
    id <- gsub("CTD_[^_]*_([^_]*).*", "\\1", l)
    direction <- gsub(".*_(DN|UP).*", "\\1", l)
    data.frame(id = id, direction = direction)
}
index <- readIndex()
str(index)
