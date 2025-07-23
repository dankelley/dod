months <- c(
    "january", "february", "march", "april", "may", "june",
    "july", "august", "september", "october", "november",
    "december"
)
m <- "Ja"
i <- pmatch(tolower(m), months)
if (is.na(i)) stop("No match for month name \"", m, "\"")
months[i]
