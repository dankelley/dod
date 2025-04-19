# NOTES on river level data
#
# Station list
#
#  https://hpfx.collab.science.gc.ca/today/hydrometric/doc/hydrometric_StationList.csv
#
# Code for Sackville River (also, other codes for upstream of this location, I think.)
#
#  01EJ001,"SACKVILLE RIVER AT BEDFORD",44.731530,-63.660440,NS,UTC-04:00
#
# Daily data (last ~30 days)
#
#  https://hpfx.collab.science.gc.ca/today/hydrometric/csv/NS/daily/NS_01EJ001_daily_hydrometric.csv
#
# Hourly data (last ~2 days)
#
#  https://hpfx.collab.science.gc.ca/today/hydrometric/csv/NS/hourly/NS_01EJ001_hourly_hydrometric.csv

library(oce)

# Construct URL (maybe for inclusion in dod package)
stn <- "01EJ001"
region <- "NS"
# interval may be "hourly" or "daily"
interval <- "daily"
url <- paste0(
    "https://hpfx.collab.science.gc.ca/today/hydrometric/csv/",
    region,
    "/",
    interval,
    "/",
    region,
    "_",
    stn,
    "_",
    interval,
    "_hydrometric.csv"
)
file <- gsub(".*/", "", url)
if (!file.exists(file)) {
    download.file(url, file)
}
# Note: we don't use these column names, which require fiddling to get into simple forms
data <- read.csv(file, header = TRUE)
timeString <- gsub("^(.*)([+-][0-9][0-9]:[0-9][0-9])$", "\\1", data[, 2])
# Decode tz offset (maybe lubridate has a way to do this...)
tzString <- gsub("^(.*)([+-][0-9][0-9]:[0-9][0-9])$", "\\2", data[, 2])
stopifnot(all(tzString == tzString[1])) # don't permit daylight-saving shifts (unlikely)
tzSign <- ifelse(substr(tzString, 1, 1) == "-", -1, 1)
tmp <- strsplit(substr(tzString, 2, 6), ":")
hour <- as.numeric(sapply(tmp, \(x) x[1]))
minute <- as.numeric(sapply(tmp, \(x) x[2]))
offset <- -tzSign * (hour + minute / 60) * 3600
time <- as.POSIXct(timeString, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC") + offset
level <- data[, 3]
oce.plot.ts(time, level)
mtext(file, adj = 1, line = 0.2)
