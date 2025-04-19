library(oce)
url <- "https://cioosatlantic.ca/erddap/files/bio_atlantic_zone_monitoring_program_ctd/Bedford%20Basin%20Monitoring%20Program/2024/CTD_BCD2024667_001_1_DN.ODF.nc"
file <- gsub(".*/", "", url)
if (!file.exists(file)) {
    download.file(url, file)
}
d <- read.netcdf(file) |> rename() |> as.ctd()
summary(d)
plot(d)
