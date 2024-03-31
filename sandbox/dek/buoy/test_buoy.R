library(dod)
for (loc in c("h1", "halifax", "hkb", "saint_john", "saint_johns")) {
    cat("location: \"", loc, "\"\n", sep = "")
    dod.buoy("smartatlantic", loc, debug = 2)
}
