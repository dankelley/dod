library(dod)

test_that("ctd downloads", {
    tempDir <- tempfile()
    dir.create(tempDir)
    expect_error(dod.ctd("invalid"), "'program' must be")
    expect_error(dod.ctd("BBMP"), "ID must be supplied")
    expect_silent(dod.ctd(program = "BBMP", year = "2024", ID = "001_1", destdir = tempDir, quiet = TRUE))
    expect_error(dod.ctd("GTSPP"), "must provide basin")
    expect_error(dod.ctd("GTSPP", basin = "error"), "'arg' should be one of")
    #expect_error(dod.ctd("ITP"), "ID must be supplied")
    #expect_error(dod.ctd("ITP", "junk"), "ID=junk is neither an integer nor a string denoting an integer")
    unlink(tempDir, recursive = TRUE)
})
