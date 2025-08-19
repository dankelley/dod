library(dod)

test_that("monthNameToNumber", {
    expect_equal(3L, monthNumberFromName(3))
    expect_equal(3L, monthNumberFromName(3.1))
    expect_equal(3L, monthNumberFromName("mar"))
    expect_true(is.na(monthNumberFromName("ma"))) # need 1 more letter
})
