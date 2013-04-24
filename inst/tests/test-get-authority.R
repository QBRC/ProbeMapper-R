context("get_authority")

source("../../setGetter.R")

test_that("Get Authority works", {
	auth <- get_authority()
	expect_true(nrow(auth)>2)
	expect_equal(ncol(auth), 2)
})