context("get_platform")

source("../../setGetter.R")

test_that("Get Platform works", {
	plat <- get_platform()
	expect_true(nrow(plat)>30)
	expect_equal(ncol(plat), 3)
})