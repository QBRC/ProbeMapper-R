context("get_etp")

source("setGetter.R")

test_that("Get ETP by probe works", {
	etp <- get_etp(probe = 1000001)
	expect_equal(nrow(etp), 3)
	expect_equal(ncol(etp), 4)
})

test_that("Get ETP by entrez works", {
	etp <- get_etp(entrez=780)
	expect_true(nrow(etp) > 50)
	expect_equal(unique(etp$entrezId), 780)
	expect_equal(ncol(etp), 4)
})

test_that("Auth filter works", {
	etp <- get_etp(entrez=780, auth=1)
	expect_true(nrow(etp) > 25)
	expect_equal(unique(etp$entrezId), 780)
	expect_equal(unique(as.integer(etp$authorityId)), 1)
	expect_equal(ncol(etp), 4)
})

test_that("Invalid params errors", {
	expect_error(get_etp(entrez=780, probe=1000001))
})