context("get_probe")

source("setGetter.R")

test_that("Get Single Probe ID Works", {
	pr <- get_probe(1000001)
	expect_equal(nrow(pr), 1)
	expect_equal(ncol(pr), 3)	
})

test_that("Get Multiple Probe IDs Works", {
	pr <- get_probe(1000001:1000009)
	expect_equal(nrow(pr), 9)
	expect_equal(ncol(pr), 3)	
})

test_that("Get Single Probe Name Works", {
	pr <- get_probe(platform=1, probe.name="1007_s_at")
	expect_equal(nrow(pr), 1)
	expect_equal(ncol(pr), 3)	
})

test_that("Get Multiple Probe Name Works", {
	pr <- get_probe(platform=1, probe.name=c("1007_s_at", "121_at"))
	expect_equal(nrow(pr), 2)
	expect_equal(ncol(pr), 3)	
})

test_that("Invalid params errors", {
	expect_error(get_probe(platform=1, probe.name=c("1007_s_at", "121_at"), probe.id=100))
})

test_that("Get Invalid Probe Name Errors", {
	expect_error(pr <- get_probe(platform=0, probe.name="badProbe"))
})

test_that("Invalid Probe Errors", {
	expect_error(pr <- get_probe(-1))
})