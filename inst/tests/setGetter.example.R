#' This file is used in testing to configure the getter to use for the tests. 
#' A variable "getter" must be created which will allow the tests against the 
#' web service to be executed. For privacy reasons, the key used for internal 
#' testing is hidden and this simple file is provided instead.
#' 
#' Steps to run tests successfully:
#'   - Register for a ProbeMapper account to get an ID and a key
#'   - copy this file to `setGetter.R` in this directory
#'   - fill out the id and key below
#' You should then be able to execute the tests successfully.

getter <- HMACGetter$new(id="some user", key="<user key here>")
options(pmGetter = getter)