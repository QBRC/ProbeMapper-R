#' Initialize ProbeMapper
#' 
#' Sets up the default environment for ProbeMapper URLs.
#' @importFrom RODProt read_data_package
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
.onLoad <- function(libname, pkgname){
	
	#maybe not the best way to accomplish this, but it's the only way to get
	# around the warnings in R CMD CHECK. Not just a NOTE
	# see http://r.789695.n4.nabble.com/onLoad-failing-because-could-not-find-function-quot-loadMethod-quot-td3906638.html
	library(stats)
	library(utils)
	library(methods)
	
	pmDataPackage <- read_data_package("http://qbridev.swmed.edu:8080/probemapper/datapackage.json")
	options(pmDP = pmDataPackage)
}