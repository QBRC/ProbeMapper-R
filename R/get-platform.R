#' Get All ProbeMapper Platforms
#' 
#' Retrieves a list of all the platforms contained in the ProbeMapper system.
#' @param getter The Getter to use when retrieving the data. You'll need to
#' configure and use an HMACGetter in order to securely access most platforms.
#' @param pmDataPackage The Data Package to use in retrieving the platforms.
#' @importFrom RODProt get_resource
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
get_platform <- function(getter=getOption("pmGetter"), pmDataPackage=getOption("pmDP")){
	get_resource(pmDataPackage, "Platform", getter=getter)
}