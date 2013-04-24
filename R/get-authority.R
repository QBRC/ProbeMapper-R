#' Get All ProbeMapper Authorities
#' 
#' Retrieves a list of all the authorities used in the ProbeMapper system.
#' @param getter The Getter to use when retrieving the data. You'll need to
#' configure and use an HMACGetter in order to securely make most requests.
#' @param pmDataPackage The Data Package to use in making the request.
#' @importFrom RODProt get_resource
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
get_authority <- function(getter=getOption("pmGetter"), pmDataPackage=getOption("pmDP")){	
	get_resource(pmDataPackage, "Authority", getter=getter)
}