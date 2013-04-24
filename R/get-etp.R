#' Get Entrez-To-Probe Mappings
#' 
#' Retrieves the specified Entrez-To-Probe mappings from ProbeMapper
#' @param probe The ID(s) of the probe(s) for which to retrieve ETP mappings.
#' @param entrez The Entrez ID(s) of the gene(s) for which to retrieve ETP 
#'   mappings.
#' @param auth The IDs of the authorities to include in the filtered list.
#' @param getter The Getter to use when retrieving the data. You'll need to
#'   configure and use an HMACGetter in order to securely make most requests.
#' @param pmDataPackage The Data Package to use in making the request.
#' @importFrom RODProt read_json_table
#' @importFrom httr build_url
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
get_etp <- function(probe, entrez, auth, getter=getOption("pmGetter"), pmDataPackage=getOption("pmDP")){
	url <- pmDataPackage$url
	
	if (!missing(probe) && !missing(entrez)){
		stop("We don't currently support specifying both the probe and entrez parameters in a single request. Please just use one or the other.")
	}
	
	if (!missing(probe)){
		url$path <- paste(url$path, "/etp/probe/", sep="")
		url$query <- list(id=paste(probe, collapse=","))
	}
	
	if (!missing(entrez)){
		url$path <- paste(url$path, "/etp/entrez/", sep="")
		url$query <- list(id=paste(entrez, collapse=","))
	}
	
	if (!missing(auth)){
		url$query$auth <- paste(auth, collapse=",")
	}
	
	url <- build_url(url)
	read_json_table(url, getter=getter)
}