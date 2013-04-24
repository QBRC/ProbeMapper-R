#' Get ProbeMapper Probes
#' 
#' Retrieves the specified probes from the ProbeMapper system.
#' @param probe.id The ID(s) of the probe(s) to retrieve.
#' @param platform The platform(s) to include in the returned results
#' @param probe.name The name(s) of the probe(s) to include in the returned 
#'   results.
#' @param getter The Getter to use when retrieving the data. You'll need to
#' configure and use an HMACGetter in order to securely make most requests.
#' @param pmDataPackage The Data Package to use in making the request.
#' @details You may either specify the probe.id, or (the platform and the
#' probe name).
#' @importFrom RODProt HTTPGetter
#' @importFrom RODProt read_json_table
#' @importFrom httr build_url
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
get_probe <- function(probe.id, platform, probe.name, 
											getter=getOption("pmGetter"), 
											pmDataPackage=getOption("pmDP")){
	
	if (is.null(getter)){
		warning("No getter provided, defaulting to HTTP.")
		getter <- HTTPGetter$new()
	}
	
	if (is.null(pmDataPackage)){
		stop("No Data Package provided -- cannot proceed.")
	}
	
	url <- pmDataPackage$url
	url$path <- paste(url$path, "/probe/", sep="")
	
	if (!missing(probe.id)){
		if (!missing(platform) || !missing(probe.name)){
			stop("If the probe.id is specified, the platform and probe.name should be empty.")
		}
		
		url$query <- list(id=paste(probe.id, collapse=","))		
	} else if (!missing(platform) && !missing(probe.name)){
		if (!missing(probe.id)){
			stop("If the platform and probe.name are specified, the probe.id should be empty.")
		}
		url$query <- list(plat=platform, 
											name=paste(probe.name, collapse=","))
	} else {
		stop("Not enough information provided. See the details of the documentation to find which parameters must be specified.")
	}
	
	url <- build_url(url)
	
	read_json_table(url, getter=getter)
}