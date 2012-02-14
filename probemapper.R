


globalHeader <- c("Accept"="text/xml", "Accept"="multipart/*", "Content-Type"="text/xml; charset=utf-8")



#' Correct Nil SOAP values
#' 
#' Parser to correct nil arguments in the returned XML. We want them to be NAs.
#' 
#' @param x the parameter to check for nil status
#' @return x, if the parameter is not nill, NA if it was.
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
correctNil <- function(x){	
	if (length(names(x)) && names(x) == "nil" && x == "true"){
		return(NA);
	}	
	return(x);
}


#' Formats an input string -- which is assumed to be NA, by default -- as a SOAP comopatible string
#' 
#' @param par the parameter to format
#' @param makeList whether or not to make the output into a list
#' @return the formatted parameter
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
formatSOAPParameter <- function(par, makeList=TRUE){
	if (length(par)==1 && is.na(par)){
		par <- ""
		return(par)
	}
	
	if(makeList){
		if (par != "" && typeof(par) != "list"){
			vector <- par
			par <- list();
			for (e in vector){
				par <- c(par, list(ID=e))
			}
		}
	}
	return(par)
}



#' Custom SOAP Response Handler.
#' 
#' This is a custom handler for the SOAP response. Similar to the default, but doesn't assume nesting the way the default does
#' 
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
newHand <- list(action = function (action, method, server, xmlns, sep = "#") 
{
	if (missing(sep)) {
		if (substring(action, nchar(action)) == "#" || substring(method, 
																														 1, 1) == "#") 
			sep = ""
	}
	if (action == "") 
		action = xmlns
	paste(action, sep, method, sep = "")
}, result = function (xmlSource, header, method, server) 
{
	response <- parseSOAP(xmlSource, asText = TRUE)
	fromXML(response)
})
environment(newHand$action) <- environment(.SOAP)
environment(newHand$result) <- environment(.SOAP)



#' Converts the given list to a character representation of XML with the root node named as provided.
#' This function will create a tree using the names given to each list with names and lists given in the leaf nodes
#' 
#' @param rootName the name of the root node for your tree
#' @param data the list to be converted to XML
#' @return a character representation of the XML tree created
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
listToXML <- function(rootName="root", data=list()){
	foo <- function(node, sublist){
	    for(i in 1:length(sublist)){
	        child <- newXMLNode(names(sublist)[i], parent=node);
	
	        if (typeof(sublist[[i]]) == "list"){
	            foo(child, sublist[[i]])
	        }
	        else{
	            xmlValue(child) <- sublist[[i]]
	        }
	    } 
	}
	root <- newXMLNode(rootName)
	foo(root, data)
	saveXML(root, indent=FALSE)
}


pmcache <- list()

#' Takes a data.frame of probes and looks up the IDs for each probe describes, returning a data.frame which is annotated with the probe ID.
#' The order of the rows in the data.frame is NOT maintained.
#'
#' @param server the SOAPServer object describing the location of the server to use
#FIXME: @param probes a data.frame containing the probe names and platform IDs of the probes of which you want to retrieve the IDs. The column names must be exactly ("Name" and "platform") in either order.
#' @return a data.frame with the same data that was put in (possibly in a different order) annotated with the ProbeID as the rowname.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
pm.lookupProbe <- function (server, probeName, platform, probeID){
		
	if (missing(probeID)){
		if (missing(probeName) || missing(platform)){
			stop("Must specify either the probeID(s) OR (the probeNames + platforms)")			
		}	
		if (length(platform) != 1 && length(platform) != length(probeName)){
			stop("Lengths of platform and probeName must match.")
		}
		if (length(platform) == 1){
			platform <- rep(platform, length(probeName))
		}		
		mode <- "Name"		
		probeCount <- length(probeName)
	}	
	else{
		if (!missing(probeName) || !missing(platform)){
			warning("probeID and probeName or platform was specified -- ignoring the platform and probeName and using only the probeIDs")
		}				
		mode <- "ID"
		probeCount <- length(probeID)
	}
	
	
	if (mode == "Name"){
		thisAction = "http://qbri.swmed.edu/ProbeMapper/LookupProbe"
		method <- "LookupProbe"
		
	}
	else{
		thisAction = "http://qbri.swmed.edu/ProbeMapper/GetProbes"
		method <- "GetProbes"
	}
	
	action = paste("\"",thisAction,"#",method,"\"", sep="");
	header = c(globalHeader, "SOAPAction"=action)
	.opts = list(url=SSOAP:::toURL(server),
						 httpheader = header)
	
	maxPerRequest <- 5000;
	pointer <- 1;
	
	toReturn <- NULL;
	
	while (pointer <= probeCount){
		
		if (mode == "ID"){
			vals <- probeID[pointer:min(pointer+maxPerRequest, length(probeID))]
			vals <- as.list(vals)
			names(vals) <- rep("ID", length(vals))			
			post <- listToXML("probeID", vals)
			
		}
		else{
			vals <- data.frame(probeName=probeName[pointer:min(pointer+maxPerRequest, length(probeName))], platforms=platforms[pointer:min(pointer+maxPerRequest, length(platforms))])			
			innerPost <- paste(apply(vals, 1, function(li) { names(li) <- c("Name", "platform"); listToXML("ProbeInfo",li) }), collapse="")
			post <- paste("<ProbeInfo>",innerPost,"</ProbeInfo>", sep="");			
		}
		
		post <- paste("<",method,">",post,"</",method,">", sep="");
		
		
		soap <- customSOAP(server, post, method,
	        action = thisAction, xmlns = "http://qbri.swmed.edu/LungCancer/", 
	        .literal = FALSE, nameSpaces = "1.2", handlers=newHand,
					.elementFormQualified = TRUE)
		
		soap <- lapply(soap, function(x) {lapply(x, correctNil)})
		
		if (length(soap)){
			soap <- data.frame(do.call(rbind, soap), row.names=NULL)			
		}	
			
		if (is.null(toReturn)){
			toReturn <- soap;
		}
		else{			
			toReturn <- rbind(toReturn, soap);			
		}		
		
		pointer <- pointer + maxPerRequest +1;
	}	
	
	
	#TODO: implement caching -- could be temporary files, sqlite, or in-memory
	addToCache <- function(platformID, platform){
		if (is.null(pmcache$platform)){
			if(exists(pmcache$platform[platformID])){
				
			}
			else{
				pmcache$platform[platformID] <- list()
			}
		}
		else{
			pmcache$platform <- list();
		}
		
	}

	toReturn <- toReturn[!duplicated(toReturn),]
	return(toReturn);	
}

#' Get information about a gene
#' 
#' Retrieves information about the specified gene from the Entrez database.
#' @param server the SOAPServer object describing the location of the server to use
#' @param entrezID
#' @return Information about the selected genes from the Entrez Database.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
pm.getGene <- function(server, entrezID){
	if (missing(server)){
		stop("You must specify a SSOAP Server in order to make any request from the web service.")
	}
	
	if (length(entrezID) == 0){
		stop ("You must specify at least one entrezID of the gene in which you're interested")
	}
	
	soap <- .SOAP(server, "GetGenes", entrezID=entrezID,
								action = "http://qbri.swmed.edu/ProbeMapper/GetGenes", 
								handlers=newHand, 
								.literal = TRUE, nameSpaces = "1.2", 
								.elementFormQualified = TRUE)
	
	soap <- lapply(soap, function(x) {lapply(x, correctNil)})
	
	if (length(soap)){
		soap <- data.frame(do.call(rbind, soap), row.names=1)
		return(soap)
	}
}

#' Get the genes associated with specified probes
#' 
#' Finds any gene associated with this probe according to any authority in the database. Note that you must specify either probeID and/or (probeName and platformID).
#' @param server the SOAPServer object describing the location of the server to use
#' @param probeID
#' @param probeName
#' @param platformID
#' @return a data.frame containing all of the gene associations for this probe and according to what authority.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
pm.getGenesByProbe <- function(server, probeID, probeName, platformID){
	#if only a single platform is specified, then repeat it for each probe.
	if (length(platformID == 1)){
		platformID <- rep(platformID, length(probeName))
	}
	
	if (length(probeName) != length(platformID)){
		stop("probeName and platformID have different lengths. You must ensure that each probe given has an associated platform ID.")
	}
	
	#First convert to probeID
	#TODO: do this server-side
	#FIXME: do this conversion
	
	
	
	probeID <- formatSOAPParameter(probeID)
		
	thisAction = "http://qbri.swmed.edu/ProbeMapper/GetGenesForProbe"
	method <- "GetGenesForProbe"
	
	action = paste("\"",thisAction,"#",method,"\"", sep="");
	header = c(lcdb:::globalHeader, "SOAPAction"=action)
	.opts = list(url=SSOAP:::toURL(server),
							 httpheader = header)
	
	post <- listToXML(method, list(probeID=probeID));
	
	soap <- customSOAP(server, post, method,
										 action = thisAction, xmlns = "http://qbri.swmed.edu/ProbeMapper/", 
										 .literal = FALSE, nameSpaces = "1.2", handlers=newHand,
										 .elementFormQualified = TRUE)
	
	soap <- lapply(soap, function(x) {lapply(x, correctNil)})
	if (length(soap)){
		opt <- options("warn")
		options("warn"=-1)
		soap <- data.frame(do.call(rbind, soap))
		options(opt)
		return(soap)
	}
	
	return(NULL)
	
	
	
}

#' Get the probes associated with specified genes
#' 
#' Finds any probes associated with this gene according to any authority in the database.
#' @param server the SOAPServer object describing the location of the server to use
#' @param entrezID
#' @return A data.frame containing all of the probes associations for this gene and according to which authority.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
pm.getProbesByGene <- function(server, entrezID){
	if (missing(server)){
		stop("You must specify a SSOAP Server in order to make any request from the web service.")
	}
	
	if (missing(entrezID)){
		stop("You must specify an Entrez ID");
	}

	entrezID <- formatSOAPParameter(entrezID)
	
	thisAction = "http://qbri.swmed.edu/ProbeMapper/GetProbesForGene"
	method <- "GetProbesForGene"
	
	action = paste("\"",thisAction,"#",method,"\"", sep="");
	header = c(lcdb:::globalHeader, "SOAPAction"=action)
	.opts = list(url=SSOAP:::toURL(server),
							 httpheader = header)
	
	post <- listToXML(method, list(entrezID=entrezID));
	
	soap <- customSOAP(server, post, method,
										 action = thisAction, xmlns = "http://qbri.swmed.edu/ProbeMapper/", 
										 .literal = FALSE, nameSpaces = "1.2", handlers=newHand,
										 .elementFormQualified = TRUE)
	
	soap <- lapply(soap, function(x) {lapply(x, correctNil)})
	if (length(soap)){
		opt <- options("warn")
		options("warn"=-1)
		soap <- data.frame(do.call(rbind, soap))
		options(opt)
		return(soap)
	}
	
	return(NULL)	
}

#' Get the available platforms
#' 
#' Gets a list of the available platforms currently contained in the database and available to you.
#' @param server the SOAPServer object describing the location of the server to use
#' @return a data.frame containing the platforms contained in the database and available to you.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
pm.getPlatforms <- function(server){
	if (missing(server)){
		stop("You must specify a SSOAP Server in order to make any request from the web service.")
	}
	
	thisAction = "http://qbri.swmed.edu/ProbeMapper/GetPlatforms"
	method <- "GetPlatforms"
	
	action = paste("\"",thisAction,"#",method,"\"", sep="");
	header = c(lcdb:::globalHeader, "SOAPAction"=action)
	.opts = list(url=SSOAP:::toURL(server),
							 httpheader = header)
	
	post <- paste("<",method," />", sep="");
	
	soap <- customSOAP(server, post, method,
										 action = thisAction, xmlns = "http://qbri.swmed.edu/ProbeMapper/", 
										 .literal = FALSE, nameSpaces = "1.2", handlers=newHand,
										 .elementFormQualified = TRUE)
	
	soap <- lapply(soap, function(x) {lapply(x, correctNil)})
	if (length(soap)){
		opt <- options("warn")
		options("warn"=-1)
		soap <- data.frame(do.call(rbind, soap))
		options(opt)
		return(soap)
	}
	
	return(NULL)
	
}


		
#' Clears the probemapper cache
#' 
#' Caching is not yet implemented in probemapper. Eventually, we plan to add client-side caching which will streamline the repetitive lookup of probes/genes to avoid the performance hit of having to make trips to the server for every probe/gene. 
#'
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
flushCache <- function(){
	pmcache <- list();
}

			
		 					
#' This is a modification of the .SOAP function available in the SSOAP package. At the time of writing, SSOAP couldn't properly handle the conversion of arguments into XML, so this function accepts a custom "body" element and statically defines much of the SOAP envelope, itself. This is less flexible, but gets the job done until the .SOAP function can be fixed to meet our needs.
#' 
#' @param server the SOAPServer object describing the location of the server to use
#' @param body anything inside the body of the SOAP Request envelope
	#' @return a list representing the SOAP values returned
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
customSOAP <- function (server, body, method, ..., .soapArgs = list(), action, nameSpaces = SOAPNameSpaces(), 
   	xmlns = NULL, handlers = SOAPHandlers(), .types = NULL, .convert = TRUE, 
    .opts = list(), curlHandle = getCurlHandle(), .header = getSOAPRequestHeader(action, 
        .server = server), .literal = FALSE, .soapHeader = NULL, 
    .elementFormQualified = FALSE, .returnNodeName = NA) 
{
    
		heading <- "<?xml version=\"1.0\"?> <SOAP-ENV:Envelope xmlns:SOAP-ENC=\"http://schemas.xmlsoap.org/soap/encoding/\" xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" SOAP-ENV:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"> <SOAP-ENV:Body>"
		footing <- "</SOAP-ENV:Body> </SOAP-ENV:Envelope>"										
												
		if (is.null(xmlns)) 
        xmlns <- c(namesp1 = action)
    if (!is(action, "AsIs")) {
        if ((!is(action, c("SOAPAction")) || action == "") && 
            !is.null(handlers) && "action" %in% names(handlers)) 
            action <- handlers[["action"]](action, method, server, 
                xmlns)
    }
    useNodes = TRUE
    txt <- paste(heading,body,footing,sep="");
    headerDataFun = basicTextGatherer()
    bodyDataFun = basicTextGatherer()
    if (is(server, "CURLHandle") && missing(curlHandle)) 
        curlHandle = server
    else .opts$url = toURL(server)
    if (length(.header)) 
        .opts$httpheader = .header
    curlPerform(postfields = txt, writeFunction = bodyDataFun$update, 
        headerFunction = headerDataFun$update, .opts = .opts, 
        curl = curlHandle)
    content = structure(list(header = RCurl:::parseHTTPHeader(headerDataFun$value(NULL)), 
        content = bodyDataFun$value()), class = "SOAPHTTPReply")
    if (isHTTPError(content$header) || isSOAPBodyError(content$content)) {
        fault <- SOAPFault(parseSOAP(content$content, asText = TRUE))
        e = simpleError(paste("Error occurred in the HTTP request: ", 
            fault@message, xmlValue(fault@detail)))
        httpError = RCurl:::getHTTPErrorClass(content$header[["status"]])
        class(e) = c("SOAPError", httpError, class(e))
        stop(e)
        return(fault)
    }
    if (is.logical(.convert) && .convert && !is.null(handlers) && 
        !is.na(match("result", names(handlers)))) 
        handlers[["result"]](content$content, content$header, 
            method)
    else if (is.function(.convert)) 
        return(if (inherits(.convert, "RawSOAPConverter")) .convert(content) else .convert(getReturnNode(content)))
    else if (is(.convert, "GenericSchemaType")) 
        convertFromSOAP(SOAPResult(content$content, content$header), 
            .convert, nodeName = .returnNodeName)
    else if (is.character(.convert)) 
        as(getReturnNode(content), .convert)
    else return(content)
}		
environment(customSOAP) <- environment(.SOAP)


 