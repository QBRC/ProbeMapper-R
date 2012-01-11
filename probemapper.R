


globalHeader <- c("Accept"="text/xml", "Accept"="multipart/*", "Content-Type"="text/xml; charset=utf-8")


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

#' Custom handler for the SOAP response. Similar to the default, but doesn't assume nesting the way the default does
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
#' @param probes a data.frame containing the probe names and platform IDs of the probes of which you want to retrieve the IDs. The column names must be exactly "Name" and "platform" in either order.
#' @return a data.frame with the same data that was put in (possibly in a different order) annotated with the ProbeID as the rowname.
#' @export
#' @author Jeffrey D. Allen \email{Jeffrey.Allen@@UTSouthwestern.edu}
pm.lookupProbe <- function (server, probes){
	probes <- probes[!duplicated(probes),]
	
	if (length(probes) == 1 && is.na(probes)){
		stop("You must provide a data frame in order to lookup any probes. See the documentation for this function for more details.")
	}
	
	if (!all(colnames(probes) %in% c("platform", "Name"))){
		stop("The columns of the data.frame must be named exactly 'Name', and 'platform' (in either order).")
	}
	
	thisAction = "http://qbri.swmed.edu/ProbeMapper/LookupProbe"
	method <- "LookupProbe"
	
	action = paste("\"",thisAction,"#",method,"\"", sep="");
	header = c(globalHeader, "SOAPAction"=action)
	.opts = list(url=SSOAP:::toURL(server),
						 httpheader = header)
	
	nameOrder <- colnames(probes)

	maxPerRequest <- 5000;
	pointer <- 1;
	
	toReturn <- NULL;
	
	while (pointer <= nrow(probes)){
		vals <- probes[pointer:min(pointer+maxPerRequest, nrow(probes)),]
					
		innerPost <- paste(apply(vals, 1, function(li) { names(li) <- nameOrder; listToXML("ProbeInfo",li) }), collapse="")
										 
	  post <- paste("<ProbeInfo>",innerPost,"</ProbeInfo>", sep="");
		post <- paste("<",method,">",post,"</",method,">", sep="");
		
		soap <- customSOAP(server, post, method,
	        action = thisAction, xmlns = "http://qbri.swmed.edu/LungCancer/", 
	        .literal = FALSE, nameSpaces = "1.2", handlers=newHand,
					.elementFormQualified = TRUE)
		
		soap[[1]][".attrs"] <- NULL
  
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


 