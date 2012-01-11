
library(XML)
library(XMLSchema)
library(SSOAP)

pmcache <- list()


hg <- read.csv("/home/projects/ProbeAlignment/data/VendorData/HGU133A.csv")
ua <- data.frame(hg[,1], rep(1, nrow(hg)))
colnames(ua) <- c("Name", "platform");
uaa <- pm.lookupProbe(TestServer, ua[1:5,])
values <- data.frame(SampleID=20340, Value=rnorm(10)+10, ProbeName=ua[1:10,1], PlatformID=1)


PMServer <- SOAPServer("129.112.182.86", "ProbeMapperWS/services/ProbeMapperSOAP", 8181)
DevServer <- SOAPServer("qbridev.swmed.edu", "ProbeMapperWS/services/ProbeMapperSOAP", 8080)
ProdServer <- SOAPServer("qbri", "ProbeMapperWS/services/ProbeMapperSOAP", 8080)

#' Order is not maintained
pm.lookupProbe <- function (server, probes){
	probes <- probes[!duplicated(probes),]
	
	if (length(probes) == 1 && is.na(probes)){
		stop("You must provide a data frame in order to lookup any probes. See the documentation for this function for more details.")
	}
	
	if (!all(colnames(probes) %in% c("platform", "Name"))){
		stop("The columns of the data.frame must be named exactly 'ProbeName', and 'PlatformID' (in either order).")
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
		
		soap <- data.frame(do.call(rbind, soap), row.names=1)
				
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


 