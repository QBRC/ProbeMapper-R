\name{customSOAP}
\alias{customSOAP}
\title{This is a modification of the .SOAP function available in the SSOAP package. At the time of writing, SSOAP couldn't properly handle the conversion of arguments into XML, so this function accepts a custom "body" element and statically defines much of the SOAP envelope, itself. This is less flexible, but gets the job done until the .SOAP function can be fixed to meet our needs.}
\usage{
  customSOAP(server, body, method, ..., .soapArgs = list(),
    action, nameSpaces = SOAPNameSpaces(), xmlns = NULL,
    handlers = SOAPHandlers(), .types = NULL,
    .convert = TRUE, .opts = list(),
    curlHandle = getCurlHandle(),
    .header = getSOAPRequestHeader(action, .server = server),
    .literal = FALSE, .soapHeader = NULL,
    .elementFormQualified = FALSE, .returnNodeName = NA)
}
\arguments{
  \item{server}{the SOAPServer object describing the
  location of the server to use}

  \item{body}{anything inside the body of the SOAP Request
  envelope}
}
\value{
  a list representing the SOAP values returned
}
\description{
  This is a modification of the .SOAP function available in
  the SSOAP package. At the time of writing, SSOAP couldn't
  properly handle the conversion of arguments into XML, so
  this function accepts a custom "body" element and
  statically defines much of the SOAP envelope, itself.
  This is less flexible, but gets the job done until the
  .SOAP function can be fixed to meet our needs.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

