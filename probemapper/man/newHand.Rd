\docType{data}
\name{newHand}
\alias{newHand}
\title{Custom SOAP Response Handler.}
\format{List of 2
 $ action:function (action, method, server, xmlns, sep = "#")  
 $ result:function (xmlSource, header, method, server)}
\usage{
  newHand
}
\description{
  This is a custom handler for the SOAP response. Similar
  to the default, but doesn't assume nesting the way the
  default does
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}
\keyword{datasets}

