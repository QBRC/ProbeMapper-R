\name{pm.lookupProbe}
\alias{pm.lookupProbe}
\title{Convert between probe names and probe IDs.}
\usage{
  pm.lookupProbe(server, probeName, platformID, probeID)
}
\arguments{
  \item{server}{the SOAPServer object describing the
  location of the server to use}

  \item{probeID}{the ID of a probe (or a vector of such
  IDs) containing the internal IDs used by QBRC to identify
  a probe.}

  \item{probeName}{the vendor-given name of a probe (or a
  vector of such names)}

  \item{platformID}{specifies the ID(s) of the platforms
  which match the probe name. This must be specified if the
  probe name is given instead of the probe IDs. Must either
  be of length 1 (in which case it is assumed that all
  probe names are from the same platform), or the same
  length as the probeName vector.}
}
\value{
  a data.frame with the same data that was put in (possibly
  in a different order) annotated with the ProbeID as the
  rowname.
}
\description{
  Takes a list of probes as specified either by probe ID or
  (probe name and platform ID) and returns the information
  on that probe (ID, name, and platform). Note that if you
  do not specify the probeID, then you must specify BOTH
  the probeName and platformID. The order of the rows in
  the returned data.frame is NOT the same as the input, nor
  is it necessarily consistent between calls.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}
\seealso{
  \code{\link{pm.getPlatforms}}
}

