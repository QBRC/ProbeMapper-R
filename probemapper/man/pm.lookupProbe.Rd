\name{pm.lookupProbe}
\alias{pm.lookupProbe}
\title{Takes a data.frame of probes and looks up the IDs for each probe describes, returning a data.frame which is annotated with the probe ID.
The order of the rows in the data.frame is NOT maintained.}
\usage{
  pm.lookupProbe(server, probes)
}
\arguments{
  \item{server}{the SOAPServer object describing the
  location of the server to use}

  \item{probes}{a data.frame containing the probe names and
  platform IDs of the probes of which you want to retrieve
  the IDs. The column names must be exactly "Name" and
  "platform" in either order.}
}
\value{
  a data.frame with the same data that was put in (possibly
  in a different order) annotated with the ProbeID as the
  rowname.
}
\description{
  Takes a data.frame of probes and looks up the IDs for
  each probe describes, returning a data.frame which is
  annotated with the probe ID. The order of the rows in the
  data.frame is NOT maintained.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

