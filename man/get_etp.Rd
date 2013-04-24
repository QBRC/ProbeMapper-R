\name{get_etp}
\alias{get_etp}
\title{Get Entrez-To-Probe Mappings}
\usage{
  get_etp(probe, entrez, auth,
    getter = getOption("pmGetter"),
    pmDataPackage = getOption("pmDP"))
}
\arguments{
  \item{probe}{The ID(s) of the probe(s) for which to
  retrieve ETP mappings.}

  \item{entrez}{The Entrez ID(s) of the gene(s) for which
  to retrieve ETP mappings.}

  \item{auth}{The IDs of the authorities to include in the
  filtered list.}

  \item{getter}{The Getter to use when retrieving the data.
  You'll need to configure and use an HMACGetter in order
  to securely make most requests.}

  \item{pmDataPackage}{The Data Package to use in making
  the request.}
}
\description{
  Retrieves the specified Entrez-To-Probe mappings from
  ProbeMapper
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

