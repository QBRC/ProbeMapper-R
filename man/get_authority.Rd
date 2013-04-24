\name{get_authority}
\alias{get_authority}
\title{Get All ProbeMapper Authorities}
\usage{
  get_authority(getter = getOption("pmGetter"),
    pmDataPackage = getOption("pmDP"))
}
\arguments{
  \item{getter}{The Getter to use when retrieving the data.
  You'll need to configure and use an HMACGetter in order
  to securely make most requests.}

  \item{pmDataPackage}{The Data Package to use in making
  the request.}
}
\description{
  Retrieves a list of all the authorities used in the
  ProbeMapper system.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

