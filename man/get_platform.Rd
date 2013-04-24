\name{get_platform}
\alias{get_platform}
\title{Get All ProbeMapper Platforms}
\usage{
  get_platform(getter = getOption("pmGetter"),
    pmDataPackage = getOption("pmDP"))
}
\arguments{
  \item{getter}{The Getter to use when retrieving the data.
  You'll need to configure and use an HMACGetter in order
  to securely access most platforms.}

  \item{pmDataPackage}{The Data Package to use in
  retrieving the platforms.}
}
\description{
  Retrieves a list of all the platforms contained in the
  ProbeMapper system.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

