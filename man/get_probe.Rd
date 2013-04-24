\name{get_probe}
\alias{get_probe}
\title{Get ProbeMapper Probes}
\usage{
  get_probe(probe.id, platform, probe.name,
    getter = getOption("pmGetter"),
    pmDataPackage = getOption("pmDP"))
}
\arguments{
  \item{probe.id}{The ID(s) of the probe(s) to retrieve.}

  \item{platform}{The platform(s) to include in the
  returned results}

  \item{probe.name}{The name(s) of the probe(s) to include
  in the returned results.}

  \item{getter}{The Getter to use when retrieving the data.
  You'll need to configure and use an HMACGetter in order
  to securely make most requests.}

  \item{pmDataPackage}{The Data Package to use in making
  the request.}
}
\description{
  Retrieves the specified probes from the ProbeMapper
  system.
}
\details{
  You may either specify the probe.id, or (the platform and
  the probe name).
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

