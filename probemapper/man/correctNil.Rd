\name{correctNil}
\alias{correctNil}
\title{Correct Nil SOAP values}
\usage{
  correctNil(x)
}
\arguments{
  \item{x}{the parameter to check for nil status}
}
\value{
  x, if the parameter is not nill, NA if it was.
}
\description{
  Parser to correct nil arguments in the returned XML. We
  want them to be NAs.
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

