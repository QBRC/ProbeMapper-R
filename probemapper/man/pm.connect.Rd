\name{pm.connect}
\alias{pm.connect}
\title{Sets up a variable for the connection to the API.}
\usage{pm.connect(url, key)}
\description{Sets up a variable for the connection to the API. You should pass such a connection variable to all future calls.}
\value{a connection object representing the connection to the LCDB API based on the input you've provided.}
\author{Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}}
\arguments{\item{url}{The base URL at which the API is running, i.e. "http://qbri.swmed.edu/etp/"}
\item{key}{the API key which you will use to connect to the API.}}
