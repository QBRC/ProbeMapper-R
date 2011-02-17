\name{etp.getProbe}
\alias{etp.getProbe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
etp.getProbe(conn, probeID, platformID, probeName)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{probeID}{
%%     ~~Describe \code{probeID} here~~
}
  \item{platformID}{
%%     ~~Describe \code{platformID} here~~
}
  \item{probeName}{
%%     ~~Describe \code{probeName} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (conn, probeID, platformID, probeName) 
{
    url <- paste(conn["url"], "getProbe?", conn["keyStr"], sep = "")
    if (missing(probeID)) {
        if (missing(platformID) || missing(probeName)) {
            stop("You must provide either a probeID or a probeName + platformID combination.")
        }
        else {
            if (typeof(probeName) == "character") {
                probeName <- URLencode(probeName)
            }
            url <- paste(url, "&plid=", platformID, "&prname=", 
                probeName, sep = "")
        }
    }
    else {
        url <- paste(url, "&prid=", probeID, sep = "")
        if (!missing(platformID) || !missing(probeName)) {
            warn("The probeID was provided, so probeName and platformID will be ignored.")
        }
    }
    dat <- fromJSON(file = url)
    return(dat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
