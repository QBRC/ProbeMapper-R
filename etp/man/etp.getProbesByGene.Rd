\name{etp.getProbesByGene}
\alias{etp.getProbesByGene}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
etp.getProbesByGene(conn, entrezID, platformID, authorityID)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conn}{
%%     ~~Describe \code{conn} here~~
}
  \item{entrezID}{
%%     ~~Describe \code{entrezID} here~~
}
  \item{platformID}{
%%     ~~Describe \code{platformID} here~~
}
  \item{authorityID}{
%%     ~~Describe \code{authorityID} here~~
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
function (conn, entrezID, platformID, authorityID) 
{
    url <- paste(conn["url"], "getProbes?", conn["keyStr"], sep = "")
    if (missing(entrezID)) {
        stop("You must provide an EntrezID")
    }
    else {
        url <- paste(url, "&eid=", entrezID, sep = "")
    }
    if (!missing(authorityID)) {
        authorityID <- paste(authorityID, collapse = ",")
        url <- paste(url, "&authid=", authorityID, sep = "")
    }
    if (!missing(platformID)) {
        platformID <- paste(platformID, collapse = ",")
        url <- paste(url, "&platid=", platformID, sep = "")
    }
    dat <- fromJSON(file = url)
    nullToNA <- function(x) sapply(x, function(x) if (is.list(x)) 
        nullToNA(x)
    else if (is.null(x)) 
        NA
    else x, simplify = FALSE)
    dat <- nullToNA(dat)
    dat$probes <- data.frame(t(sapply(dat$probes, "[")))
    return(dat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
