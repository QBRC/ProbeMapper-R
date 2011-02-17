\name{etp.getGenesByProbe}
\alias{etp.getGenesByProbe}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
etp.getGenesByProbe(conn, probeID, platformID, probeName, authorityID)
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
function (conn, probeID, platformID, probeName, authorityID) 
{
    url <- paste(conn["url"], "getGenes?", conn["keyStr"], sep = "")
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
    if (!missing(authorityID)) {
        authorityID <- paste(authorityID, collapse = ",")
        url <- paste(url, "&authid=", authorityID, sep = "")
    }
    nullToNA <- function(x) sapply(x, function(x) if (is.list(x)) 
        nullToNA(x)
    else if (is.null(x)) 
        NA
    else x, simplify = FALSE)
    dat <- fromJSON(file = url)
    dat <- nullToNA(dat)
    dat$genes <- data.frame(t(sapply(dat$genes, "[")))
    return(dat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
