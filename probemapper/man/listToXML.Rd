\name{listToXML}
\alias{listToXML}
\title{Converts the given list to a character representation of XML with the root node named as provided.
This function will create a tree using the names given to each list with names and lists given in the leaf nodes}
\usage{
  listToXML(rootName = "root", data = list())
}
\arguments{
  \item{rootName}{the name of the root node for your tree}

  \item{data}{the list to be converted to XML}
}
\value{
  a character representation of the XML tree created
}
\description{
  Converts the given list to a character representation of
  XML with the root node named as provided. This function
  will create a tree using the names given to each list
  with names and lists given in the leaf nodes
}
\author{
  Jeffrey D. Allen \email{Jeffrey.Allen@UTSouthwestern.edu}
}

