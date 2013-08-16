\name{openspending.list.to.data.frame}
\alias{openspending.list.to.data.frame}
\title{Openspending list.to.data.frame}
\usage{
  openspending.list.to.data.frame(lst)
}
\arguments{
  \item{lst}{the list to be converted to a dataframe}
}
\description{
  Converts an evenly structured list to a dataframe - the
  first level of lists will be interpreted as the rows
}
\examples{
require(ropenspending)
data=openspending.datasets(territory="GB") # get all great britain data sets
openspending.list.to.data.frame(data) # convert to data frame
}

