\name{openspending.datasets}
\alias{openspending.datasets}
\title{openspending.datasets}
\usage{
  openspending.datasets(territory = NA, language = NA)
}
\arguments{
  \item{territory}{(Optional): searches for datasets in a
  specific country (2 letter ISO codes)}

  \item{language}{(Optional): searches for datasets in a
  specifig langue (2 letter code)}
}
\description{
  Accesses all Openspending Datasets
}
\examples{
require(ropenspending)
data=openspending.datasets(territory="GB") # get all great britain data sets
openspending.list.to.data.frame(data) # convert to data frame
}

