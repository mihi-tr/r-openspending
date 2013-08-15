\name{openspending.distinct}
\alias{openspending.distinct}
\title{Openspending Distinct}
\usage{
  openspending.distinct(dataset, dimension)
}
\arguments{
  \item{dataset}{the dataset to work on}

  \item{dimension}{the dimension you want distinct values
  from}
}
\description{
  get the distinct (unique) values in a dimension
}
\examples{
require(ropenspending)

# get the distinct values of the "de-bund" dataset for the dimension
# "hauptfunktion"

openspending.distinct("de-bund","hauptfunktion")
}

