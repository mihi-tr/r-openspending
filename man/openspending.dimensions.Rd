\name{openspending.dimensions}
\alias{openspending.dimensions}
\title{Openspending Dimensions}
\usage{
  openspending.dimensions(dataset)
}
\arguments{
  \item{dataset}{the dataset you want to have information
  on}
}
\description{
  Gives information about the dimensions used in a Dataset
  (the parameters you could use to cut or drilldown your
  analysis)
}
\examples{
require(ropenspending)

# get the dimensions for the "de-bund" dataset
openspending.dimensions("de-bund") 
}

