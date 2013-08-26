\name{openspending.search}
\alias{openspending.search}
\title{Openspending search}
\usage{
  openspending.search(q = NA, dataset = NA, filter = NA,
    category = NA)
}
\arguments{
  \item{q}{(optional) the query string}

  \item{dataset}{(optional) the dataset to be searched}

  \item{filter}{(optional) a field to filter for}

  \item{category}{(optional) filter for category (budget,
  spending or other)}
}
\description{
  uses the openspending search API to search for specific
  records in a dataset
}
\examples{
require(ropenspending)

openspending.search(dataset="ukgov-25k-spending",
  filter=c("to.name:teachers-pensions","time.year:2012"))
}

