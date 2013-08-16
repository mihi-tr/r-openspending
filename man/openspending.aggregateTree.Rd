\name{openspending.aggregateTree}
\alias{openspending.aggregateTree}
\title{Openspending aggregateTree}
\usage{
  openspending.aggregateTree(dataset, cut = NA,
    drilldown = NA, measure = "amount", order = NA, p = T)
}
\arguments{
  \item{p}{(default=T) whether or not R should attempt to
  use parallel to parallelize building the tree - turn this
  off if you run out of memory or have other issues}

  \item{dataset}{the dataset you want to work on}

  \item{cut}{(optional) the cut (filter) you want to apply
  to the dataset e.g. \code{time.year:2012}, can be a
  vector of multiple conditions}

  \item{drilldown}{(optional) the drilldown you want to do
  - how you want the data aggregated - need to be dimension
  names, can be a vector for multiple level drilldown}

  \item{measure}{(default=amount) the measurement you want
  to have aggregated. Defaults to amount since this is the
  measure that has to exist in all datasets.}

  \item{order}{(optional) parameters to order by. e.g.
  \code{amount:asc}, can be a vector}
}
\description{
  Similar to \code{openspending.aggregate} but returns the
  results in a tree structure.
}
\examples{
require(ropenspending)

# get a tree-structured list starting from
# total->hauptfunktion->oberfunktion with aggregated summaries.

data=openspending.aggregateTree("de-bund",drilldown=c("hauptfunktion","oberfunktion"),
cut="time.year:2012")
}

