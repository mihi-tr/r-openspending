\name{openspending.aggregate}
\alias{openspending.aggregate}
\title{Openspending Aggregate}
\usage{
  openspending.aggregate(dataset, cut = NA, drilldown = NA,
    measure = "amount", order = NA)
}
\arguments{
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
  Uses the Aggregate API to get data out of a dataset on
  OpenSpending and perform basic analysis
}
\examples{
require(ropenspending)

# get an aggregation of the "de-bund" dataset aggregated by "hauptfunktion"
# and "oberfunktion" (results in one entry per combination of both) for the
# year 2012
data=openspending.aggregate("de-bund",
  drilldown=c("hauptfunktion","oberfunktion"),cut="time.year:2012")

# convert the result to a dataframe
df=openspending.as.data.frame(data)

# sum up all amounts
sum(df[["amount"]])
}

