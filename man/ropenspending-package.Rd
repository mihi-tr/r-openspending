\docType{package}
\name{ropenspending-package}
\alias{openspending}
\alias{ropenspending}
\alias{ropenspending-package}
\title{A R-Package for accessing Openspending}
\description{
  Openspending is a global spending data analysis and
  visualization plattform it offers a JSON based API to
  access various information of the stored data.
  \code{r-openspending} helps accessing openspending data
  from within R
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
\author{
  Michael Bauer \email{michael.bauer@okfn.org}
}
\keyword{openspending}

