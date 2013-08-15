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
