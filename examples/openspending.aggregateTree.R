require(ropenspending)

# get a tree-structured list starting from
# total->hauptfunktion->oberfunktion with aggregated summaries.

data=openspending.aggregateTree("de-bund",drilldown=c("hauptfunktion","oberfunktion"),
cut="time.year:2012")
