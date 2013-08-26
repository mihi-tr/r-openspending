require(ropenspending)

openspending.search(dataset="ukgov-25k-spending",
  filter=c("to.name:teachers-pensions","time.year:2012"))
