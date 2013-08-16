require(ropenspending)
data=openspending.datasets(territory="GB") # get all great britain data sets
openspending.list.to.data.frame(data) # convert to data frame
