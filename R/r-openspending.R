require("RCurl")
require("rjson")

openspending.host="http://openspending.org"
openspending.api=paste(openspending.host,"/api/2/",sep="")
openspending.datasets <- function() {
  j=getURL(paste(openspending.host,"/datasets.json",sep=""))
  data=fromJSON(j)
  return(data$datasets)
  }

openspending.aggregate <- function(dataset, cut=NA, drilldown=NA) {
  url=paste(openspending.api,"aggregate?dataset=",dataset,sep="")
  if (!is.na(cut)) {
    url=paste(url,"&cut=",cut,sep="")
    }
  if (!is.na(drilldown)) {
    url=paste(url,"&drilldown=",drilldown,sep="")
    };
  j=getURL(url)
  print (j)
  data=fromJSON(j)
  return(data)
  }
