require("RCurl")
require("rjson")

openspending.host="http://openspending.org"
openspending.api=paste(openspending.host,"/api/2/",sep="")
openspending.datasets <- function(territory=NA,language=NA) {
  url=paste(openspending.host,"/datasets.json?",sep="")
  if (!is.na(territory)) {
    url=paste(url,"&territories=",territory,"")
    }
  if (!is.na(language)) {
    url=paste(url,"&languages=",language,"")
    }
  j=getURL(url)
  data=fromJSON(j)
  return(data$datasets)
  }

openspending.aggregate <- function(dataset, cut=NA, drilldown=NA, measure="amount") {
  url=paste(openspending.api,"aggregate?dataset=",dataset,"&measure=",measure,sep="")
  if (!is.na(cut)) {
    url=paste(url,"&cut=",cut,sep="")
    }
  if (!is.na(drilldown)) {
    url=paste(url,"&drilldown=",drilldown,sep="")
    };
  j=getURL(url)
  data=fromJSON(j)
  if (!is.null(data$errors)) {
    write(data$errors,stderr())
    return(NULL)
    }
  return(data)
  }
