library("RCurl")
library("rjson")

openspending.host="http://openspending.org"
openspending.api=paste(openspending.host,"/api/2/",sep="")

openspending.datasets <- function(territory=NA,language=NA) {
  url=paste(openspending.host,"/datasets.json?",sep="")
  if (!is.na(territory)) {
    url=paste(url,"&territories=",territory,sep="")
    }
  if (!is.na(language)) {
    url=paste(url,"&languages=",language,sep="")
    }
  print(url)  
  j=getURL(url)
  data=fromJSON(j)
  return(data$datasets)
  }

openspending.dimensions <- function(dataset) {
  url=paste(openspending.host,dataset,"dimensions.json",sep="/")
  j=getURL(url)
  return(fromJSON(j))
  }

openspending.model <- function(dataset) {
  url=paste(openspending.host,dataset,"model.json",sep="/")
  j=getURL(url)
  return(fromJSON(j))
  }


openspending.aggregate <- function(dataset, cut=NA, drilldown=NA, measure="amount",order=NA) {
  url=paste(openspending.api,"aggregate?dataset=",dataset,"&measure=",measure,sep="")
  if (!is.na(cut[1])) {
    url=paste(url,"&cut=",paste(cut,collapse="|"),sep="")
    }
  if (!is.na(drilldown[1])) {
    url=paste(url,"&drilldown=",paste(drilldown,collapse="|"),sep="")
    };
  if(!is.na(order[1])) {
    url=paste(url,"&order=",paste(drilldown,collapse="|"),sep="")
    };
  j=getURL(url)
  data=fromJSON(j)
  if (!is.null(data$errors)) {
    write(data$errors,stderr())
    return(NULL)
    }
  return(data)
  }

openspending._getChildren <- function (drilldown,data,measure="amount",p=T) {
  if (p && exists("mclapply") ) {
      t.lapply<-mclapply
      }
  else {
    t.lapply=lapply
    }
  if (is.na(drilldown[1])) {
    return (list())
    }
  if (length(drilldown)==1) {
    dd=drilldown[1]
    drilldown=NA
    }
  else {  
    dd=drilldown[1]
    drilldown=drilldown[seq(2,length(drilldown))]
    }
  getLabel <- function(x) {
    if (is.list(x[[dd]])) {
        return (x[[dd]]$label)
      }
      else {
        return (x[[dd]])
        }
    }
   names=as.vector(sapply(data,getLabel))
   if (is.na(drilldown[1])) {
    return (lapply(unique(names),function(x) {
      o=data[names==x][[1]];
      return(
        list(name=x, dimension=dd, amount=o[[measure]])
        )
      }
      ))
    }
   else {
    return(t.lapply(unique(names),function(x) {
      children=openspending._getChildren(drilldown,data[names==x],measure,p=F);
      amount=sum(as.vector(sapply(children,function(x) { return(x$amount)
      })));
      return (list(name=x, dimension=dd, amount=amount, children=children))
      }))
    }
  }

openspending.aggregateTree <- function(dataset, cut=NA, drilldown=NA, measure="amount", order=NA, p=T) {
  if (p) {
    require("multicore")
    }
  data=openspending.aggregate(dataset,cut=cut,drilldown=drilldown,measure=measure,order=order)
  root=list(amount=data$summary$amount,currency=data$summary$currency,children=openspending._getChildren(drilldown,data$drilldown,measure,p))
  return(root);
  }
