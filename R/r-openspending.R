#' A R-Package for accessing Openspending
#'
#' Openspending is a global spending data analysis and visualization
#' plattform it offers a JSON based API to access various information of 
#' the stored data. \code{r-openspending} helps accessing openspending data
#' from within R
#'
#' @import RCurl rjson
#' @aliases openspending ropenspending
#' @name ropenspending-package
#' @keywords openspending
#' @docType package
#' @author Michael Bauer \email{michael.bauer@@okfn.org}
library("RCurl")
library("rjson")

openspending.host="http://openspending.org"
openspending.api=paste(openspending.host,"/api/2/",sep="")

#' openspending.datasets 
#'
#' Accesses all Openspending Datasets
#'
#' @name openspending.datasets
#' @param territory (Optional): searches for datasets in a specific country (2 letter ISO codes)
#' @param language (Optional): searches for datasets in a specifig langue (2 letter code)
#' @export
openspending.datasets <- function(territory=NA,language=NA) {
  url=paste(openspending.host,"/datasets.json?",sep="")
  if (!is.na(territory)) {
    url=paste(url,"&territories=",territory,sep="")
    }
  if (!is.na(language)) {
    url=paste(url,"&languages=",language,sep="")
    }
  j=getURL(url)
  data=fromJSON(j)
  return(data$datasets)
  }

#' Openspending Dimensions
#'
#' Gives information about the dimensions used in a Dataset (the parameters
#' you could use to cut or drilldown your analysis)
#'
#' @name openspending.dimensions
#' @param dataset the dataset you want to have information on
#' @export
openspending.dimensions <- function(dataset) {
  url=paste(openspending.host,dataset,"dimensions.json",sep="/")
  j=getURL(url)
  return(fromJSON(j))
  }

#' Openspending Model
#'
#' Shows you the internal model of the dataset in Openspending - what
#' columns do exist and how are they mapped to the source file
#' @name openspending.model
#' @param dataset the dataset you want to see the model of
#' @export
openspending.model <- function(dataset) {
  url=paste(openspending.host,dataset,"model.json",sep="/")
  j=getURL(url)
  return(fromJSON(j))
  }

#' Openspending Aggregate
#' 
#' Uses the Aggregate API to get data out of a dataset on OpenSpending and
#' perform basic analysis
#' @name openspending.aggregate
#' @param dataset the dataset you want to work on
#' @param cut (optional) the cut (filter) you want to apply to the dataset
#' e.g. \code{time.year:2012}, can be a vector of multiple conditions
#' @param drilldown (optional) the drilldown you want to do - how you want the data
#' aggregated - need to be dimension names, can be a vector for multiple
#' level drilldown
#' @param measure (default=amount) the measurement you want to have
#' aggregated. Defaults to amount since this is the measure that has to
#' exist in all datasets.
#' @param order (optional) parameters to order by. e.g. \code{amount:asc},
#' can be a vector
#' @export
openspending.aggregate <- function(dataset, cut=NA, drilldown=NA, measure="amount",order=NA) {
  url=paste(openspending.api,"aggregate?dataset=",dataset,"&measure=",measure,sep="")
  if (!is.na(cut[1])) {
    url=paste(url,"&cut=",paste(cut,collapse="|"),sep="")
    }
  if (!is.na(drilldown[1])) {
    url=paste(url,"&drilldown=",paste(drilldown,collapse="|"),sep="")
    };
  if(!is.na(order[1])) {
    url=paste(url,"&order=",paste(order,collapse="|"),sep="")
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
#' Openspending aggregateTree
#'
#' Similar to \code{openspending.aggregate) but returns the results in a
#' tree structure.
#' @name openspending.aggregateTree
#' @inheritParams openspending.aggregate
#' @param p (default=T) whether or not R should attempt to use multicore to
#' parallelize building the tree - turn this off if you run out of memory
#' or have other issues
#' @export
openspending.aggregateTree <- function(dataset, cut=NA, drilldown=NA, measure="amount", order=NA, p=T) {
  if (p) {
    require("multicore")
    }
  data=openspending.aggregate(dataset,cut=cut,drilldown=drilldown,measure=measure,order=order)
  root=list(amount=data$summary$amount,currency=data$summary$currency,children=openspending._getChildren(drilldown,data$drilldown,measure,p))
  return(root);
  }

#' Openspending as.data.frame
#'
#' converts the output from \code{openspending.aggregate} into a data.frame
#'
#' @name openspending.data.frame
#' @param data the output from openspending.aggregate
#' @export
openspending.as.data.frame <- function(data) {
  results=list()
  d=data$drilldown[[1]]
  for (i in names(d)) {
    if (is.list(d[[i]])) {
      for (j in names(d[[i]])) {
        results[[paste(i,j,sep=".")]]=as.vector(
            sapply(data$drilldown,function(x) {
              return(x[[i]][[j]]) }));
        
        }
      #labels=c(labels,paste(i,names(d[[i]]),sep="."))
    }
    else {
      results[[i]]=as.vector(sapply(data$drilldown,function(x) {return
      x[[i]]}))
      }
  }
  return(data.frame(results));    
}  
