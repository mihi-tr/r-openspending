A R package to interface with OpenSpending
==========================================

Goal: get this into CRAN - so long
Status: Submitted to CRAN waiting for inclusion/approval

Installation
------------

clone from github, then:
```bash
R CMD INSTALL r-openspending
```

Requires
--------
* RCurl
* rjson

If present ```multicore``` will be used to paralellize aggregateTree.

Usage
-----
```R
require(ropenspending)

data=openspending.aggregate("medientransparenz-at",cut="time.year:2012",drilldown=c("medium","from"))
df=openspending.as.data.frame(data)
df[["amount"]]
```

For mor examples check the examples/ directory or ```?openspending```
