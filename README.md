A R package to interface with OpenSpending
==========================================

Goal: get this into CRAN - so long

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
openspending.datasets()
openspending.aggregate("medientransparenz-at",cut="time.year:2012",drilldown=c("medium","from"))
```
