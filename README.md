A R package to interface with OpenSpending
==========================================

Goal: get this into CRAN - so long

```R
source("/path/to/r-openspending/R/r-openspending.R")
openspending.datasets()
openspending.aggregate("medientransparenz-at",cut="time.year:2012",drilldown="medium|from")
```
