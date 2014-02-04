require(ropenspending)
require(rCharts)

sankeyPlot=function(df){
  sankeyPlot <- rCharts$new()
  
  #--------
  #Sankey demo originated by @timelyportfolio
  #http://timelyportfolio.github.io/rCharts_d3_sankey/example_build_network_sankey.html
  
  #We need to tell R where the Sankey library is.
  #I put it as a subdirectory to my current working directory (.),
  ##which is assumed to be the examples directory
  sankeyPlot$setLib('../util/rCharts_d3_sankey-gh-pages/')
  
  #We also need to point to an HTML template page
  sankeyPlot$setTemplate(script = "../util/rCharts_d3_sankey-gh-pages/layouts/chart.html")
  #---------
  
  sankeyPlot$set(
    data = df,
    nodeWidth = 15,
    nodePadding = 10,
    layout = 32,
    width = 750,
    height = 500,
    labelFormat = ".1%"
  )
  
  sankeyPlot
}

#This routine makes it easier to get the data for plotting as a Sankey diagram
#Select the source, target and value column names explicitly to generate a dataframe containing
#just those columns, appropriately named.
sankeyData=function(df,colsource='source',coltarget='target',colvalue='value'){
  sankey.df=subset(df,select=c(colsource,coltarget,colvalue))
  colnames(sankey.df)=c('source','target','value')
  
  #Try to ensure that if source and target names are the same,
  ##we make them unique
  sankey.df$target=paste('_',sankey.df$target,sep='')
  sankey.df
}

#For example:
data=openspending.aggregate("uk-barnet-budget",drilldown=c("level1","level2"))
df=openspending.as.data.frame(data)
data.sdf=sankeyData(df,'level1.label','level2.label','amount')

sankeyPlot(data.sdf)