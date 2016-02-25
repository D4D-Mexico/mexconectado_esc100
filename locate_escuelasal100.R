#!/usr/bin/env Rscript
require(gdata)
require(geosphere)

Sys.setlocale("LC_ALL", "C")
mydir <- '~/mxabierto/mexconectado_esc100/'
setwd(mydir)
file100 <- './data/escuelasalcien.csv'
df100 <- read.csv(file=file100, header =  T, skip = 1, encoding = 'utf-8', strip.white = T)
filecct <- './data/cct.csv'
dfcct <- read.csv(file=filecct, header =  T, skip = 0, encoding = 'utf-8', strip.white = T)
geo100 <- dfcct[as.character(dfcct$clavecct) %in% as.character(df100$CCT),c("clavecct","longitud","latitud")]
fileMXcon <-'result.csv'
dfMXcon = read.csv(fileMXcon, header =  T, skip = 0, encoding = 'utf-8', strip.white = T)


result <- c()
result2 <- c()
for (cp in unique(dfMXcon$C..digo.Postal) ){
  if (is.element(cp,c(0,1800,16030,9220))){
    next
  }
  dfcctbycp <- row.names(dfcct[dfcct$codpost==cp,])
  dfMXcbycp <- row.names(dfMXcon[dfMXcon$C..digo.Postal==cp,])
  if (length(dfcctbycp)==0){
    next
  }
  test<-Filter(function(row)
    min(
          distHaversine(
            cbind(dfMXcon[row,'Longitud'], dfMXcon[row,'Latitud']), 
            cbind(dfcct[dfcctbycp,'longitud'], dfcct[dfcctbycp,'latitud'])
        ) ) < 50
        , dfMXcbycp 
    )
  result <- c(result,test)
  
  test2<-Filter(function(row)
    min(
      distHaversine(
        cbind(dfMXcon[row,'Longitud'], dfMXcon[row,'Latitud']), 
        cbind(dfcct[dfcctbycp,'longitud'], dfcct[dfcctbycp,'latitud'])
      ) ) < 50
    , dfcctbycp 
  )
  result2 <- c(result2,test2)
}


#selected_mxcon <- c()
#Filter(function(row) 
#  min(
#    distHaversine(
#      cbind(dfMXcon[row,'Longitud'], dfMXcon[row,'Latitud']), 
#      cbind(dfcct$longitud, dfcct$latitud)
#    ) ) < 1000
#  , row.names(dfMXcon) 
#)




#selected_cct <- dfcct[
#  gdist(
#    lon.1=dfcct$Longitud, 
#    lat.1=dfcct$Latitud, 
#    lon.2=dfMXcon$longitud, 
#    lat.2=dfMXcon$latitud,
#    units='m') < 500 , ]
#selected_100 <- df100[df100$CCT==selected_geo$clavecct,]
