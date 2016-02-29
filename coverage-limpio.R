#!/usr/bin/env Rscript
setwd('~/mxabierto/mexconectado_esc100/')

fileLimpio <- './data/ProgramasLimpio2.csv'
dfLimpio <- read.csv(file=fileLimpio, header =  T, skip = 0, encoding = 'utf-8', strip.white = T)

fileCover <- './data/CoverageByLocality.csv'
dfCover <- read.csv(file=fileCover, header =  T, skip = 0, encoding = 'utf-8', strip.white = T)

filecct <- './data/cct.csv'
dfcct <- read.csv(file=filecct, header =  T, skip = 0, encoding = 'utf-8', strip.white = T)
dfcct$CVE <-dfcct$entidad * 1e7 + dfcct$municipio * 1e4 + dfcct$localidad


filecat <- './data/CATALOGO_CT.csv'
dfcat <- read.csv(file=filecat, header =  T, skip = 0, encoding = 'utf-8', strip.white = T)
dfcat$CVE <-dfcat$ENT * 1e7 + dfcat$MUN * 1e4 + dfcat$LOC


cien_nocon <- dfLimpio[dfLimpio$MexicoConectado == 0 & dfLimpio$EscuelasCien == 1,]

for (row in row.names(cien_nocon)) {
  print(as.character(cien_nocon[row,'CCT']))
  cveml <-  dfcat[(strtrim(as.character(dfcat$CLAVE_CT),10) == as.character(cien_nocon[row,'CCT'])), 'CVE']
  print(cveml)
  remove_licality <- c(30010291, 30010049, 40020090, 40020118, 40110403, 40110166, 40040003, 40040047, 40050005,
    40080002, 70170426, 202650063, 71140019, 70290012, 70290008, 70210013, 70210006, 70210006, 70460047, 70330042,
    71160005, 70591799, 70590142, 70600001)
  if (length(cveml)==0 || is.na(cveml) ||
      as.numeric(cveml) %in% remove_licality){
    next
  }
  cien_nocon[row,'CVEML'] <- cveml[1]
  gsm_a <- dfCover[dfCover$CVE == cien_nocon[row,'CVEML'], c("TelcelGSM", "IusacelGSM", "Movistar") ]
  g3_a <- dfCover[dfCover$CVE == cien_nocon[row,'CVEML'], c("Telcel3G","Iusacel3G") ]
  if (nrow(gsm_a)==0 ){
    gsm <- 0
  } else{
    gsm <- sum(gsm_a)
  }
  if (nrow(g3_a)==0 ){
    g3 <- 0
  } else{
    g3 <- sum(g3_a)
  }
  if (is.na(gsm)){
    cveml <-  dfcct[(strtrim(as.character(dfcct$clavecct),10) == as.character(cien_nocon[row,'CCT'])), 'CVE']
    cien_nocon[row,'CVEML'] <- cveml
    gsm <- sum(dfCover[dfCover$CVE == cien_nocon[row,'CVEML'], c("TelcelGSM", "IusacelGSM", "Movistar") ])
    g3 <- sum(dfCover[dfCover$CVE == cien_nocon[row,'CVEML'], c("Telcel3G","Iusacel3G") ])
  }
  if (is.na(gsm) || is.nan(gsm) || is.na(g3) || is.nan(g3)){
    gsm <- 0
    g3 <- 0
  }
  if (!is.na(gsm) & gsm > 0){
    gsm <- 1
  }  
  print(g3)
  if (!is.na(g3) & g3 > 0){
    g3 <- 1
  }
  cien_nocon[row,'3g'] <- g3
  cien_nocon[row,'gsm'] <- gsm
  
}