# rm(list=setdiff(ls(), c("finalData")))
rm(list=ls())
#setwd("~/Columbia/Columbia Water Center/America's Water-Food/codes/Adi")
# library('corrplot')
library('dplyr')
library('psych')
library('xlsx')

load('data/New/FINALData.Rda')
str(finalData)

finalData=finalData[finalData$method=='all',] #modify code to iterate through all in one for loop

finalData=finalData[c(4,8:ncol(finalData))]
cropGroups=split(finalData, finalData$crop)
for(i in cropGroups)
{
  cropName=unique(i$crop)
  print(cropName)
  cclsGroups=split(i, i$ccls)
  for(j in cclsGroups){
    ccls=unique(j$ccls)
    print(ccls)
    jNum=j[sapply(j, is.numeric)]
    # cor.test(jNum$norm_yield,j$planting_dryspell,method="pearson",use='pairwise')
    if(nrow(jNum)>0){
      cor=corr.test(jNum[4:ncol(jNum)],jNum[3])
      corR=cor$r
      corP=cor$p
      corElig=corR*(cor$p<=0.1)
      colnames(corR)=paste(cropName,ccls, sep='_')
      colnames(corP)=paste(cropName,ccls, sep='_')
      colnames(corElig)=paste(cropName,ccls, sep='_')
      if(!exists('corRFinal')){
        corRFinal=corR
        corPFinal=corP
        corEligFinal=corElig
      } else{
        corRFinal=cbind(corRFinal, corR)
        corPFinal=cbind(corPFinal, corP)
        corEligFinal=cbind(corEligFinal, corElig)
      }
    }
  }
}

write.xlsx(corEligFinal, file="Output/Correlations_all.xlsx", sheetName = 'SignificantR')
write.xlsx(corRFinal, file="Output/Correlations_all.xlsx", sheetName = 'RValues', append=TRUE)
write.xlsx(corPFinal, file="Output/Correlations_all.xlsx", sheetName = 'PValues', append=TRUE)

