rm(list=ls())
setwd("~/Columbia/Columbia Water Center/America's Water-Food/codes/Adi")
load('Data/New/data_final.Rda')
countyMaster=read.csv('Data/New/County_Master_ccls.csv')
countyMaster=countyMaster[,c(6,7)]
data_final_ccls=merge(data_final, countyMaster, by.x='fips', by.y = 'FIPS')


str(data_final_ccls)


library('reshape2')

blahWideGrowing=dcast(data_final_ccls, fips+year+method+crop~climate, value.var = 'growing')
names(blahWideGrowing)[5:16]=paste(names(blahWideGrowing[5:16]), "_growing", sep="")
blahWidePlanting=dcast(data_final_ccls, fips+year+method+crop~climate, value.var = 'planting')
names(blahWidePlanting)[5:16]=paste(names(blahWidePlanting[5:16]), "_planting", sep="")

data_final_ccls=subset(data_final_ccls, select=-c(climate,growing,planting))
data_final_ccls=unique(data_final_ccls)
finalData=merge(data_final_ccls, blahWideGrowing, by=c('fips', 'year', 'method','crop'), all.x=TRUE)
finalData=merge(finalData, blahWidePlanting, by=c('fips', 'year', 'method','crop'), all.x=TRUE)

save(finalData, file="Data/New/FINALData.Rda")
