#normalising climate indicators using 7 year window centered on observation
#Y = y-mean(y(t+3:t-3))/std(y(t+3:t-3))
#setwd("~/Columbia/Columbia Water Center/America's Water-Food/codes/Adi")
library(plyr) 
library(zoo)
library(reshape)
load("Data/New/climate_split.Rda")

#normalising climate data
library(caTools); library(data.table)
new=list()
for(i in 1:length(climate_split)){
  dat = climate_split[[i]]
  dat = dat[order(dat$year),]
  x=as.matrix(dat[,c(1:12)])
  xx = runmean(x,7, endrule="mean", alg = "C")
  xx2 = runsd(x,7, endrule="NA")
  xx_cal = (x-xx)/(xx2)
  xx_cal = data.frame(xx_cal)
  xx = cbind(xx_cal, dat[,c(13:15)])
  #xx[c(1:3), c(1:12)] = NA
  #xx[c((NROW(xx)-2):NROW(xx)), c(1:12)] =NA
  new[[i]]=xx
  
}

climate_norm = as.data.frame(data.table::rbindlist(new))
states= read.csv("Data/New/County_Master.csv")
states = states[,c(3,6)]; colnames(states)=c("state", "fips")

# climate_norm<-merge(climate_norm, states, by = "fips")
clim_norm = merge(climate_norm, states, by = "fips")

#Saving stadardized climate files
save(clim_norm, file = "Data/New/clim_norm.Rda")










