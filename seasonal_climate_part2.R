rm(list=ls())
#planting and growing means for each crop
#setwd("~/Columbia/Columbia Water Center/America's Water-Food/codes/Adi")
library(data.table); library(plyr); library(reshape)

load("Data/New/clim_norm.Rda")
clim = split(clim_norm, f =clim_norm$state)
clim =Filter(function(x) NROW(x)>0, clim)
season = read.csv("Data/New/PlantingHarvesting.csv")
counties = read.csv("Data/New/County_Master.csv")
crops = unique(season$crop)
set2 = list()

#corn
for(i in 1:length(clim)){
	data = clim[[i]]
	set1 = list()
		for(s in 1:length(crops)){
			if(!crops[s]=="ww"){
				data2= seasonal_means_nw(x = data, y=unique(crops)[s])
			} else{	
				data2= seasonal_means_ww(x = data, y=unique(crops)[s])
			}
			nn1 = c("climate","state","fips","growing","planting","year")
			data3 = data2[,colnames(data2)%in%nn1]
			colnames(data3)[which(colnames(data3)=="growing")] = paste(crops[s],"growing",sep="_")
			colnames(data3)[which(colnames(data3)=="planting")] =paste(crops[s],"planting",sep="_")
			
			set1[[s]] = data3		
		}
		#merge all growing
		data4 = Reduce(function(x,y) merge(x,y, all = TRUE, 
		by = c("fips","state","year","climate")), set1, accumulate=FALSE)
		set2[[i]] = data4		
}



seasonal_means = as.data.frame(data.table::rbindlist(set2))
save(seasonal_means, file = "Data/New/seasonal_means.Rda")
