#Load Seasonal means data and Standardized crop yields data
rm(list=ls())
#setwd("~/Columbia/Columbia Water Center/America's Water-Food/codes/Adi")

load("Data/New/seasonal_means.Rda")
load("Data/New/crops_irrigatedNorm.Rda")
load("Data/New/crops_NonirrigatedNorm.Rda")
load("Data/New/crops_norm.Rda")


##### Rice is supposed to be in irrigated -- but we only looked at pooled for rice so in this case so i didnt fix####
nn2 = names(seasonal_means)
#first pooled set

all= list()
irrigated =list()
rain = list()

data = crops_norm
for(i in 1:length(data)){
	to_m = paste(names(data)[i],"fips","year","climate", sep="|")
	seasonal1 = grepl(to_m, nn2)
	seasonal2 = seasonal_means[,seasonal1]
	seasonal2$test = rowSums(seasonal2[,c(4:5)])
	seasonal2 = seasonal2[is.finite(seasonal2$test),]; seasonal2$test =NULL
	colnames(seasonal2) = c("fips","year","climate","growing","planting")
	colnames(data[[i]]) = c("year","fips","state","county","harvest","produced","yield","norm_yield")
	crops1 = data[[i]]; crops1 = crops1[is.finite(crops1$norm_yield),]
	seasonal3 = merge(crops1, seasonal2, by= c("fips","year"))
	all[[i]] = seasonal3
	all[[i]]$method = "all"	
	all[[i]]$crop = names(data)[i]	

}


# Irrigated and Rainfed

data = crops_NonirrigatedNorm
for(i in 1:length(data)){
	to_m = paste(names(data)[i],"fips","year","climate", sep="|")
	seasonal1 = grepl(to_m, nn2)
	seasonal2 = seasonal_means[,seasonal1]
	seasonal2$test = rowSums(seasonal2[,c(4:5)])
	seasonal2 = seasonal2[is.finite(seasonal2$test),]; seasonal2$test =NULL
	colnames(seasonal2) = c("fips","year","climate","growing","planting")
	colnames(data[[i]]) = c("year","fips","state","county","harvest","produced","yield","norm_yield")
	crops1 = data[[i]]; crops1 = crops1[is.finite(crops1$norm_yield),]
	seasonal3 = merge(crops1, seasonal2, by= c("fips","year"))
	rain[[i]] = seasonal3
	rain[[i]]$method = "rain"	
	rain[[i]]$crop = names(data)[i]	

}



data = crops_irrigatedNorm
for(i in 1:length(data)){
	to_m = paste(names(data)[i],"fips","year","climate", sep="|")
	seasonal1 = grepl(to_m, nn2)
	seasonal2 = seasonal_means[,seasonal1]
	seasonal2$test = rowSums(seasonal2[,c(4:5)])
	seasonal2 = seasonal2[is.finite(seasonal2$test),]; seasonal2$test =NULL
	colnames(seasonal2) = c("fips","year","climate","growing","planting")
	colnames(data[[i]]) = c("year","fips","state","county","harvest","produced","yield","norm_yield")
	crops1 = data[[i]]; crops1 = crops1[is.finite(crops1$norm_yield),]
	seasonal3 = merge(crops1, seasonal2, by= c("fips","year"))
	irrigated[[i]] = seasonal3
	irrigated[[i]]$method = "irrigated"	
	irrigated[[i]]$crop = names(data)[i]	

}

library(data.table)
all = as.data.frame(data.table::rbindlist(all))
irrigated = as.data.frame(data.table::rbindlist(irrigated))
rain = as.data.frame(data.table::rbindlist(rain))

test = list("all" = all, "irrigated" = irrigated, "rain"=rain)
data_final = as.data.frame(data.table::rbindlist(test))



#Save final merged file
save(data_final, file = "Data/New/data_final.Rda")


