# load in crop data and name crop lists
rm(list=ls())
#setwd("~/Columbia/Columbia Water Center/America's Water-Food/codes/Adi")

load("Data/New/corn_all.Rda")
load("Data/New/soy_all.Rda")
load("Data/New/sw_all.Rda")
load("Data/New/ww_all.Rda")
load("Data/New/rice.Rda")
load("Data/New/corn_irrigated.Rda")
load("Data/New/soy_irrigated.Rda")
load("Data/New/sw_irrigated.Rda")
load("Data/New/ww_irrigated.Rda")
load("Data/New/corn_Nonirrigated.Rda")
load("Data/New/soy_Nonirrigated.Rda")
load("Data/New/sw_Nonirrigated.Rda")
load("Data/New/ww_Nonirrigated.Rda")

crops = list(corn_all,soy_all,sw_all,ww_all, RICE)
crops_irrigated = list(corn_irrigated,soy_irrigated,sw_irrigated,ww_irrigated)
crops_nonIrrigated = list(corn_Nonirrigated,soy_Nonirrigated,sw_Nonirrigated,ww_Nonirrigated, RICE)

library(zoo)
###choose crop list e.g.
data = crops

names(data) = c("corn", "soy", "sw", "ww","rice")
results_1=vector("list", length= length(data))
for(s in 1:length(data)){
data_crop = data[[s]]
	ll2 = split(data_crop, as.factor(data_crop $fips))
	#clean choose counties with greater than 10 observations --
	test = sapply(ll2, function(x) dim(x[1])[1]>=10)
	ll1 = ll2[test]
		#7 year moving window
			mean1 =vector("list", length = length(ll1))
			std1 = vector("list", length=length(ll1))
			crop_n = vector("list", length = length(ll1))
				for( i in 1:length(ll1)){
				first = ll1[[i]]
				std1[[i]] = rollapply(zoo(first[,10]),7,sd, fill = NA)
				mean1[[i]] = rollmean(first[,10], 7, fill=NA)
				dat = (first[,10] - mean1[[i]])/std1[[i]]
				dat2= first[,c(1,2,3,6,9,7,10)]
				dat3 = cbind(dat2, data.frame(dat))
				colnames(dat3)[8] = "norm_yield"
				crop_n[[i]] = dat3
					}
						crop_n=do.call(rbind.data.frame, crop_n)
						results_1[[s]] = crop_n
					
						}

crops_norm=results_1
names(crops_norm) = names(data)
###########################################################################################

###standardizing irrigated crops
data=crops_irrigated
names(data) = c("corn", "soy", "sw", "ww")
results_1=vector("list", length= length(data))
for(s in 1:length(data)){
data_crop = data[[s]]
	ll2 = split(data_crop, as.factor(data_crop $fips))
	#clean choose counties with greater than 10 observations --
	test = sapply(ll2, function(x) dim(x[1])[1]>=10)
	ll1 = ll2[test]
		#7 year moving window
			mean1 =vector("list", length = length(ll1))
			std1 = vector("list", length=length(ll1))
			crop_n = vector("list", length = length(ll1))
				for( i in 1:length(ll1)){
				first = ll1[[i]]
				std1[[i]] = rollapply(zoo(first[,10]),7,sd, fill = NA)
				mean1[[i]] = rollmean(first[,10], 7, fill=NA)
				dat = (first[,10] - mean1[[i]])/std1[[i]]
				dat2= first[,c(1,2,3,6,9,7,10)]
				dat3 = cbind(dat2, data.frame(dat))
				colnames(dat3)[8] = "norm_yield"
				crop_n[[i]] = dat3
					}
						crop_n=do.call(rbind.data.frame, crop_n)
						results_1[[s]] = crop_n
					
						}
crops_irrigatedNorm=results_1
names(crops_irrigatedNorm) = names(data)
###########################################################################################


###standardizing nonirrigated crops
data=crops_nonIrrigated
names(data) = c("corn", "soy", "sw", "ww", "rice")
results_1=vector("list", length= length(data))
for(s in 1:length(data)){
data_crop = data[[s]]
	ll2 = split(data_crop, as.factor(data_crop $fips))
	#clean choose counties with greater than 10 observations --
	test = sapply(ll2, function(x) dim(x[1])[1]>=10)
	ll1 = ll2[test]
		#7 year moving window
			mean1 =vector("list", length = length(ll1))
			std1 = vector("list", length=length(ll1))
			crop_n = vector("list", length = length(ll1))
				for( i in 1:length(ll1)){
				first = ll1[[i]]
				std1[[i]] = rollapply(zoo(first[,10]),7,sd, fill = NA)
				mean1[[i]] = rollmean(first[,10], 7, fill=NA)
				dat = (first[,10] - mean1[[i]])/std1[[i]]
				dat2= first[,c(1,2,3,6,9,7,10)]
				dat3 = cbind(dat2, data.frame(dat))
				colnames(dat3)[8] = "norm_yield"
				crop_n[[i]] = dat3
					}
						crop_n=do.call(rbind.data.frame, crop_n)
						results_1[[s]] = crop_n
					
						}
crops_NonirrigatedNorm=results_1
names(crops_NonirrigatedNorm) = names(data)

#Savind standardized files
save(crops_norm,file="Data/New/crops_norm.Rda")
save(crops_irrigatedNorm,file="Data/New/crops_irrigatedNorm.Rda")
save(crops_NonirrigatedNorm,file="Data/New/crops_NonirrigatedNorm.Rda")
