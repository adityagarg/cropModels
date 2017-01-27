#Planting and growing means for Corn, Soy, Rice, and Spring wheat
season = read.csv("PlantingHarvesting.csv")
counties = read.csv("County_Master.csv")

#list dataset by state

seasonal_means_nw = function(x,y){
    data = data.frame(x)
    state = as.character(counties$STATE_NAME[counties$FIPS%in%data$fips])
    if(sum(duplicated(state)==FALSE)==1){
        state = state[1]
    } else{print("ERROR with state call")}
    #if the state grows the crop - or if the seasons are given by USDA
    if(state%in%season$State&y%in%season$crop[season$State==state]){
        first = match(season$Planting.Stop[which(season$crop==y&season$State==state)], names(data))+1
        second = match(season$Active.Harvest.Month[which(season$crop==y&season$State==state)], names(data))
        first_p = match(season$Planting.Start[which(season$crop==y&season$State==state)], names(data))
        second_p= first-1
        
        
        if(first==second){
            data$growing = data[,first]
        } else{
            data$growing = rowMeans(data[,c(first:second)], na.rm = TRUE)
        }
        
        if(first_p==second_p){
            data$planting = data[,first_p]
        } else{
            data$planting = rowMeans(data[,c(first_p:second_p)], na.rm = TRUE)
            
        }
        
    } else{
        data$growing= NA
        data$planting = NA
    }
    
    return(data)
    
}


seasonal_means_ww = function(x,y){
    data = data.frame(x)
    state = as.character(counties$STATE_NAME[counties$FIPS%in%data$fips])
    if(sum(duplicated(state)==FALSE)==1){
        state = state[1]
    } else{print("ERROR with state call")}
    #if the state grows the crop - or if the seasons are given by USDA
    if(state%in%season$State&y%in%season$crop[season$State==state]){
        first = match(season$Planting.Stop[which(season$crop==y&season$State==state)], names(data))+1
        second = match(season$Active.Harvest.Month[which(season$crop==y&season$State==state)], names(data))
        first_p = match(season$Planting.Start[which(season$crop==y&season$State==state)], names(data))
        second_p= first-1
        
        
        if(first==second){
            data$growing = data[,first]
        } else{
            data$growing = rowMeans(data[,c(first:second)], na.rm = TRUE)
        }
        
        if(first_p==second_p){
            data$planting = data[,first_p]
        } else{
            data$planting = rowMeans(data[,c(first_p:second_p)], na.rm = TRUE)
            
        }
        
        dat= split(data, f=list(data$fips, data$climate))
        new= vector("list", length=length(dat))
        for(s in 1:length(dat)){
            dat2 = dat[[s]]
            dat2 = dat2[order(dat2$year),]
            dat2a=dat2[1:(dim(dat2)[1]-1),]
            dat2b=dat2[2:dim(dat2)[1],]
            
            if(first_p==second_p){
                dat2b$planting = dat2a[,first_p]
            } else{
                dat2b$planting = rowMeans(dat2a[,c(first_p:second_p)], na.rm = TRUE)
            }
            
            
            control = match("Dec", names(dat2))
            padding = dat2[1,];padding$growing = NA; padding$planting = NA
            
            #if growing occurs years (not starting in december)
            
            if(first<control){
                grow1 = data.frame(dat2a[,c(first:control)])
                grow2 = dat2b[,1:second]
                grow11 = cbind(grow1,grow2)
                dat2b$growing = rowMeans(grow11, na.rm = TRUE)
                dat2b = rbind(padding, dat2b)
                #if growing starts in december
                
            } else if(first==control){
                grow1 = dat2a[,control]
                grow2 = dat2b[,1:second]	
                grow11 = cbind(grow1,grow2)
                dat2b$growing = rowMeans(grow11, na.rm = TRUE)
                dat2b = rbind(padding, dat2b)
                
                
            } else if(first>control){
                grow11 = dat2b[,1:second]
                dat2b$growing = rowMeans(grow11, na.rm = TRUE)
                dat2b = rbind(padding, dat2b)

            }else{print("ERROR with growing season wheat call")} #column (columns start with Jan)
            
            new[[s]] = dat2b
            
        }
        
        data = do.call(rbind.data.frame, new)
        
    } else{
        data$growing= NA #for no growing season on USDA report, but not neccessary because rolling mean already has padding
        data$planting = NA # same
    } 				
    
    return(data)	
    
} 



