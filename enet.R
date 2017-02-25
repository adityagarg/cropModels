####principal component regression and Partial Least Square Regression
rm(list=ls())
# setwd("~/Columbia/Columbia Water Center/America's Water-Food/codes/Adi")
library('dplyr')
library('ggplot2')
library('glmnet')
library('caret')
set.seed(1234)

load('data/New/FINALData.Rda')

#Splitting by Crops.Bioclimatic region
cropGroups=split(finalData, list(finalData$crop, finalData$ccls, finalData$method))

for(name in names(cropGroups)){
  print(name)

  
  df=cropGroups[[name]]
  l=nrow(df) #number of rows in dataframe
  print(paste("number of rows - ", l, sep=""))
  
  
  #Check if df has >0 rows
  if(l>0){
    k=round(l/3) #observations to leave out
    ##Number of iterations,cross validation of iterations
    iter=min(l, 500)
    
    #Replace NA values with mean
    dfFinal=df
    for(i in 12:ncol(dfFinal)){
      if(is.numeric(dfFinal[,i])){
        meanVal=mean(dfFinal[,i], na.rm = TRUE)
        if(is.na(meanVal)){
          meanVal=0
        }
        dfFinal[is.na(dfFinal[,i]), i] <- meanVal
      }
    }
    
    
    #Alpha hyperparameter optimization for enet
    X=as.matrix(dfFinal[,-c(1,2,3,4,5,6,7,8,9,10, 11)])
    Y=as.matrix(dfFinal[,10])
    
    alpha.grid=seq(0,1, length=10)
    
    res <- matrix(0, nrow = length(alpha.grid), ncol = 6) # five columns for results - five repeats of each CV run
    for(i in 1:length(alpha.grid)){
      for(j in 2:6){
        cvmod <- cv.glmnet(X, Y, alpha = alpha.grid[i])
        res[i, c(1, j)] <- c(alpha.grid[i], sqrt(min(cvmod$cvm)))
      }
    }
    
    res <- data.frame(res)
    res$average_rmse <- apply(res[ , 2:6], 1, mean)
    res <- res[order(res$average_rmse), ]
    names(res)[1] <- "alpha"
    
    bestalpha <- res[1, 1]
    
    #### Elasticnet predictions with CV 
    yl <- array(NaN, c(l, iter));
    
    for (j in 1:iter) {
      ind = sample(1:l,k,replace=F) # k numbers in between 1 to l are chosen randomly
      XTrain = X[-ind,] #train set
      XTest = X[ind,] #test set
      YTrain = Y[-ind,] #train set
      YTest = Y[ind,] #test set
      cvmod <- cv.glmnet(XTrain, YTrain, alpha = bestalpha)
      
      # coeffs=data.frame(c('intercept',cvmod$glmnet.fit$beta@Dimnames[[1]]), as.vector(coef(mod1, s = cvmod$lambda.min)))
      
      fit=predict(cvmod, newx = XTest, s = "lambda.min")
      
      # summary(fl)
      yl[ind,j] = fit
      if(j%%100==0){
        print(paste("reached here for j=",j)) 
      }
      
    }
    
    
    #Compute mean prediction value
    meanPredictions=rowMeans(yl,na.rm = TRUE)
    dfFinal$meanPredictions=meanPredictions
    rmse=sqrt( mean( (dfFinal$meanPredictions - dfFinal$norm_yield)^2 , na.rm = TRUE ) )
    print(rmse)
    
    #Check correlation measure
    enetcor=cor.test(dfFinal$meanPredictions,dfFinal$norm_yield,method="pearson",use='pairwise')
    # PLSRcor=cor.test(dfFinal$meanPLSRPredictions,dfFinal$norm_yield,method="pearson",use='pairwise')
    
    #Generate statistics dataframe
    if(!exists('stats')){
      stats=data.frame(name=c('RMSE_enet','r_enet','p_enet','iterations'), cropGroup=NA)
      stats=cbind(stats, c(rmse, enetcor$estimate, enetcor$p.value, iter))
      names(stats)[ncol(stats)]=name
    }else{
      stats=cbind(stats, c(rmse, enetcor$estimate, enetcor$p.value, iter))
      names(stats)[ncol(stats)]=name
    }
    
    
    #Plot Predicted vs observed data
    p<- ggplot(dfFinal)+ ggtitle(name)+ geom_point(aes(x = norm_yield, y = meanPredictions), fill='#ffafaf',shape=21, stroke=0.2) + geom_smooth(aes(norm_yield, meanPredictions), se=FALSE, method='lm', color='#ff5e5e', size=0.5) + ylab('Predicted Yield')+xlab('Observed Yield')
    ggsave(filename=paste("Output/RegPlots/enet_", name, ".png", sep=""), plot=p)
  }
  
}

#Export stats dataframe
stats=stats[,-2]
stats[2:ncol(stats)]=round(stats[2:ncol(stats)], digits=2)
write.csv(stats,file="Output/StatsEnet.csv", row.names = TRUE)
