####principal component regression and Partial Least Square Regression

rm(list=setdiff(ls(), 'finalData', stats))
#setwd("~/Columbia/Columbia Water Center/America's Water-Food/codes/Adi")
library('dplyr')
library('pls')
library('ggplot2')
set.seed(1234)

load('data/New/FINALData.Rda')


cropGroups=split(finalData, list(finalData$crop, finalData$ccls, finalData$method))
n=11
for(name in names(cropGroups)){
  print(n)
  print(name)
  n=n+1
  
  df=cropGroups[[name]]
  l=nrow(df) #number of rows in dataframe
  print(paste("number of rows - ", l, sep=""))
  
  if(l>0){
    ##predictions for
    k=round(l/3) #observations to leave out
    ##Number of iterations,cross validation of iterations
    iter=min(l, 5000)
    ## Yield and Climate predictors #change to %Na logic
    
    # for(i in 1:ncol(Xy)){
    # Xy[is.na(data[,i]), i] <- mean(data[,i], na.rm = TRUE)
    # }
    
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
    
    
    
    #### model 
    p=85 ## percentage variance explained by components
    # yl = matrix(0,i # 2*k because first k columns will record the random indices being chosen and last k columns will record the predictands
    yl <- array(NaN, c(l, iter));
    yl2 <- array(NaN, c(l, iter));
    nc1Mat = matrix(NaN,iter,1)
    nc2Mat = matrix(NaN,iter,1)
    
    for (j in 1:iter) {
      ind = sample(1:l,k,replace=F) # k numbers in between 1 to l are chosen randomly
      XyTrain = dfFinal[-ind,] #train set
      XyTest = dfFinal[ind,] #test set
      #fl = pcr(norm_yield~., data = XyTrain, validation = "CV")
      fl = pcr(norm_yield~dryspell_growing+hwaves_growing+max_dailyprcp_growing+max_fiveday_growing+nfrost_growing+nhot_growing+prcp_int_growing+prcp_total_growing+rainy_growing+tmax_growing+tmean_growing+tmin_growing+dryspell_planting+hwaves_planting+max_dailyprcp_planting+max_fiveday_planting+nfrost_planting+nhot_planting+prcp_int_planting+prcp_total_planting+rainy_planting+tmax_planting+tmean_planting+tmin_planting, data = dfFinal)
      fl2 = plsr(norm_yield~dryspell_growing+hwaves_growing+max_dailyprcp_growing+max_fiveday_growing+nfrost_growing+nhot_growing+prcp_int_growing+prcp_total_growing+rainy_growing+tmax_growing+tmean_growing+tmin_growing+dryspell_planting+hwaves_planting+max_dailyprcp_planting+max_fiveday_planting+nfrost_planting+nhot_planting+prcp_int_planting+prcp_total_planting+rainy_planting+tmax_planting+tmean_planting+tmin_planting, data = dfFinal)
      # summary(fl)
      if (explvar(fl)[1]>p) {nc=1
      } else if (explvar(fl)[1]+explvar(fl)[2]>p) {nc = 2
      }else if (explvar(fl)[1]+explvar(fl)[2]+explvar(fl)[3]>p) {nc = 3
      }else if (explvar(fl)[1]+explvar(fl)[2]+explvar(fl)[3]+explvar(fl)[4]>p){nc = 4
      }else if (explvar(fl)[1]+explvar(fl)[2]+explvar(fl)[3]+explvar(fl)[4]+explvar(fl)[5]>p){nc = 5
      }else if (explvar(fl)[1]+explvar(fl)[2]+explvar(fl)[3]+explvar(fl)[4]+explvar(fl)[5]+explvar(fl)[6]>p) {nc = 6
      }else if (explvar(fl)[1]+explvar(fl)[2]+explvar(fl)[3]+explvar(fl)[4]+explvar(fl)[5]+explvar(fl)[6]+explvar(fl)[7]>p){nc=7
      }else if (explvar(fl)[1]+explvar(fl)[2]+explvar(fl)[3]+explvar(fl)[4]+explvar(fl)[5]+explvar(fl)[6]+explvar(fl)[7]+explvar(fl)[8]>p){nc=8
      }else if (explvar(fl)[1]+explvar(fl)[2]+explvar(fl)[3]+explvar(fl)[4]+explvar(fl)[5]+explvar(fl)[6]+explvar(fl)[7] +explvar(fl)[8] +explvar(fl)[9]>p){nc=9
      }else{nc=10}
      
      if (explvar(fl2)[1]>p) {nc2=1
      } else if (explvar(fl2)[1]+explvar(fl2)[2]>p) {nc2 = 2
      }else if (explvar(fl2)[1]+explvar(fl2)[2]+explvar(fl2)[3]>p) {nc2 = 3
      }else if (explvar(fl2)[1]+explvar(fl2)[2]+explvar(fl2)[3]+explvar(fl2)[4]>p){nc2 = 4
      }else if (explvar(fl2)[1]+explvar(fl2)[2]+explvar(fl2)[3]+explvar(fl2)[4]+explvar(fl2)[5]>p){nc2 = 5
      }else if (explvar(fl2)[1]+explvar(fl2)[2]+explvar(fl2)[3]+explvar(fl2)[4]+explvar(fl2)[5]+explvar(fl2)[6]>p) {nc2 = 6
      }else if (explvar(fl2)[1]+explvar(fl2)[2]+explvar(fl2)[3]+explvar(fl2)[4]+explvar(fl2)[5]+explvar(fl2)[6]+explvar(fl2)[7]>p){nc2=7
      }else if (explvar(fl2)[1]+explvar(fl2)[2]+explvar(fl2)[3]+explvar(fl2)[4]+explvar(fl2)[5]+explvar(fl2)[6]+explvar(fl2)[7]+explvar(fl2)[8]>p){nc2=8
      }else if (explvar(fl2)[1]+explvar(fl2)[2]+explvar(fl2)[3]+explvar(fl2)[4]+explvar(fl2)[5]+explvar(fl2)[6]+explvar(fl2)[7] +explvar(fl2)[8] +explvar(fl2)[9]>p){nc2=9
      }else{nc2=10}
      
      fit = predict(fl, ncomp = nc, newdata = XyTest) 
      fit2 = predict(fl2, ncomp = nc2, newdata = XyTest) 
      yl[ind,j] = fit
      yl2[ind,j] = fit2
      nc1Mat[j] = nc
      nc2Mat[j] = nc2
      if(j%%100==0){
        print(paste("reached here for j=",j)) 
      }
      
    }
    
    
    
    meanPredictions=rowMeans(yl,na.rm = TRUE)
    meanPredictions2=rowMeans(yl2,na.rm = TRUE)
    # medianPredictions=r
    dfFinal$meanPCRPredictions=meanPredictions
    dfFinal$meanPLSRPredictions=meanPredictions2
    rmse=sqrt( mean( (dfFinal$meanPCRPredictions - dfFinal$norm_yield)^2 , na.rm = TRUE ) )
    rmse2=sqrt( mean( (dfFinal$meanPLSRPredictions - dfFinal$norm_yield)^2 , na.rm = TRUE ) )
    print(rmse)
    print(rmse2)
    
    PCRcor=cor.test(dfFinal$meanPCRPredictions,dfFinal$norm_yield,method="pearson",use='pairwise')
    PLSRcor=cor.test(dfFinal$meanPLSRPredictions,dfFinal$norm_yield,method="pearson",use='pairwise')
    
    if(!exists('stats')){
      stats=data.frame(name=c('RMSE_PCR', 'RMSE_PLSR', 'r_PCR', 'r_PLSR', 'p_PCR','p_PLSR', 'nc_PCR', 'nc_PLSR', 'iterations'), cropGroup=NA)
    }else{
      stats=cbind(stats, c(rmse, rmse2, PCRcor$estimate, PLSRcor$estimate, PCRcor$p.value, PLSRcor$p.value,
                           mean(nc1Mat), mean(nc2Mat), iter))
      names(stats)[ncol(stats)]=name
    }
    
    
    
    p<- ggplot(dfFinal)+ ggtitle(name)+ geom_point(aes(x = norm_yield, y = meanPCRPredictions), fill='#ffafaf',shape=21, stroke=0.2) + geom_smooth(aes(norm_yield, meanPCRPredictions), se=FALSE, method='lm', color='#ff5e5e', size=0.5) + 
      geom_point(aes(x = norm_yield, y = meanPLSRPredictions), fill='#8fadea', shape=21, stroke=0.2)+geom_smooth(aes(norm_yield, meanPLSRPredictions), se=FALSE, method='lm', color='#336fe8', size=0.5)+ylab('Predicted Yield')+xlab('Observed Yield')
    ggsave(filename=paste("Output/RegPlots/", name, ".png", sep=""), plot=p)
    }
    
  }


stats=stats[,-2]
stats[2:ncol(stats)]=round(stats[2:ncol(stats)], digits=2)
write.csv(stats,file="Output/Stats.csv", row.names = TRUE)
  