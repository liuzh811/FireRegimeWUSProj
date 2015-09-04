library(randomForest)
library(maptools)
library(rgdal)
require(caret)
require(e1071)
require(SDMTools)

setwd("./data")

dat = read.csv("dat.csv")[-1,]
dat = subset(dat, !is.na(dat$Aspect))
dat = subset(dat, !is.na(dat$BPS))
dat = subset(dat, !is.na(dat$Tavg))
dat = subset(dat, !is.na(dat$Heatload))
dat = subset(dat, !is.na(dat$LandOnShp))
dat = subset(dat, !is.na(dat$TSI))

dat = as.data.frame(dat)
dat$BPS = as.factor(dat$BPS)
dat$LandOnShp = as.factor(dat$LandOnShp)

###################### random forest fitting  ###########################################
# BPS: vegtation type, 13 class
# Tjan,Tjul,Tmaysep: mean january, july, and May to september temperature
# PPTjan,PPTjul,PPTmaysep: mean january, july, and May to september precipitation
# DEM: elevation
# Slope: slope
# Aspect: aspect
# Heatload: heated load index
# TSI: terrain shape index
bps.rf <- randomForest(BPS ~ Tjan+Tjul+Tmaysep+PPTjan+PPTjul+PPTmaysep+DEM+Slope+Aspect+Heatload+TSI, #formula
                       data=dat,
                       na.rm = TRUE,
                       ntree=500, #Number of trees to grow. 
                       mtry = 3) #Number of variables randomly sampled as candidates at each split.

varImpPlot(bps.rf, sort=TRUE) # variable importance plot
bps.rf #show compontent of results

####################### calculate kappa statistic  ###################
dat$Pred <- predict(bps.rf, data = dat)
confusionMatrix(dat$Pred, dat$BPS) #confusionMatrix fuction in 'caret'
########################################################################
Confusion Matrix and Statistics

          Reference
Prediction   1   2   3   4   5   6   7   8   9  10  11  12  13  14
        1  142  11   1   2   2   4   5   1   8   2  41   2   4   8
        2   26 178   0  16   2   9   0   1   4   1  16   1  23  26
        3   16   0 185   0   0   1   7  12   8   0   0   3   0   0
        4   13  22   0 137   2  20  25  33  12   0   0   6  11   4
        5    7   1   0   5  70   7   1   2   7   3  11   7  27   5
        6   14  16   0  18  13 120   9   9   8   3   5   5  10  13
        7   40  10   4  43   2  14 268  30  14   1   0   8  16   0
        8    9   1   8  15   1  11   7 112  20   4   1   0   2   1
        9   20   4   6   2   3   2   6  19 151  46   0   6   3   1
        10   6   1   0   0   5   0   0   1  28  66   7   0   0   2
        11 142  39   0   0  25   1   0   0   7  16 737   0  48  39
        12   0   0   0   1   0   0   1   0   1   0   0   4   1   0
        13  52  31   0  25  92  24  10  12  19   3  63  26 655  80
        14  25  31   0   0   7   7   0   0   1   5  22   0  44 204

Overall Statistics
                                         
               Accuracy : 0.6087         
                 95% CI : (0.595, 0.6223)
    No Information Rate : 0.1815         
    P-Value [Acc > NIR] : < 2.2e-16      
                                         
                  Kappa : 0.5602         
 Mcnemar's Test P-Value : NA  
 ###############################################################################
 
 ##################### plot a single  tree  ####################################
 to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){

  if(dfrep[rownum,'status'] == -1){
    rval <- list()

    attr(rval,"members") <- 1
    attr(rval,"height") <- 0.0
    attr(rval,"label") <- dfrep[rownum,'prediction']
    attr(rval,"leaf") <- TRUE

  }else{##note the change "to.dendrogram" and not "to.dendogram"
    left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
    right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
    rval <- list(left,right)

    attr(rval,"members") <- attr(left,"members") + attr(right,"members")
    attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
    attr(rval,"leaf") <- FALSE
    attr(rval,"edgetext") <- dfrep[rownum,'split var']
  }

  class(rval) <- "dendrogram"
  return(rval)
}


tree <- getTree(bps.rf,1,labelVar=TRUE)

d <- to.dendrogram(tree)
str(d)
plot(d,center=TRUE,leaflab='none',edgePar=list(t.cex=1,p.col=NA,p.lty=0))

###################### predicting future vegetation  ##########################
BPS.Pred.future <- predict(predictors.future, bps.rf) #raster method to predict vegetation distribution
writeRaster(BPS.Pred.future, filename="BPS.Pred.future.asc", format="ascii", overwrite=TRUE)  



