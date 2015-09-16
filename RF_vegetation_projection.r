library(randomForest)
library(maptools)
library(rgdal)
require(caret)
require(e1071)
require(SDMTools)

setwd("./data")

dat = as.data.frame(read.csv("dat.csv")[-1,])
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

###################### predicting future vegetation  ##########################
BPS.Pred.future <- predict(predictors.future, bps.rf) #raster method to predict vegetation distribution
writeRaster(BPS.Pred.future, filename="BPS.Pred.future.asc", format="ascii", overwrite=TRUE)  



