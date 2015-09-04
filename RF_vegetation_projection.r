library(randomForest)
library(maptools)
library(rgdal)
require(caret)
require(e1071)
require(SDMTools)

setwd("./data")

dat.rf = read.csv("dat.csv")

###################### random forest fitting  ###########################################
# BPS: vegtation type, 13 class
# Tjan,Tjul,Tmaysep: mean january, july, and May to september temperature
# PPTjan,PPTjul,PPTmaysep: mean january, july, and May to september precipitation
# DEM: elevation
# Slope: slope
# Aspect: aspect
# Heatload: heated load index
# TSI: terrain shape index
bps.rf <- randomForest(BPS ~ Tjan+Tjul+Tmaysep+PPTjan+PPTjul+PPTmaysep+DEM+Slope+Aspect+Heatload+TSI, data=dat.rf,na.rm = TRUE)
varImpPlot(bps.rf, sort=TRUE)

dat$Pred <- predict(bps.rf, data = dat.rf)

confusionMatrix(dat$Pred, dat$BPS) #confusionMatrix fuction in 'caret'
########################################################################
Confusion Matrix and Statistics

          Reference
Prediction   1   2   3   4   5   6   7   8   9  10  11  12  13  14
        1  132  15   2   5   1   4   5   2   6   3  46   1   7   7
        2   20 160   0  13   4   7   0   0   2   3  11   2  15  40
        3   19   0 176   0   0   0   4  10   2   0   0   0   0   0
        4   21  22   0 153   5  22  22  25   6   0   0   3  18   5
        5    8   8   0   8 102  21   1   2  12   6   8   9  28  11
        6   14   8   0  12  15  88   8   9   9   2   0   4   5   5
        7   39  14   2  29   2   7 291  40  14   0   0   4  20   0
        8   10   2   9  25   1  16  14 107  20   3   0   0   4   0
        9   16   3   2   4   5   4   1  15 111  37   0   2   2   0
        10   7   6   0   0   2   0   0   2  33  76  13   0   0   1
        11 157  27   0   0  16   0   0   0   7  21 715   3  67  25
        12   0   0   0   1   0   3   0   0   2   0   0   8   2   0
        13  58  25   0  25 103  20  14  12  13   4  66  19 693  82
        14  26  35   0   1  10   4   0   0   1   2  24   3  41 203

Overall Statistics
                                          
               Accuracy : 0.6052          
                 95% CI : (0.5914, 0.6188)
    No Information Rate : 0.1811          
    P-Value [Acc > NIR] : < 2.2e-16       
                                          
                  Kappa : 0.5552          
 Mcnemar's Test P-Value : NA   
 ###############################################################################
 
 
 ###################### predicting future vegetation  ##########################
BPS.Pred.future <- predict(predictors.future, bps.rf) #raster method to predict vegetation distribution
writeRaster(BPS.Pred.future, filename="BPS.Pred.future.asc", format="ascii", overwrite=TRUE)  