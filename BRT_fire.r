###########################################################################
# Boosted Regression Tree analysis
###########################################################################

setwd(".\\data")

library(gbm)
library(dismo)

###########################################################################
# Fit fire Occurrence Probability surface 
###########################################################################
Fire.1 = data.frame(read.csv("Fire.Occ.csv")[,-1])
Fire.1$LOshp = as.factor(Fire.1$LOshp)
Variable.list1.exclu = c("H90Mn","T90Mn","HPAo")
Fire.1 = Fire.1[ , -which(names(Fire.1) %in% Variable.list1.exclu)]

# select best BRT settings based on deviance and AUC
tc = c(1,2,3); lr = c(0.005, 0.01, 0.025, 0.05, 0.075, 0.1); nt = c(10, 20, 30, 40,50)
model.perf = c()						
for (i in 1:length(tc)){
 for (j in 1:length(lr)){
  for (k in 1:length(nt)){
 gbm1=gbm.step(data = Fire.1, gbm.x = 1:(ncol(Fire.1)-2),
						gbm.y = ncol(Fire.1)-1, 
						family = "bernoulli",
						tree.complexity = tc[i], 
						n.trees = nt[k], learning.rate = lr[j], bag.fraction = 0.75)
						
preds = predict.gbm(gbm1, Fire.1, n.trees = gbm1$gbm.call$best.trees, type="response")
dev = calc.deviance(obs=Fire.1$Status, pred=preds, calc.mean=TRUE)
	
d <- cbind(Fire.1$Status, preds)
pres <- d[d[,1]==1, 2]
abse <- d[d[,1]==0, 2]
e <- evaluate(p=pres, a=abse)
model.perf.t = c(tc[i],lr[j],nt[k],dev, e@auc, gbm1$gbm.call$best.trees)
model.perf = rbind(model.perf, model.perf.t)

print(paste("Finish Fitting tree complexity = ", tc[i], "; learning.rate = ", lr[j]," n.trees = ", nt[k], " at ", format(Sys.time(), "%a %b %d %X %Y"), sep = " ") )
  }
 }
}
write.csv(model.perf, "model.perf.occ.csv")

#best BRT setting: tree.complexity = 3; n.trees = 40; learning.rate = 0.075,with best tree names at 2720;  AUC = 0.97

#fit a fire occurrece model using best BRT setting
gbm.fire.occ=gbm.step(data = Fire.1, gbm.x = 1:(ncol(Fire.1)-2),
						gbm.y = ncol(Fire.1)-1, 
						family = "bernoulli",
						tree.complexity = 3, 
						n.trees = 40, learning.rate = 0.075, bag.fraction = 0.75)

#predicting fire occurrence probability suface						
p <- predict(predictors,                                        #a stack of predictors
	    gbm.fire.occ,                                       #fire occurrece model
            n.trees=gbm.fire.occ$gbm.call$best.trees,           #number of trees
	   type="response")						
###########################################################################
# fire size model
###########################################################################
Fire1Var.1 = as.data.frame(read.csv("Fire.SizeSeverity.csv")[,-1])
Fire1Var.1$LOshp = factor(Fire1Var.1$LOshp)
Fire1Var.1$Calacre_ha = log(Fire1Var.1$Calacre_ha)
Variable.list2.exclu = c("T90Mn","H90Ao","HPAo","HWAo")
Fire1Var.1 = Fire1Var.1[ , -which(names(Fire1Var.1) %in% Variable.list2.exclu)]

# select best BRT settings for fire size model
model.perf = c()						
for (i in 1:length(tc)){
 for (j in 1:length(lr)){
  for (k in 1:length(nt)){
gbm.fs=gbm.step(data = Fire1Var.1, gbm.x = 1:(ncol(Fire1Var.1)-4),
						gbm.y = ncol(Fire1Var.1)-3, 
						family = "gaussian",
						tree.complexity = tc[i], 
						n.trees = nt[k], learning.rate = lr[j], bag.fraction = 0.75)
						
r <- Fire1Var.1$Calacre_ha - predict.gbm(gbm.fs, Fire1Var.1,n.trees = gbm.fs$gbm.call$best.trees, type="response")
dev = 1-var(r)/var(Fire1Var.1$Calacre_ha) 
	
model.perf.t = c(tc[i],lr[j],nt[k],dev, gbm.fs$gbm.call$best.trees)
model.perf = rbind(model.perf, model.perf.t)

print(paste("Finish Fitting tree complexity = ", tc[i], "; learning.rate = ", lr[j]," n.trees = ", nt[k], " at ", format(Sys.time(), "%a %b %d %X %Y"), sep = " ") )
  }
 }
}
write.csv(model.perf, "model.perf.size.csv")
# best setting for fire size model, tree.complexity = 3; n.trees = 40; learning.rate = 0.05,with best tree names at 1960;  variance explained = 0.76

#fit a fire size model using best BRT setting
gbm.fs=gbm.step(data = Fire1Var.1, gbm.x = 1:(ncol(Fire1Var.1)-4),
						gbm.y = ncol(Fire1Var.1)-3, 
						family = "gaussian",
						tree.complexity = 3, 
						n.trees = 25, learning.rate = 0.05, bag.fraction = 0.75)

###########################################################################
# Fit fire Spead Probability surface 
###########################################################################
Fire.Spread = data.frame(read.csv("Fire.Spread.csv")[,-1])
Fire.Spread1$LOshp = as.factor(Fire.Spread$LOshp)

# select best BRT settings based on deviance and AUC
tc = c(1,2,3); lr = c(0.005, 0.01, 0.025, 0.05, 0.075, 0.1); nt = c(10, 20, 30, 40,50)
model.perf = c()						
for (i in 1:length(tc)){
 for (j in 1:length(lr)){
  for (k in 1:length(nt)){
 gbm1=gbm.step(data = Fire.1, gbm.x = 1:(ncol(Fire.Spread)-2),
						gbm.y = ncol(Fire.Spread)-1, 
						family = "bernoulli",
						tree.complexity = tc[i], 
						n.trees = nt[k], learning.rate = lr[j], bag.fraction = 0.75)
						
preds = predict.gbm(gbm1, Fire.Spread, n.trees = gbm1$gbm.call$best.trees, type="response")
dev = calc.deviance(obs=Fire.Spread$Status, pred=preds, calc.mean=TRUE)
	
d <- cbind(Fire.Spread$Status, preds)
pres <- d[d[,1]==1, 2]
abse <- d[d[,1]==0, 2]
e <- evaluate(p=pres, a=abse)
model.perf.t = c(tc[i],lr[j],nt[k],dev, e@auc, gbm1$gbm.call$best.trees)
model.perf = rbind(model.perf, model.perf.t)

print(paste("Finish Fitting tree complexity = ", tc[i], "; learning.rate = ", lr[j]," n.trees = ", nt[k], " at ", format(Sys.time(), "%a %b %d %X %Y"), sep = " ") )
  }
 }
}
write.csv(model.perf, "model.perf.spread.csv")

#fit a fire spread model using best BRT setting
gbm.fire.spread=gbm.step(data =Fire.Spread, gbm.x = 1:(ncol(Fire.Spread)-2),
						gbm.y = ncol(Fire.Spread)-1, 
						family = "bernoulli",
						tree.complexity = 3, 
						n.trees = 40, learning.rate = 0.075, bag.fraction = 0.75)

#predicting fire spread probability suface						
p.spread <- predict(predictors,                                        #a stack of predictors
	    gbm.fire.spread,                                       #fire occurrece model
            n.trees=gbm.fire.spread$gbm.call$best.trees,           #number of trees
	   type="response")

