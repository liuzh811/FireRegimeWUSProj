###################       9/10/2015            ###############################
##########             Fire simulation model            ######################

#load libraries
library(gbm)
library(dismo)
library("ncdf")
library("raster")
library("maptools")
library("rgdal")
library("maps")
library("rgeos") 
library("sp")
library("shapefiles")

setwd(".\\data")

#load fire size model
load("gbm.fs.rda") 
Residuals =gbm.fs$residuals
Residuals.mean = mean(Residuals)
Residuals.sd = sd(Residuals)

#read environmental data
source("Read.environmental.variable2.brt.r")

#read climate data
source("StackFutureClimate.brt.r")

### define some functions
#a fire spread function
FireSp3 <- function(row.n, col.n, size, p1)  ##row.n, col.n in cells, size in ha, p1 is the burned probability matrix
{
  ##convert fire size into numbers of cells
  cell.n = floor(size*0.01)
  #1 spread begins
  #1.1 produce an accumulative travel cost surface
  #1.1.1 producing distance to fire source map
  p2 = p1
  for (i in 1:nrow(p1)){ 
    for (j in 1:ncol(p1)){ 
      p2[i,j] = sqrt((i-row.n)*(i-row.n)+(j-col.n)*(j-col.n))
    }
  }
  #1.1.2 producing accumulative travel cost surface
  BP.cost = p2/p1
  #1.2 produce an fire shape
  Breakpoint = 0
  while(cell.n - sum(rowSums(FireSP))>1) { #do until the difference between pre-defined fire size and simulated fire size < 2 cells
    Breakpoint = Breakpoint+1
    FireSP[BP.cost<Breakpoint] = 1
  }
  return(FireSP) ##return a matrix
}

#modified fire spread funciton for edges
FireSp4 <- function(row.n, col.n, size, nc = 1683, nr = 2174)  ##row.n, col.n in cells, size in ha, p1 is the burned probability matrix
{
  ##convert fire size into numbers of cells
  cell.n = size*0.01
  R = floor(sqrt(cell.n)/2)
   if ((col.n-R)<0)
     col.n = col.n+R
	  else if ((col.n+R)>nc)
        col.n = col.n-R
        else if ((row.n-R)<0)
		 row.n = row.n+R
		  else if ((row.n+R)>nr)
		  row.n = row.n-R
  else
  FireSP[(row.n-R):(row.n+R),(col.n-R):(col.n+R)] = 1
  return(FireSP) ##return a matrix
}

#sample the fire occurrence date
SAMPLE = function(x){
if (length(which(x>=40)>0)) {
 P1 = which(x>=40)
 P = P1[sample(1:length(P1), 1)]
 } else 
 if (length(which(x>=30 & x<40)>0)) {
 P1 = which(x>=30 & x<40)
 P = P1[sample(1:length(P1), 1)]
  } else 
  if (length(which(x>=20 & x<30)>0)) {
  P1 = which(x>=20 & x<30)
  P = P1[sample(1:length(P1), 1)]
 } else 
	 P1 = which(x>=1 & x<20)
     P = P1[sample(1:length(P1), 1)]
return(P)
}

#record whether >45 days requirement is met 
SAMPLE2 = function(x){
if (length(which(x>=45)>0)) {
 P1 = length(which(x>=45))
 } else 
  P1 = 0
return(P1)
}

# read future fire occurrence probability map
proj.eco = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
proj.geo = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"

p.occ = raster("p.occ.gldl.brt.asc")
p.burn = raster("p.burn.future.asc")
projection(p.occ) <- proj.eco
p.occ = crop(p.occ, extent(mask))
extent(p.occ) <- extent(mask)
projection(p.occ) <- projection(mask)
p.occ = p.occ*mask

p.burn = crop(p.burn, extent(mask))
extent(p.burn) <- extent(mask)
projection(p.burn) <- projection(mask)
p.burn = p.burn*mask


FP1 = p.occ ##produce a empty raster for burned probability initialization
FP1[FP1>-90] = 0
FP2 = as.matrix(FP1)

## Read historical occurrence probability to calculate future fire ##
p.occ.c = raster("p.occ.brt.asc")
projection(p.occ.c) <- proj.eco
p.occ.c = crop(p.occ.c, extent(mask))
extent(p.occ.c) <- extent(mask)
projection(p.occ.c) <- projection(mask)
p.occ.c = p.occ.c*mask
## END of read historical occurrence probability

Ratio = p.occ/p.occ.c
Mean = cellStats(Ratio, stat='mean')
NumFire = (6193/27)*Mean  ##give fire number here, 119 for northern rockies
#"predictors.future" is the future variables stack
###simulate fire occurrence
nr = nrow(p.occ)
nc = ncol(p.occ)

p1 = as.matrix(p.burn) ##p1 is burn probability, needed for fire spread
M30 = matrix(0, nrow = 61,ncol = 61)
M302 = matrix(0, nrow = 61,ncol = 61)
###############################################################################################
#########The following code need loop for multiple simulation##############################
###############################################################################################
Pname = c("FireOccDate1", "PMn","PMx", "PAo","P90Mn", "P90Ao","PPMn", "PPAo", "PWMn","PWAo", "PP2GMn", "PP2GAo", "PP2Mn", "PP2Ao")
Tname = c("FireOccDate1", "TMn","TMx", "TAo","T90Mn", "T90Ao","TPMn", "TPAo", "TWMn","TWAo", "TP2GMn", "TP2GAo", "TP2Mn", "TP2Ao")
Wname = c("FireOccDate1", "WMn","WMx")
ln = list(Pname,Tname,Wname)

FIRE.BURN.LIST.CURRENT = list()
FIRE.SIZE = c()
FIREDATE =c()
FIREDATEYr = c()
DATEmeet = c()  #record how often >40 days requirement is not met

NofSim = 0
repeat {
NofSim = NofSim+1

##Single Fire Simulate BEGINS from here
FireIg = matrix(data = 0, nrow = nr, ncol = nc) #create a matrix with 0
FireLoc = c() #record fire location
FireIgAtt = c() # record the ith trail
N = 0
repeat {
N = N+1
#select a cell ramdomly
nr.t = sample(c(1:nr),1)
nc.t = sample(c(1:nc),1)
p1.t = p1[nr.t,nc.t]
if (!is.na(p1.t)){
#generate a random number from 0.3-1, in this case, probability < 0.2 will never have a big fire
r1 <- runif(1, 0.4, 1)
#compare
if (p1.t>r1){ 
FireIg[nr.t,nc.t] = FireIg[nr.t,nc.t]+1 ##if pass bernorli trail, the correspoding location add 1
FireLoc = rbind(FireLoc, c(nr.t,nc.t))
FireIgAtt = c(FireIgAtt,N)
}
sum1 = sum(FireIg)
if(sum1>NumFire){break}
}
}
print(paste("Simulating fire occurrece: Simulation No = ", NofSim, " at", format(Sys.time(), "%a %b %d %X %Y"), sep = " ") )

FireLoc = cbind(FireLoc)
colnames(FireLoc) = c("nrow", "ncol")

x.cor = FireLoc[,2]*1000+xmin(p.occ)
y.cor = ymax(p.occ) - FireLoc[,1]*1000
FireLoc2 = cbind(x.cor, y.cor)

##change the meter to degress in FireLoc2
FireLoc3 <- SpatialPoints(FireLoc2)
proj4string(FireLoc3) = proj.eco
FireLoc3.geo = spTransform(FireLoc3, CRS(paste("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")))
FireLoc3 = FireLoc3.geo

########################### Predict fire size from here #######################
########      Predict fire size: step 1, randomly get a fire date    ##########               
#step 1: extract climate series data

PrcpMean.t1 = extract(B.prec, FireLoc3)  ###slow here
PrcpMean.t1 = as.data.frame(PrcpMean.t1)

TmaxMean.t1 = extract(B.tmax, FireLoc3)
TmaxMean.t1 = as.data.frame(TmaxMean.t1)

#step 2: compute the quantile data
##compute 10 quantile for precentile

Qutile = function(x){quantile(x, probs = 0.1, na.rm = TRUE)}
PrcpQu10 = apply(PrcpMean.t1, 1, Qutile)

PrcpMean.t2 = PrcpMean.t1
PrcpMean.t2[] = 0

Repl = function(x){
  Day = which(x[1:(length(x)-1)]<=x[length(x)]) ##the last collum is the threshold,
  x = x[-length(x)] 
  x[Day] = 1
  x[-Day] = 0
  return(x)
}

PrcpMean.t2 = apply(cbind(PrcpMean.t1,PrcpQu10) , 1, Repl) #give 1 and 0 for each cell
PrcpMean.t2 = t(PrcpMean.t2)

##based on emperical data, the large occurs at 95 percentile of maximum temperature, 
##compute 95 quantile for temperature
## for future climate, we assume the climate > 90 percentile will be suitable for large fires
Qutile = function(x){quantile(x, probs = 0.9, na.rm = TRUE)}
TmaxQu90 = apply(TmaxMean.t1, 1, Qutile)

Repl = function(x){
  Day = which(x[1:(length(x)-1)]>=x[length(x)]) ##the last collum is the threshold,
  x = x[-length(x)] 
  x[Day] = 1
  x[-Day] = 0
  return(x)
}

TmaxMean.t2 = apply(cbind(TmaxMean.t1, TmaxQu90), 1, Repl) ##combine the temperature and threshold
TmaxMean.t2 = t(TmaxMean.t2)

#####step 3: cunstruct a climate matrix, if accumulated 45 day before each day is > 60, it means at least
#####we have 30 days with temperature =1 (> 90 percentile) and prec = 1 (< 10 percentile) 
#/ Climt = tmaxMean.t2+PrcpMean.t2 
Climt = TmaxMean.t2
Climt2 = Climt
Climt2[] = 0

for (jj in 366:ncol(Climt)){
  tmp.matrix = Climt[,(jj-45):jj]
  Climt2[,jj] = apply(tmp.matrix, 1, sum)
  print(paste("Simulation Climate + Vegetation No = ", NofSim, ": finish extracting", jj," of ", ncol(Climt)," at ", format(Sys.time(), "%a %b %d %X %Y"), sep = " ") )
}

rm(Climt,TmaxMean.t2, PrcpMean.t2,TmaxQu90,PrcpQu10, jj)
### step 4: get a candidate date for each fires and then sample the date randomly
FireOccDate = apply(Climt2[,732:7300],1, SAMPLE)+731
FIREDATE = cbind(FIREDATE, FireOccDate%%365)
FIREDATEYr = cbind(FIREDATEYr, FireOccDate%/%365+2080)
DATEmeet = cbind(DATEmeet, apply(Climt2[,732:7300],1, SAMPLE2))
######end of edit at 6/21/2013 ###############

########      Predict fire size: step 2, calculate climate variables for each fire    ##########     
#########2.1   calculate precipation #############
CliVar1 = c("pr_","tmmx_","tmmn_","vs_")
CliVar = c("prec","tmax","tmin","vs")
dat = c("20C3M_19712000", "A1B_20812100")

MN.prec = extract(MN.prec.R, FireLoc3)
MN.prec_may2sep = extract(pr_mean_may2sep.R, FireLoc3)
PrcpMean = c()
PrcpM = c()
Prcpano = c()
Prcp90Mean = c()
Prcp90ano = c()
PrcpPreYMean = c()
PrcpPreYano = c()

PrcpPreYWMean = c() #previous winter
PrcpPreYWano = c()

PrcpPre2YGMean = c() #growing season
PrcpPre2YGano = c()

PrcpPre2YAMean = c() #ANNUAL
PrcpPre2YAano = c()

for (Fid in 1:nrow(FireLoc2)){ ###for each simulated fire
  #extract the observed mean climate variables for that fire
  MN.prec1 = MN.prec[Fid]
  #For fire spreading fire days
  PrcpMean.t = as.numeric(PrcpMean.t1[Fid, FireOccDate[Fid]:(FireOccDate[Fid]+30)])
  PrcpMean.t[PrcpMean.t>quantile(PrcpMean.t, probs = 0.9999,na.rm = TRUE)] = NA  
  PrcpMean.t11 = mean(as.matrix(PrcpMean.t), na.rm = T) ##Compute the mean value
  PrcpM.t1 = max(as.matrix(PrcpMean.t), na.rm = T) ##Compute the max value
  PrcpMean = c(PrcpMean, PrcpMean.t11)
  PrcpM = c(PrcpM, PrcpM.t1)
  Prcpano.t1 = PrcpMean.t11 - MN.prec1 ##Compute the anormally value
  Prcpano = c(Prcpano, Prcpano.t1)
  
  #Calculate 90 day before fire and anomaly
  PrcpMean.t = as.numeric(PrcpMean.t1[Fid, (FireOccDate[Fid]-90):FireOccDate[Fid]])
  PrcpMean.t[PrcpMean.t>quantile(PrcpMean.t, probs = 0.9999,na.rm = TRUE)] = NA
  Prcp90Mean.t11 = mean(as.matrix(PrcpMean.t), na.rm = T) ##Compute the mean value
  Prcp90ano.t1 = Prcp90Mean.t11 - MN.prec1 ##Compute the anomaly
  Prcp90ano = c(Prcp90ano, Prcp90ano.t1)
  Prcp90Mean = c(Prcp90Mean, Prcp90Mean.t11)
    
  #Calculate previous growing season
  PreY = FireOccDate[Fid]%/%365 #select previos year
  PrcpMean.t = as.numeric(PrcpMean.t1[Fid, (PreY-1)*365+121:274])
  PrcpMean.t[PrcpMean.t>quantile(PrcpMean.t, probs = 0.9999,na.rm = TRUE)] = NA  
  PrcpPreYMean.t11 = mean(PrcpMean.t, na.rm = TRUE) ##Compute the mean value
  PrcpPreYano.t1 = PrcpPreYMean.t11 - MN.prec_may2sep[Fid] ##Compute the anomaly
  PrcpPreYano = c(PrcpPreYano, PrcpPreYano.t1)
  PrcpPreYMean = c(PrcpPreYMean, PrcpPreYMean.t11)
  
  #Calculate previous winter season [Nov 1st - Mar 1st]
  PreY = FireOccDate[Fid]%/%365 #select previos year
  PrcpMean.t = as.numeric(PrcpMean.t1[Fid, (PreY-1)*365+305:425])
  PrcpMean.t[PrcpMean.t>quantile(PrcpMean.t, probs = 0.9999,na.rm = TRUE)] = NA  
  PrcpPreYWMean.t11 = mean(PrcpMean.t, na.rm = T) ##Compute the mean value
  PrcpPreYWano.t1 = PrcpPreYMean.t11 - MN.prec[Fid] ##Compute the anomaly
  PrcpPreYWano = c(PrcpPreYWano, PrcpPreYWano.t1)
  PrcpPreYWMean = c(PrcpPreYWMean, PrcpPreYWMean.t11)
  
  #Calculate previous two year season growing and annual 
  PreY = FireOccDate[Fid]%/%365-1 #select previos year
  PrcpMean.t = as.numeric(PrcpMean.t1[Fid, (PreY-1)*365+121:274])
  PrcpMean.t[PrcpMean.t>quantile(PrcpMean.t, probs = 0.9999,na.rm = TRUE)] = NA  
  PrcpPre2YGMean.t11 = mean(PrcpMean.t, na.rm = T) ##Compute the mean value
  PrcpPre2YGano.t1 = PrcpPre2YGMean.t11 - MN.prec_may2sep[Fid] ##Compute the anomaly
  PrcpPre2YGano = c(PrcpPre2YGano, PrcpPre2YGano.t1)
  PrcpPre2YGMean = c(PrcpPre2YGMean, PrcpPre2YGMean.t11)
  
  #annual
  PrcpMean.t = as.numeric(PrcpMean.t1[Fid, (PreY-1)*365+1:365])
  PrcpMean.t[PrcpMean.t>quantile(PrcpMean.t, probs = 0.9999,na.rm = TRUE)] = NA  
  PrcpPre2YAMean.t11 = mean(PrcpMean.t, na.rm = T) ##Compute the mean value
  PrcpPre2YAano.t1 = PrcpPre2YAMean.t11 - MN.prec[Fid] ##Compute the anomaly
  PrcpPre2YAano = c(PrcpPre2YAano, PrcpPre2YAano.t1)
  PrcpPre2YAMean = c(PrcpPre2YAMean, PrcpPre2YAMean.t11)
  
  
  
  print(paste("GFDL Climate + Vegetation: extracting preception: Simulation No = ", NofSim, " for ", Fid," of ", nrow(FireLoc2)," at ", format(Sys.time(), "%a %b %d %X %Y"), sep = " ") )
}
Prcp = cbind(FireOccDate, PrcpMean,PrcpM, Prcpano,Prcp90Mean, Prcp90ano,PrcpPreYMean, PrcpPreYano, PrcpPreYWMean, PrcpPreYWano, PrcpPre2YGMean,PrcpPre2YGano, PrcpPre2YAMean, PrcpPre2YAano)
colnames(Prcp) = ln[[1]]

###############################  calculate maximum temperature  ###############################
MN.tmax = extract(MN.tmax.R, FireLoc3)
MN.tmax_may2sep = extract(tmax_mean_may2sep.R, FireLoc3)

TmaxMean = c()
TmaxM = c()
Tmaxano = c()
Tmax90Mean = c()
Tmax90ano = c()
TmaxPreYMean = c()
TmaxPreYano = c()

TmaxPreYWMean = c() #previous winter
TmaxPreYWano = c()

TmaxPre2YGMean = c() #growing season
TmaxPre2YGano = c()

TmaxPre2YAMean = c() #ANNUAL
TmaxPre2YAano = c()

for (Fid in 1:nrow(FireLoc2)){ ###for each simulated fire
  #extract the observed mean climate variables for that fire
  MN.tmax1 = MN.tmax[Fid]
  #For fire spreading fire days
  TmaxMean.t = as.numeric(TmaxMean.t1[Fid, FireOccDate[Fid]:(FireOccDate[Fid]+30)])
  TmaxMean.t[TmaxMean.t>quantile(TmaxMean.t, probs = 0.9999,na.rm = TRUE)] = NA  
  TmaxMean.t11 = mean(as.matrix(TmaxMean.t), na.rm = T) ##Compute the mean value
  TmaxM.t1 = max(as.matrix(TmaxMean.t), na.rm = T) ##Compute the max value
  TmaxMean = c(TmaxMean, TmaxMean.t11)
  TmaxM = c(TmaxM, TmaxM.t1)
  Tmaxano.t1 = TmaxMean.t11 - MN.tmax1 ##Compute the anormally value
  Tmaxano = c(Tmaxano, Tmaxano.t1)
  
  #Calculate 90 day before fire and anomaly
  TmaxMean.t = as.numeric(TmaxMean.t1[Fid, (FireOccDate[Fid]-90):FireOccDate[Fid]])
  TmaxMean.t[TmaxMean.t>quantile(TmaxMean.t, probs = 0.9999,na.rm = TRUE)] = NA  
  Tmax90Mean.t11 = mean(as.matrix(TmaxMean.t), na.rm = T) ##Compute the mean value
  Tmax90ano.t1 = Tmax90Mean.t11 - MN.tmax1 ##Compute the anomaly
  Tmax90ano = c(Tmax90ano, Tmax90ano.t1)
  Tmax90Mean = c(Tmax90Mean, Tmax90Mean.t11)
    
  #Calculate previous growing season
  PreY = FireOccDate[Fid]%/%365 #select previos year
  TmaxMean.t = as.numeric(TmaxMean.t1[Fid, (PreY-1)*365+121:274])
  TmaxMean.t[TmaxMean.t>quantile(TmaxMean.t, probs = 0.9999,na.rm = TRUE)] = NA  
  TmaxPreYMean.t11 = mean(TmaxMean.t, na.rm = TRUE) ##Compute the mean value
  TmaxPreYano.t1 = TmaxPreYMean.t11 - MN.tmax_may2sep[Fid] ##Compute the anomaly
  TmaxPreYano = c(TmaxPreYano, TmaxPreYano.t1)
  TmaxPreYMean = c(TmaxPreYMean, TmaxPreYMean.t11)
  
  #Calculate previous winter season [Nov 1st - Mar 1st]
  PreY = FireOccDate[Fid]%/%365 #select previos year
  TmaxMean.t = as.numeric(TmaxMean.t1[Fid, (PreY-1)*365+305:425])
  TmaxMean.t[TmaxMean.t>quantile(TmaxMean.t, probs = 0.9999,na.rm = TRUE)] = NA  
  TmaxPreYWMean.t11 = mean(TmaxMean.t, na.rm = T) ##Compute the mean value
  TmaxPreYWano.t1 = TmaxPreYMean.t11 - MN.tmax[Fid] ##Compute the anomaly
  TmaxPreYWano = c(TmaxPreYWano, TmaxPreYWano.t1)
  TmaxPreYWMean = c(TmaxPreYWMean, TmaxPreYWMean.t11)
  
  #Calculate previous two year season growing and annual 
  PreY = FireOccDate[Fid]%/%365-1 #select previos year
  TmaxMean.t = as.numeric(TmaxMean.t1[Fid, (PreY-1)*365+121:274])
  TmaxMean.t[TmaxMean.t>quantile(TmaxMean.t, probs = 0.9999,na.rm = TRUE)] = NA  
  TmaxPre2YGMean.t11 = mean(TmaxMean.t, na.rm = T) ##Compute the mean value
  TmaxPre2YGano.t1 = TmaxPre2YGMean.t11 - MN.tmax_may2sep[Fid] ##Compute the anomaly
  TmaxPre2YGano = c(TmaxPre2YGano, TmaxPre2YGano.t1)
  TmaxPre2YGMean = c(TmaxPre2YGMean, TmaxPre2YGMean.t11)
  
  #annual
  TmaxMean.t = as.numeric(TmaxMean.t1[Fid, (PreY-1)*365+1:365])
  TmaxMean.t[TmaxMean.t>quantile(TmaxMean.t, probs = 0.9999,na.rm = TRUE)] = NA  
  TmaxPre2YAMean.t11 = mean(TmaxMean.t, na.rm = T) ##Compute the mean value
  TmaxPre2YAano.t1 = TmaxPre2YAMean.t11 - MN.tmax[Fid] ##Compute the anomaly
  TmaxPre2YAano = c(TmaxPre2YAano, TmaxPre2YAano.t1)
  TmaxPre2YAMean = c(TmaxPre2YAMean, TmaxPre2YAMean.t11)
  
   
  print(paste("GFDL Climate + Vegetation: extracting preception: Simulation No = ", NofSim, " for ", Fid," of ", nrow(FireLoc2)," at ", format(Sys.time(), "%a %b %d %X %Y"), sep = " ") )
}
Tmax = cbind(FireOccDate, TmaxMean,TmaxM, Tmaxano,Tmax90Mean, Tmax90ano,TmaxPreYMean, TmaxPreYano, TmaxPreYWMean, TmaxPreYWano, TmaxPre2YGMean,TmaxPre2YGano, TmaxPre2YAMean, TmaxPre2YAano)
colnames(Tmax) = ln[[2]]


########################  calculating wind  ###################################
 WindMean.t1 = extract(B.vs, FireLoc3)
 WindMean.t1 = as.data.frame(WindMean.t1)
 
 WindMean = c()
 WindM = c()

for (Fid in 1:nrow(FireLoc2)){ ###for each simulated fire
  #ramdon sample a fire occurrence date
  #extract the observed mean climate variables for that fire
  #For fire spreading fire days
  WindMean.t = as.numeric(WindMean.t1[Fid, FireOccDate[Fid]:(FireOccDate[Fid]+30)])
  WindMean.t[WindMean.t>quantile(WindMean.t, probs = 0.9999,na.rm = TRUE)] = NA
  WindMean.t11 = mean(as.matrix(WindMean.t), na.rm = T) ##Compute the mean value
  WindM.t1 = max(as.matrix(WindMean.t), na.rm = T) ##Compute the max value
  WindMean = c(WindMean, WindMean.t11)
  WindM = c(WindM, WindM.t1)

  print(paste("GFDL Climate + Vegetation: extracting wind: Simulation No = ", NofSim, " for ", Fid," of ", nrow(FireLoc2)," at ", format(Sys.time(), "%a %b %d %X %Y"), sep = " ") )
}

Wind = cbind(FireOccDate, WindMean,WindM)
colnames(Wind) = ln[[3]]

########      Predict fire size: step 3, sample environmental variables for each fire,     ##########     
FireP.dataP = extract(predictors.future, FireLoc2)  #using the current predictors
#/ ##combine the environmental data and climate data
#/ dat.new = cbind(FireP.dataP,tmax1,tmin1, wind1, prec1)
dat.new = cbind(FireP.dataP,Tmax,Prcp,Wind)
dat.new = as.data.frame(dat.new)

#### Predict fire size: step 4, predict fire size based on gbm model     
dat.new$FireSize = predict.gbm(gbm.fs, dat.new,n.trees = gbm.fs$gbm.call$best.trees, type="response")+rnorm(nrow(FireLoc2),mean = Residuals.mean, sd = Residuals.sd)
FireLoc2 = cbind(FireLoc2, dat.new$FireSize)
colnames(FireLoc2) = c("x", "y", "size")
FireLoc2 = as.data.frame(FireLoc2)

#################  simulate fire spread here  #####################
FP = FP2 #initialize a 0 value matrix
FireLoc = cbind(FireLoc,exp(dat.new$FireSize))
colnames(FireLoc) = c("nrow", "ncol", "size")
FireLoc = subset(FireLoc, !is.na(FireLoc[,3]))
FireLoc[,3][FireLoc[,3]>263512] = 263512

for (i in 1:nrow(FireLoc)){
if ((FireLoc[i,1]-31)<0 |(FireLoc[i,1]+31)>nr|(FireLoc[i,2]-31)<0|(FireLoc[i,2]+31)>nc) 
{FireSP = FP2
FP.t = FireSp4(FireLoc[i,1],FireLoc[i,2],FireLoc[i,3])
FP = FP + FP.t}
else {M30 = p1[(FireLoc[i,1]-30):(FireLoc[i,1]+30),(FireLoc[i,2]-30):(FireLoc[i,2]+30)]
FireSP = M302
FP.t = FireSp3(31,31,FireLoc[i,3],M30)
FP.t1 = FP2
FP.t1[(FireLoc[i,1]-30):(FireLoc[i,1]+30),(FireLoc[i,2]-30):(FireLoc[i,2]+30)] = FP.t
FP = FP + FP.t1}
print(paste("GFDL Climate + Vegetation: Spreading:: BRT : Simulation No = ", NofSim, " for ", i,"th fire of ", nrow(FireLoc)," at ", format(Sys.time(), "%a %b %d %X %Y"), sep = " ") )
}
#################  simulate fire spread ends here  #####################
##Single Simulate ENDS from here


FIRE.SIZE = cbind(FIRE.SIZE, FireLoc2$size)
FIRE.BURN.LIST.CURRENT[[NofSim]] <- FP

###end of gam simulation
rm(list = c('FP.t1','FireIg','TmaxMean.t1', 'PrcpMean.t1','Climt2','WindMean.t1')) #free up some memory

########################################################################
if(NofSim>=500){break}
}

save.image("run1.RData")

###############################################################################################
############               Entire simulate ends here             ##############################
###############################################################################################
#for BRT
FP = FP2 #initialize a 0 value matrix
for (i in 1:length(FIRE.BURN.LIST.CURRENT)){
  FP = FP+FIRE.BURN.LIST.CURRENT[[i]]
  print(paste("GFDL: Climate+Vegetation: Finish extracting BRT", i, "th fire"," of ", length(FIRE.BURN.LIST.CURRENT), " at ", format(Sys.time(), "%a %b %d %X %Y"), sep = " ") )
}

FP = raster(FP)
extent(FP)<-extent(p.occ)
projection(FP)<-projection(p.occ)

writeRaster(FP, filename="run1_brt.asc", format="ascii", overwrite=TRUE)   

write.csv(FIRE.SIZE, "run1_FireSize_brt.csv")
write.csv(FIREDATE, "run1_FireDate.csv")
write.csv(FIREDATEYr, "run1_FireYr.csv")

