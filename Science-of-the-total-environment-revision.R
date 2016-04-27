#final results codes:D:\dataset\MTBSdata\FireProjection\WestUSbrt2\PostProcess_091514.r

# Science of the total environment, revision

setwd("D:\\dataset\\MTBSdata\\FireProjection")

library(rgdal)
library(raster)
library(rasterVis)

# read into shapefiles

proj.geo = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0 "

EPA = readOGR(dsn="D:\\dataset\\ClimateData\\Regions",layer="EpaWest3")
epa_geo = spTransform(EPA, CRS(proj.geo))

#for temperature
# read into climate change fils
temp.name = list.files(path = ".\\revision090315", pattern = "*tmean_14_2070_2099.asc$")
name = data.frame(gcm = unlist(lapply(temp.name, function(x) substr(x, 30,40))),
                  co2 = unlist(lapply(temp.name, function(x) substr(x, nchar(x)-25,nchar(x)-23))))        
                  
temp = stack(paste("D:\\dataset\\MTBSdata\\FireProjection\\revision090315\\", temp.name, sep = ""))
temp = crop(temp, epa_geo)

temp.mean = apply(as.matrix(temp),2,mean, na.rm = TRUE)
temp.mean = data.frame(temp = temp.mean, name)

#precp
pecp.name = list.files(path = ".\\revision090315", pattern = "*pptPct_14_2070_2099.asc$")
name1 = data.frame(gcm = unlist(lapply(pecp.name, function(x) substr(x, 30,40))),
                  co2 = unlist(lapply(pecp.name, function(x) substr(x, nchar(x)-26,nchar(x)-24))))        

pecp = stack(paste("D:\\dataset\\MTBSdata\\FireProjection\\revision090315\\", pecp.name, sep = ""))
pecp = crop(pecp, epa_geo)

pecp.mean = apply(as.matrix(pecp),2,mean, na.rm = TRUE)
pecp.mean = data.frame(pecp = pecp.mean, name1)


##########################used temperature in this analsysis

setwd('D:\\dataset\\MTBSdata\\FireProjection\\Gisdata')

Tavg.c= raster("Tavg.c.asc")
PPTavg.c= raster("PPTavg.c.asc")
TavgCNRM.c= raster("TavgCNRM.c.asc")
PPTavgCNRM.c= raster("PPTavgCNRM.c.asc")

cellStats(Tavg, stat='mean'):2.691247
cellStats(PPTavg, stat='mean') : 1.417323

cellStats(Tavg.c, stat='mean'):3.67
cellStats(TavgCNRM.c, stat='mean'):3.33
cellStats(PPTavg.c, stat='mean'):-0.06997957 (-25.5 annual)
cellStats(PPTavgCNRM.c, stat='mean'):0.08062955 (29.42979)
1.49*365*0.05
setwd("D:\\dataset\\MTBSdata\\FireProjection")

png(".\\revision090315\\CliamteChange2.png",height = 2000, width = 3000, res = 300, units = "px")

par(mfrow=c(1,2),mar=c(1,5,1,1))

boxplot(temp.mean$temp-3, ylab = "Temperature Change (degree)", cex.axis = 1.5, cex.lab = 1.5)
text(0.6,7,labels="a)",cex=2)
abline(h =3.67, lwd = 2, col = "red")
abline(h =3.33, lwd = 2, col = "blue")
abline(h =temp.mean[which(temp.mean$gcm == "gfdl_cm2_1." & temp.mean$co2 == "_a2"),1]-3, lwd = 2, lty = 2,col = "red")
abline(h =temp.mean[which(temp.mean$gcm == "gfdl_cm2_1." & temp.mean$co2 == "_b1"),1]-3, lwd = 2, lty = 3,col = "red")

abline(h =temp.mean[which(temp.mean$gcm == "cnrm_cm3.1_" & temp.mean$co2 == "_a2"),1]-3, lwd = 2, lty = 2,col = "blue")
abline(h =temp.mean[which(temp.mean$gcm == "cnrm_cm3.1_" & temp.mean$co2 == "_b1"),1]-3, lwd = 2, lty = 3,col = "blue")

#abline(h =temp.mean[which(temp.mean$gcm == "cnrm_cm3.1_" & temp.mean$co2 == "a1b"),1], lwd = 2, col = "blue")

boxplot(pecp.mean$pecp, ylab = "Annal Rainfall Change (%)", cex.axis = 1.5, cex.lab = 1.5)
text(0.6,13,labels="b)",cex=2)
abline(h =-4.9, lwd = 2, col = "red")
abline(h =5.7, lwd = 2, col = "blue")

#abline(h =pecp.mean[which(pecp.mean$gcm == "gfdl_cm2_1." & pecp.mean$co2 == "a1b"),1]+10, lwd = 2, lty = 1,col = "red")
#abline(h =pecp.mean[which(pecp.mean$gcm == "cnrm_cm3.1_" & pecp.mean$co2 == "a1b"),1]+5, lwd = 2, lty = 1,col = "blue")

abline(h =pecp.mean[which(pecp.mean$gcm == "gfdl_cm2_1." & pecp.mean$co2 == "_a2"),1]+10, lwd = 2, lty = 2,col = "red")
abline(h =pecp.mean[which(pecp.mean$gcm == "cnrm_cm3.1_" & pecp.mean$co2 == "_a2"),1]+5, lwd = 2, lty = 3,col = "red")

abline(h =pecp.mean[which(pecp.mean$gcm == "gfdl_cm2_1." & pecp.mean$co2 == "_b1"),1]+10, lwd = 2, lty = 2,col = "blue")
abline(h =pecp.mean[which(pecp.mean$gcm == "cnrm_cm3.1_" & pecp.mean$co2 == "_b1"),1]+5, lwd = 2, lty = 3,col = "blue")

legend("topright", inset=0.05,        
       lty = c(1,1,2,2,3,3), 
       col =c("red","blue","red","blue","red","blue"), 
       lwd=2, 
       cex = 1.5, 
       text.col = c("red","blue","red","blue","red","blue"),  
       c("GFDL A1B", "CNRM A1B", "GFDL A2", "CNRM A2", "GFDL B1", "CNRM B1"),
       horiz=F, box.col = "white")

#abline(h =-0.06997957*100/1.417323, lwd = 2, col = "red")
#abline(h =0.08062955*100/1.417323, lwd = 2, col = "blue")

dev.off()

summary(temp.mean$temp-3)


#plot current climate variables
Tavg= raster("D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\TavgPROJ.asc")
PPTavg= raster("D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\PPTPROJ.asc")
Tavg59= raster("D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\TmaysepPROJ.asc")
PPTavg59= raster("D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\PPTmaysepPROJ.asc")


Tavg = crop(Tavg, Tavg.c)
PPTavg = crop(PPTavg, Tavg.c)
Tavg59 = crop(Tavg59, Tavg.c)
PPTavg59 = crop(PPTavg59, Tavg.c)


Tavg.c1 = !is.na(Tavg.c) 
Tavg.c1[Tavg.c1 != 1] = NA

extent(Tavg) <- extent(Tavg.c1)
extent(PPTavg) <- extent(Tavg.c1)
extent(Tavg59) <- extent(Tavg.c1)
extent(PPTavg59) <- extent(Tavg.c1)

Tavg = Tavg*Tavg.c1
PPTavg = PPTavg*Tavg.c1
Tavg59 = Tavg59*Tavg.c1
PPTavg59 = PPTavg59*Tavg.c1
projection(Tavg) <- projection(EPA2)


EPA2 = readOGR(dsn="D:\\dataset\\ClimateData\\Regions",layer="EpaWest3")
state = readOGR(dsn="D:\\dataset\\NationalAtlas\\statep010_nt00798.tar",layer="statep010_ables2_west")

library("maptools")
library("rgdal")
library("maps")

png(".\\revision090315\\CliamteInput.png",height = 3000, width = 3000, res = 300, units = "px")

par(mfrow=c(2,2),mar=c(3,3,3,1)+.1)
my.colors = colorRampPalette(c("blue", "white", "red"))

plot(Tavg, zlim=c(-10,30),col = my.colors(100), main = "",axes=F)
title(main = "Mean Temperature: Annual")
plot(state, lwd = 1.5, add = TRUE)
plot(EPA2, lwd = 1.5, add = TRUE)

plot(Tavg59, zlim=c(-10,30),col = my.colors(100), main = "",axes=F)
title(main = "Mean Temperature: May - Sep")
plot(state, lwd = 1.5, add = TRUE)
plot(EPA2, lwd = 1.5, add = TRUE)

plot(log10(PPTavg*365), zlim=c(1,4),col = my.colors(100), main = "",axes=F)
title(main = "Annual Precipitation in log10 scale")
plot(state, lwd = 1.5, add = TRUE)
plot(EPA2, lwd = 1.5, add = TRUE)

plot(log10(PPTavg59*154), zlim=c(1,4),col = my.colors(100), main = "",axes=F)
title(main = "May-Sep Precipitation in log10 scale")
plot(state, lwd = 1.5, add = TRUE)
plot(EPA2, lwd = 1.5, add = TRUE)

dev.off()



##calculate transition matrix 
BPS = raster('D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\bps14clas.tif')

BPS.Pred = raster('D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\BPS.Pred.asc')
BPS.Pred.future = raster('D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\BPS.Pred.future.asc')
BPS.Pred.future.CNRM = raster('D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\BPS.Pred.future.CNRM.asc')

BPS.Pred1 = BPS.Pred >= 3 & BPS.Pred <= 9
freq(BPS.Pred1)

BPS.Pred.future1 = BPS.Pred.future >= 3 & BPS.Pred.future <= 9
freq(BPS.Pred.future1)

BPS.Pred.future.CNRM1 = BPS.Pred.future.CNRM >= 3 & BPS.Pred.future.CNRM <= 9
freq(BPS.Pred.future.CNRM1)

(897233 - 829176)/829176

tmp = BPS == BPS.Pred
freq(tmp)
freq(BPS)


change.gfdl.df <- crosstab(BPS.Pred, BPS.Pred.future)
change.cnrm.df <- crosstab(BPS.Pred, BPS.Pred.future.CNRM)
names(change.gfdl.df)[1:2] <- c("Current", "GFDL")
names(change.cnrm.df)[1:2] <- c("Current", "CNRM")

# for GFDL scenarios
change.gfdl.df <- change.gfdl.df[is.na(change.gfdl.df$Current) == F & is.na(change.gfdl.df$GFDL) == F,]
change.gfdl.df <- matrix(change.gfdl.df$Freq, nrow=14, ncol=14)
change.gfdl.df = change.gfdl.df[-1,]
change.gfdl.df = change.gfdl.df[,-1]

shortnames = c("Gras","PCDF","InDF","PJW","PIPO","Salp","Mixd","Hdwd","CaCr","DeSc","MMsh","Sgbr","Shsp")
rownames(change.gfdl.df) <- shortnames
colnames(change.gfdl.df) <- shortnames

sum01 <- apply(change.gfdl.df, MARGIN=1, FUN=sum)
percchange <- 100 * sweep(change.gfdl.df, MARGIN=1, STATS=sum01, FUN="/")

require(RColorBrewer)
clascols = c(brewer.pal(9,"Reds")[c(3,6,9)],
             brewer.pal(9,"Greens")[c(3,6,9)],
             brewer.pal(9,"Blues")[c(3,6,9)],
             brewer.pal(9,"Oranges")[c(2,4,6,8)])



# for CNRM scenarios
change.cnrm.df <- change.cnrm.df[is.na(change.cnrm.df$Current) == F & is.na(change.cnrm.df$CNRM) == F,]
change.cnrm.df <- matrix(change.cnrm.df$Freq, nrow=14, ncol=14)
change.cnrm.df = change.cnrm.df[-1,]
change.cnrm.df = change.cnrm.df[,-1]

rownames(change.cnrm.df) <- shortnames
colnames(change.cnrm.df) <- shortnames

sum02 <- apply(change.cnrm.df, MARGIN=1, FUN=sum)
percchange2 <- 100 * sweep(change.cnrm.df, MARGIN=1, STATS=sum02, FUN="/")


write.csv(percchange,".\\revision090315\\veg.transition.gfdl.csv")
write.csv(percchange2,".\\revision090315\\veg.transition.cnrm.csv")

barplot(t(percchange), beside=TRUE, col = clascols, names.arg = shortnames,
        ylab = "Current to GFDL Change (%)",legend = colnames(percchange), 
        xlab = "Vegetation Type")

barplot(t(percchange2), beside=TRUE, col = clascols, names.arg = shortnames,
        ylab = "Current to CNRM Change (%)",
        xlab = "Vegetation Type")

#plot figures

#percchange = data.frame(read.csv(".\\revision090315\\veg.transition.gfdl.csv"))[,-1]
#percchange2 = data.frame(read.csv(".\\revision090315\\veg.transition.cnrm.csv"))[,-1]

library(ggplot2)
library(reshape2)

percchange_long = data.frame(melt(percchange, id.vars=shortnames), scenario = "GFDL")
percchange2_long = data.frame(melt(percchange2, id.vars=shortnames), scenario = "CNRM")

dat_long = rbind(percchange_long, percchange2_long)
dat_long2 = dat_long[which(dat_long$value > 10), ]

ggplot(data=dat_long2, aes(x=Var1, y=value, fill=Var2)) +
  geom_bar(stat="identity") + 
  facet_grid(Var2 ~ scenario) + 
  scale_fill_discrete(breaks=shortnames,
                      name="",
                      labels=shortnames)+
  theme(legend.text = element_text(size = 18))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=18),axis.text.x  = element_text(colour="black",size=18))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),axis.text.y  = element_text(colour="black",size=18))+
  xlab("Current Vegetation") + ylab("Percentage of future vegetation") +
  theme(strip.text.x = element_text(size=18), strip.text.y = element_text(size=18))

ggsave(".\\revision090315\\VegTransition.png", width = 20, height = 15, units = "in")


##################################################################################
################## not use from here ####################################
##there are too many classes for vegetation, let's combine them together
shortnames = c("Gras","PCDF","InDF","PJW","PIPO","Salp","Mixd","Hdwd","CaCr","DeSc","MMsh","Sgbr","Shsp")

oldclas <- unique(BPS.Pred)
newclas <- c(0, 1, 2, 2, 2, 2, 2, 2, 2, 3, 4,4,4,4)
rclastab.df <- data.frame(oldclas, newclas)

BPS.Pred_rc = subs(BPS.Pred, rclastab.df) 
BPS.Pred.future_rc = subs(BPS.Pred.future, rclastab.df) 
BPS.Pred.future.CNRM_rc = subs(BPS.Pred.future.CNRM, rclastab.df) 


change.gfdl.df <- crosstab(BPS.Pred_rc, BPS.Pred.future_rc)
change.cnrm.df <- crosstab(BPS.Pred_rc, BPS.Pred.future.CNRM_rc)
names(change.gfdl.df)[1:2] <- c("Current", "GFDL")
names(change.cnrm.df)[1:2] <- c("Current", "CNRM")

# for GFDL scenarios
change.gfdl.df <- change.gfdl.df[is.na(change.gfdl.df$Current) == F & is.na(change.gfdl.df$GFDL) == F,]
change.gfdl.df <- matrix(change.gfdl.df$Freq, nrow=5, ncol=5)
change.gfdl.df = change.gfdl.df[-1,]
change.gfdl.df = change.gfdl.df[,-1]

shortnames_new = c("Gras","Forest","CaCr","Shrubs")
rownames(change.gfdl.df) <- shortnames_new
colnames(change.gfdl.df) <- shortnames_new

sum01 <- apply(change.gfdl.df, MARGIN=1, FUN=sum)
percchange <- 100 * sweep(change.gfdl.df, MARGIN=1, STATS=sum01, FUN="/")


# for CNRM scenarios
change.cnrm.df <- change.cnrm.df[is.na(change.cnrm.df$Current) == F & is.na(change.cnrm.df$CNRM) == F,]
change.cnrm.df <- matrix(change.cnrm.df$Freq, nrow=5, ncol=5)
change.cnrm.df = change.cnrm.df[-1,]
change.cnrm.df = change.cnrm.df[,-1]

rownames(change.cnrm.df) <- shortnames_new
colnames(change.cnrm.df) <- shortnames_new

sum02 <- apply(change.cnrm.df, MARGIN=1, FUN=sum)
percchange2 <- 100 * sweep(change.cnrm.df, MARGIN=1, STATS=sum02, FUN="/")

#plot figures

library(ggplot2)
library(reshape2)

percchange_long = data.frame(melt(percchange, id.vars=shortnames), scenario = "GFDL")
percchange2_long = data.frame(melt(percchange2, id.vars=shortnames), scenario = "CNRM")

dat_long = rbind(percchange_long, percchange2_long)

ggplot(data=dat_long, aes(x=Var1, y=value, fill=Var2)) +
  geom_bar(stat="identity") + 
  facet_grid(Var2 ~ scenario) + 
  scale_fill_discrete(breaks=shortnames_new,
                      name="",
                      labels=shortnames_new)+
  theme(legend.text = element_text(size = 18))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=18),axis.text.x  = element_text(colour="black",size=18))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),axis.text.y  = element_text(colour="black",size=18))+
  xlab("From Current Vegetation") + ylab("%") +
  theme(strip.text.x = element_text(size=18), strip.text.y = element_text(size=18))

ggsave(".\\revision090315\\VegTransition_2.png", width = 12, height = 12, units = "in")

################## not use ends here ####################################
##################################################################################


##plot random forest predictors
getwd()

predictor = stack(".\\revision090315\\rf.veg.grd")


png(".\\revision090315\\rf.veg.predictor.png",height = 3500, width = 3500, res = 300, units = "px")

par(mfrow=c(3,4),mar=c(3,1,3,3)+.1)
my.colors = colorRampPalette(c("blue", "white", "red"))

plot(predictor[[1]], col = my.colors(100), main = "Mean January temperature (degree)",axes=F, box=FALSE)
plot(state, lwd = 1.5, add = TRUE)

plot(predictor[[2]], col = my.colors(100), main = "Mean July temperature (degree)",axes=F, box=FALSE)
plot(state, lwd = 1.5, add = TRUE)

plot(predictor[[3]], col = my.colors(100), main = "Mean May-Sep temperature (degree)",axes=F, box=FALSE)
plot(state, lwd = 1.5, add = TRUE)

plot(log10(predictor[[4]]*30), col = my.colors(100), main = "Mean January Precipitation (mm) \n (in log10 scale)",axes=F, box=FALSE)
plot(state, lwd = 1.5, add = TRUE)

plot(log10(predictor[[5]]*30), col = my.colors(100), main = "Mean July Precipitation(mm) \n (in log10 scale)",axes=F, box=FALSE)
plot(state, lwd = 1.5, add = TRUE)

plot(log10(predictor[[6]]*154), col = my.colors(100), main = "Mean May-Sep Precipitation(mm)\n (in log10 scale)",axes=F, box=FALSE)
plot(state, lwd = 1.5, add = TRUE)

plot(predictor[[7]], col = my.colors(100), main = "Elevation (m)",axes=F, box=FALSE)
plot(state, lwd = 1.5, add = TRUE)

plot(predictor[[8]], zlim=c(0,100), col = my.colors(100), main = "Slope",axes=F, box=FALSE)
plot(state, lwd = 1.5, add = TRUE)

plot(predictor[[9]], col = my.colors(100), main = "Aspect",axes=F, box=FALSE)
plot(state, lwd = 1.5, add = TRUE)

plot(predictor[[10]], col = my.colors(100), main = "Heated load index",axes=F, box=FALSE)
plot(state, lwd = 1.5, add = TRUE)

plot(predictor[[11]], col = my.colors(100), main = "terrain shape index",axes=F, box=FALSE)
plot(state, lwd = 1.5, add = TRUE)

dev.off()


###################doing validation for each subregions
##reproducing figure 3 for each regions

eco_names = unique(EPA$Levelii)
EPA_forest = EPA[which(EPA$Levelii == eco_names[1] | EPA$Levelii == eco_names[3] | EPA$Levelii == eco_names[6]),]
EPA_nonforest = EPA[-which(EPA$Levelii == eco_names[1] | EPA$Levelii == eco_names[3] | EPA$Levelii == eco_names[6]),]

plot(EPA_nonforest)
plot(EPA_forest)

################  for fire occurrence prob   #################

proj.geo = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
proj.eco = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#read into fire points
firep <- readOGR(dsn = "D:\\dataset\\MTBSdata\\NewMTBSdata100114\\point", layer = "mtbs_fod_pts_20141001_west")
firep = firep[firep$FIRE_YEAR==2011|firep$FIRE_YEAR==2012,]
firep = firep[firep$Fire_Type=="WF"|firep$Fire_Type=="WFU",]
firep.df = as.data.frame(firep@data)
firep.df$idd = 1:nrow(firep.df)

firep.loc <- as.data.frame(cbind(firep.df$LONG, firep.df$LAT,firep.df$idd ))
names(firep.loc) = c("x","y")
coordinates(firep.loc) <- ~x+y
proj4string(firep.loc) = proj.geo
firep.loc.eco = spTransform(firep.loc, CRS(proj.eco))

#split into fire and nonforest
firep.loc.eco_forest = firep.loc.eco[EPA_forest, ]
firep.loc.eco_nonforest = firep.loc.eco[EPA_nonforest, ]

#read into fire occ prob, and split into forest and non-forest
p.occ = raster("D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\pocc.asc")
p.occ_rc <- reclassify(p.occ, c(-Inf,0.125,1, 0.125,0.25,2, 0.25,0.375,3, 0.375,0.5,4,0.5,0.625, 5, 0.625, 0.75, 6, 0.75, 0.875, 7, 0.875, Inf,8))

EPA_forest.mask = rasterize(EPA_forest, p.occ_rc)


firep.loc.df = extract(p.occ_rc, firep.loc.eco)

firep.loc.df= table(firep.loc.df)

p.occ_rc.df = table(getValues(p.occ_rc))
relative = firep.loc.df/p.occ_rc.df

names.arg = c("0-0.125", "0.125-0.25","0.25-0.375","0.375-0.5", "0.5-0.625", "0.625-0.75","0.75-0.875", "0.875-1")
mp <- barplot(relative[c(1:5, 8,6,7)], axes = FALSE, axisnames = FALSE, xlab = "Relative Range", ylab = "Fire Occurrence Probability")
text(mp, par("usr")[3], labels = names.arg, srt = 30, adj = c(1.1,1.1), xpd = TRUE, cex=.9)
axis(2)



################  for historical burn rate   #################
##for historical burned rate 
firepatch <- readOGR(dsn = "D:\\dataset\\MTBSdata\\NewMTBSdata100114\\perimeter", layer = "mtbs_west_2011_2012")
firepatch = firepatch[firepatch$FireType=="WF"|firep$FireType=="WFU",]
firepatch = spTransform(firepatch, CRS(proj.eco))
firepatch@data$ID = 1:nrow(firepatch)

#p.burn = raster("D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\p.burn.asc")
##Historical fires
hfp1 = raster("D:\\dataset\\MTBSdata\\FireProjection\\WestUSbrt2\\historical_run1_brt.asc") #154 replicate
hfp2 = raster("D:\\dataset\\MTBSdata\\FireProjection\\WestUSbrt2\\historical_run2_brt.asc") #153 replicate
hfp3 = raster("D:\\dataset\\MTBSdata\\FireProjection\\WestUSbrt2\\historical_run3_brt.asc") #153 replicate
hfp4 = raster("D:\\dataset\\MTBSdata\\FireProjection\\WestUSbrt2\\historical_run4_brt.asc") #153 replicate
hfp5 = raster("D:\\dataset\\MTBSdata\\FireProjection\\WestUSbrt2\\historical_run5_brt.asc") #153 replicate

p.burn = hfp1+hfp2+hfp3+hfp4+hfp5
projection(p.burn)<-proj.eco

plot(p.burn)
plot(firepatch, add = TRUE)

BPS.Pred = raster('D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\BPS.Pred.asc')


Dat.sp = spsample(firepatch, 500, type = "random") 
Dat.sp2 = spsample(EPA, 5000, type = "random") 

Dat.sp.epaid <- over(Dat.sp, EPA)
Dat.sp2.epaid <- over(Dat.sp2, EPA)
Dat.sp.br = extract(p.burn, Dat.sp)
Dat.sp2.br = extract(p.burn, Dat.sp2)
Dat.sp.veg = extract(BPS.Pred, Dat.sp)
Dat.sp2.veg = extract(BPS.Pred, Dat.sp2)

#plot 1, based on forest vs. nonforest
Dat.sp.forest = Dat.sp.br[which(Dat.sp.epaid$levelii2 == "NWF" | Dat.sp.epaid$levelii2 == "SRF" | Dat.sp.epaid$levelii2 == "KSNF")]
Dat.sp2.forest = Dat.sp2.br[which(Dat.sp2.epaid$levelii2 == "NWF" | Dat.sp2.epaid$levelii2 == "SRF" | Dat.sp2.epaid$levelii2 == "KSNF")]
t.test(Dat.sp.forest, Dat.sp2.forest)
> t.test(Dat.sp.forest, Dat.sp2.forest)

Welch Two Sample t-test

data:  Dat.sp.forest and Dat.sp2.forest
t = 3.5022, df = 259.077, p-value = 0.0005433
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  0.3597984 1.2841169
sample estimates:
  mean of x mean of y 
2.564220  1.742263 

Dat.sp.nonforest = Dat.sp.br[-which(Dat.sp.epaid$levelii2 == "NWF" | Dat.sp.epaid$levelii2 == "SRF" | Dat.sp.epaid$levelii2 == "KSNF")]
Dat.sp2.nonforest = Dat.sp2.br[-which(Dat.sp2.epaid$levelii2 == "NWF" | Dat.sp2.epaid$levelii2 == "SRF" | Dat.sp2.epaid$levelii2 == "KSNF")]
t.test(Dat.sp.nonforest, Dat.sp2.nonforest)

> t.test(Dat.sp.nonforest, Dat.sp2.nonforest)

Welch Two Sample t-test

data:  Dat.sp.nonforest and Dat.sp2.nonforest
t = 6.1863, df = 232.104, p-value = 2.757e-09
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  0.9046293 1.7501304
sample estimates:
  mean of x mean of y 
2.549296  1.221916 

#forest region
Dat.sp.forest1 = data.frame(Dat.sp.forest, rep("Burned", length(Dat.sp.forest))); colnames(Dat.sp.forest1)<-c("p", "status")
Dat.sp2.forest1 = data.frame(Dat.sp2.forest, rep("Non-Burned", length(Dat.sp2.forest))); colnames(Dat.sp2.forest1)<-c("p", "status")
Burn.forest1 = rbind(Dat.sp.forest1, Dat.sp2.forest1)

#non-forest region
Dat.sp.nonforest1 = data.frame(Dat.sp.nonforest, rep("Burned", length(Dat.sp.nonforest))); colnames(Dat.sp.nonforest1)<-c("p", "status")
Dat.sp2.nonforest1 = data.frame(Dat.sp2.nonforest, rep("Non-Burned", length(Dat.sp2.nonforest))); colnames(Dat.sp2.nonforest1)<-c("p", "status")
Burn.nonforest1 = rbind(Dat.sp.nonforest1, Dat.sp2.nonforest1)

png(".\\revision090315\\validation-reigon.png",height = 2500, width = 1500, res = 300, units = "px")

par(mfrow=c(2,1),mar=c(0,0,0,0))

par(mar = c(1,5,1,1))

boxplot(p~status, data = Burn.forest1, main = "Forest region",
        ylab = "Burn Rate",
        ylim = c(0, 10),
        xaxt = "n",
        cex.lab = 1.5, cex.axis = 1.5)

par(mar = c(5,5,1,1))

boxplot(p~status, data = Burn.nonforest1,  main = "Non-Forest region",
        #main = "Intmediate Thin",
        #xlab = "treatment effects",
        ylab = "Burn Rate",
        ylim = c(0, 10),
        #xaxt = "n",
        cex.lab = 1.5, cex.axis = 1.5)

dev.off()

#plot 2, based on forest vs. shrub, and grass
shortnames = c("Gras","PCDF","InDF","PJW","PIPO","Salp","Mixd","Hdwd","CaCr","DeSc","MMsh","Sgbr","Shsp")
                  2       3      4      5     6      7       8      9     10     11     12     13     14

Dat.sp.forest.veg = Dat.sp.br[which(Dat.sp.veg >= 3 &  Dat.sp.veg <= 9)]
Dat.sp2.nonforest.veg = Dat.sp2.br[which(Dat.sp2.veg >= 3 &  Dat.sp2.veg <= 9)]
t.test(Dat.sp.forest.veg, Dat.sp2.nonforest.veg)

> t.test(Dat.sp.forest.veg, Dat.sp2.nonforest.veg)

Welch Two Sample t-test

data:  Dat.sp.forest.veg and Dat.sp2.nonforest.veg
t = 1.9522, df = 265.189, p-value = 0.05197
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  -0.003856393  0.903085074
sample estimates:
  mean of x mean of y 
2.355140  1.905526 

Dat.sp.shrub.veg = Dat.sp.br[which(Dat.sp.veg >= 10)]
Dat.sp2.nonshrub.veg = Dat.sp2.br[which(Dat.sp2.veg >= 10)]
t.test(Dat.sp.shrub.veg, Dat.sp2.nonshrub.veg)
> t.test(Dat.sp.shrub.veg, Dat.sp2.nonshrub.veg)

Welch Two Sample t-test

data:  Dat.sp.shrub.veg and Dat.sp2.nonshrub.veg
t = 5.8362, df = 209.915, p-value = 2.005e-08
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  0.8625139 1.7423952
sample estimates:
  mean of x mean of y 
2.544503  1.242048 

Dat.sp.grass.veg = Dat.sp.br[which(Dat.sp.veg == 2)]
Dat.sp2.nongrass.veg = Dat.sp2.br[which(Dat.sp2.veg == 2)]
t.test(Dat.sp.grass.veg, Dat.sp2.nongrass.veg)
> t.test(Dat.sp.grass.veg, Dat.sp2.nongrass.veg)

Welch Two Sample t-test

data:  Dat.sp.grass.veg and Dat.sp2.nongrass.veg
t = 3.8234, df = 25.217, p-value = 0.0007698
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  1.418649 4.728401
sample estimates:
  mean of x mean of y 
4.480000  1.406475 

#forest 
Dat.sp.forest.veg1 = data.frame(Dat.sp.forest.veg, rep("Burned", length(Dat.sp.forest.veg))); colnames(Dat.sp.forest.veg1)<-c("p", "status")
Dat.sp2.nonforest.veg1 = data.frame(Dat.sp2.nonforest.veg, rep("Non-Burned", length(Dat.sp2.nonforest.veg))); colnames(Dat.sp2.nonforest.veg1)<-c("p", "status")
Burn.forest.veg1 = rbind(Dat.sp.forest.veg1, Dat.sp2.nonforest.veg1)

#shrub 
Dat.sp.shrub.veg1 = data.frame(Dat.sp.shrub.veg, rep("Burned", length(Dat.sp.shrub.veg))); colnames(Dat.sp.shrub.veg1)<-c("p", "status")
Dat.sp2.nonshrub.veg1 = data.frame(Dat.sp2.nonshrub.veg, rep("Non-Burned", length(Dat.sp2.nonshrub.veg))); colnames(Dat.sp2.nonshrub.veg1)<-c("p", "status")
Burn.shrub.veg1 = rbind(Dat.sp.shrub.veg1, Dat.sp2.nonshrub.veg1)

#grass
Dat.sp.grass.veg1 = data.frame(Dat.sp.grass.veg, rep("Burned", length(Dat.sp.grass.veg))); colnames(Dat.sp.grass.veg1)<-c("p", "status")
Dat.sp2.nongrass.veg1 = data.frame(Dat.sp2.nongrass.veg, rep("Non-Burned", length(Dat.sp2.nongrass.veg))); colnames(Dat.sp2.nongrass.veg1)<-c("p", "status")
Burn.grass.veg1 = rbind(Dat.sp.grass.veg1, Dat.sp2.nongrass.veg1)



png(".\\revision090315\\validation-vegetation.png",height = 2000, width = 2000, res = 300, units = "px")

par(mfrow=c(2,2),mar=c(0,0,0,0))

par(mar = c(3,5,1,1))

boxplot(p~status, data = Burn.forest.veg1, main = "Forest",
        ylab = "Burn Rate",
        ylim = c(0, 10),
        xaxt = "n",
        cex.lab = 1.5, cex.axis = 1.5)

par(mar = c(3,5,1,1))

boxplot(p~status, data = Burn.shrub.veg1,  main = "Shrub",
        #main = "Intmediate Thin",
        #xlab = "treatment effects",
        ylab = "Burn Rate",
        ylim = c(0, 10),
        #xaxt = "n",
        cex.lab = 1.5, cex.axis = 1.5)


par(mar = c(3,5,1,1))

boxplot(p~status, data = Burn.grass.veg1,  main = "Grass",
        #main = "Intmediate Thin",
        #xlab = "treatment effects",
        ylab = "Burn Rate",
        ylim = c(0, 10),
        #xaxt = "n",
        cex.lab = 1.5, cex.axis = 1.5)

dev.off()



####find the future change in fire regime in current and future climate

setwd("D:\\dataset\\MTBSdata\\FireProjection\\WestUSbrt2")

proj.alb = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
my.colors = colorRampPalette(rev(brewer.pal(4,"RdYlBu")))

#1. read data 
mask2 = readGDAL("D:\\dataset\\MTBSdata\\EM_revision\\Data2\\nlcd01w90er2") ##non burnable areas
mask2 = raster(mask2)
mask2[mask2==2] = NA

mask = readGDAL("D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\mask1km")
mask = raster(mask)
ext = extent(mask) 


##Historical fires
hfp1 = raster("historical_run1_brt.asc") #154 replicate
hfp2 = raster("historical_run2_brt.asc") #153 replicate
hfp3 = raster("historical_run3_brt.asc") #153 replicate
hfp4 = raster("historical_run4_brt.asc") #153 replicate
hfp5 = raster("historical_run5_brt.asc") #153 replicate

hfp = hfp1+hfp2+hfp3+hfp4+hfp5

projection(hfp)<-proj.alb
##climate + vegetation
ffp = raster("futureGFDL_run1_brt.asc")
projection(ffp)<-proj.alb
##climate
ffpvegc = raster("futureGFDL_vegc_run1_brt.asc")
projection(ffpvegc)<-proj.alb

##climate + vegetation
ffpCNRM = raster("futureCNRM_run1_brt.asc")
projection(ffpCNRM)<-proj.alb

##climate
ffpvegcCNRM = raster("futureCNRM_vegc_run1_brt.asc")
projection(ffpvegcCNRM)<-proj.alb


##Figure 5: difference between different scenarios 
##for spatial pattern change
ffp3 = ffp - hfp
ffpvegc3 = ffpvegc - hfp
ffp3CNRM = ffpCNRM - hfp
ffpvegc3CNRM = ffpvegcCNRM - hfp

ffp4 = reclassify(ffp3,c(-Inf,-6,201, -6,-2,202, -2,-1,203,-1,1,204,1,2,205,2,6,206,6,Inf,207) )
ffpvegc4 = reclassify(ffpvegc3,c(-Inf,-6,201, -6,-2,202, -2,-1,203,-1,1,204,1,2,205,2,6,206,6,Inf,207) )
ffp4CNRM = reclassify(ffp3CNRM,c(-Inf,-6,201, -6,-2,202, -2,-1,203,-1,1,204,1,2,205,2,6,206,6,Inf,207) )
ffpvegc4CNRM = reclassify(ffpvegc3CNRM,c(-Inf,-6,201, -6,-2,202, -2,-1,203,-1,1,204,1,2,205,2,6,206,6,Inf,207) )

arg1 <- list(at=seq(201,207,1), labels=c(" <-6","- (2-6)","- (1-2)","NC","(1-2)","(2-6)",">6")) #these are the class names
color1=rev(brewer.pal(7,"RdYlGn"))
color1 <- c(brewer.pal(9,"Blues")[c(9,6,3,1)],brewer.pal(9,"Reds")[c(3,6,9)])
color1=rev(brewer.pal(7,"RdYlBu"))

#figure 5, 5 classes

ffp5 = reclassify(ffp3,c(-Inf,-6,201, -6,-2,202, -2,2,203,2,6,204,6,Inf,205) )
ffpvegc5 = reclassify(ffpvegc3,c(-Inf,-6,201, -6,-2,202, -2,2,203,2,6,204,6,Inf,205) )
ffp5CNRM = reclassify(ffp3CNRM,c(-Inf,-6,201, -6,-2,202, -2,2,203,2,6,204,6,Inf,205) )
ffpvegc5CNRM = reclassify(ffpvegc3CNRM,c(-Inf,-6,201, -6,-2,202, -2,2,203,2,6,204,6,Inf,205) )

arg1 <- list(at=seq(201,205,1), labels=c(" <-6","- (2-6)","-2 - 2","(2-6)",">6")) #these are the class names
color1=rev(brewer.pal(5,"RdYlBu"))


png("FirePatternALL091514_change_brt_final.png",height = 2500, width = 2500, res = 300, units = "px", pointsize = 12)

par(mfrow=c(2,2),mar=c(0,0,0,0))
plot(ffp5,col = color1, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
)
plot(mask2, add=T, col = "white", legend = FALSE)
plot(state, add = TRUE)
#plot(EPA, add = TRUE)
text(x=ext@xmin, y=ext@ymax-100000, "a)", cex = 2)

plot(ffpvegc5,col = color1, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
)
plot(mask2, add=T, col = "white", legend = FALSE)	
plot(state, add = TRUE)
#plot(EPA, add = TRUE)
text(x=ext@xmin, y=ext@ymax-100000, "b)", cex = 2)

par(xpd = FALSE)
plot(ffpvegc5CNRM,col = color1, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
)
plot(mask2, add=T, col = "white", legend = FALSE)	
plot(state, add = TRUE)
#plot(EPA, add = TRUE)
text(x=ext@xmin, y=ext@ymax-100000, "c)", cex = 2)
par(xpd = TRUE)
legend(x = ext@xmax-170000, y = ext@ymax, legend = arg1[[2]], fill = color1,  cex = 1.5,  box.lwd = 0,box.col = "white",bg = "white")

plot(ffp5CNRM,col = color1, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
)
plot(mask2, add=T, col = "white", legend = FALSE)
plot(state, add = TRUE)
#plot(EPA, add = TRUE)
text(x=ext@xmin, y=ext@ymax-100000, "d)", cex = 2)


scalebar(500000, xy=c(ext@xmin, ext@ymin+100000), type='bar', divs=2, below = "Meters", cex = 1.2)

dev.off()

#figure 4; 5 classes
##classify
hfp2 = reclassify(hfp,c(-Inf,2,201, 2,4,202, 4,8,203,8,15,204,15,Inf,205) )
ffp2 = reclassify(ffp,c(-Inf,2,201, 2,4,202, 4,8,203,8,15,204,15,Inf,205) )
ffpvegc2 = reclassify(ffpvegc,c(-Inf,2,201, 2,4,202, 4,8,203,8,15,204,15,Inf,205) )
ffp2CNRM = reclassify(ffpCNRM,c(-Inf,2,201, 2,4,202, 4,8,203,8,15,204,15,Inf,205) )
ffpvegc2CNRM = reclassify(ffpvegcCNRM,c(-Inf,2,201, 2,4,202, 4,8,203,8,15,204,15,Inf,205) )


arg1 <- list(at=seq(201,205,1), labels=c("<2","2-4","4-8","8-15",">15")) #these are the class names
color1=rev(brewer.pal(5,"RdYlBu"))

png("FirePatternALL091514_brt_final.png",height = 3000, width = 3000, res = 300, units = "px", pointsize = 12)
##3/4 page figures
#png("FirePatternALL040914.png",height = 6, width = 6, res = 300, units = "in", pointsize = 12)

par(mfrow=c(2,3),mar=c(0,0,0,0))
plot(ffp2,col = color1, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
)
plot(mask2, add=T, col = "white", legend = FALSE)	
plot(state, add = TRUE)
#plot(EPA2, add = TRUE)
text(x=ext@xmin+100000, y=ext@ymax-100000, "a)", cex = 2)

par(xpd = FALSE)
plot(ffpvegc2,col = color1, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
)
plot(mask2, add=T, col = "white", legend = FALSE)
plot(state, add = TRUE)
#plot(EPA2, add = TRUE)
text(x=ext@xmin+100000, y=ext@ymax-100000, "b)", cex = 2)

par(xpd = TRUE)
#legend(x = ext@xmax-100000, y = ext@ymax, legend = arg1[[2]], fill = color1,  cex = 1.5,  box.lwd = 0,box.col = "white",bg = "white")

plot.new()
legend(x = 0.1, y = 0.75, legend = arg1[[2]], fill = color1,  cex = 2,  box.lwd = 0,box.col = "white",bg = "white")
legend(x = 0.1, y = 0.85, legend = "Burn rate",  cex = 2,  box.lwd = 0,box.col = "white",bg = "white")
#legend(x = 0.1, y = 0.8, legend = "(times per 500 years)", cex = 1, box.lwd = 0,box.col = "white",bg = "transparent")

##This should be climate + vegetation
plot(ffpvegc2CNRM,col = color1, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
)
plot(mask2, add=T, col = "white", legend = FALSE)
plot(state, add = TRUE)
#plot(EPA2, add = TRUE)
text(x=ext@xmin+100000, y=ext@ymax-100000, "c)", cex = 2)

##This should be climate only
plot(ffp2CNRM,col = color1, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
)
plot(mask2, add=T, col = "white", legend = FALSE)	
plot(state, add = TRUE)
#plot(EPA2, add = TRUE)
text(x=ext@xmin+100000, y=ext@ymax-100000, "d)", cex = 2)

plot(hfp2,col = color1, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
)
plot(mask2, add=T, col = "white", legend = FALSE)
plot(state, add = TRUE)
#plot(EPA2, add = TRUE)
text(x=ext@xmin+100000, y=ext@ymax-100000, "e)", cex = 2)

scalebar(500000, xy=c(ext@xmin, ext@ymin+100000), type='bar', divs=2, below = "Meters", cex = 1.2)

dev.off()

###########test the relative effects of climate vs. climate-driven vegetation on burned rate

hfp2 = reclassify(hfp,c(-Inf,2,201, 2,4,202, 4,8,203,8,15,204,15,Inf,205) )
ffp2 = reclassify(ffp,c(-Inf,2,201, 2,4,202, 4,8,203,8,15,204,15,Inf,205) )
ffpvegc2 = reclassify(ffpvegc,c(-Inf,2,201, 2,4,202, 4,8,203,8,15,204,15,Inf,205) )
ffp2CNRM = reclassify(ffpCNRM,c(-Inf,2,201, 2,4,202, 4,8,203,8,15,204,15,Inf,205) )
ffpvegc2CNRM = reclassify(ffpvegcCNRM,c(-Inf,2,201, 2,4,202, 4,8,203,8,15,204,15,Inf,205) )

#for gfdl climate
br.veg.effects.gfdl = ffp - hfp
br.cli.effects.gfdl = ffpvegc - hfp

br.veg.effects.gfdl = ffp - ffpvegc
br.cli.effects.gfdl = ffpvegc - hfp


library(dismo)
rpts = randomPoints(br.veg.effects.gfdl, 5000) #dismo function
rpts.gfdl = extract(stack(br.veg.effects.gfdl, br.cli.effects.gfdl), rpts)

t.test(rpts.gfdl[,1], rpts.gfdl[,2])
> t.test(rpts.gfdl[,1], rpts.gfdl[,2])

Welch Two Sample t-test

data:  rpts.gfdl[, 1] and rpts.gfdl[, 2]
t = 9.3645, df = 6750.592, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  0.6341143 0.9698857
sample estimates:
  mean of x mean of y 
1.6368    0.8348 

br.veg.effects.cnrm = ffpCNRM - hfp
br.cli.effects.cnrm = ffpvegcCNRM - hfp
rpts.cnrm = extract(stack(br.veg.effects.cnrm, br.cli.effects.cnrm), rpts)
t.test( rpts.cnrm[,2], rpts.cnrm[,1])
> t.test( rpts.cnrm[,2], rpts.cnrm[,1])

Welch Two Sample t-test

data:  rpts.cnrm[, 2] and rpts.cnrm[, 1]
t = 12.4888, df = 8409.923, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  0.7855438 1.0780562
sample estimates:
  mean of x mean of y 
1.6220    0.6902



###calculate the vegetation transiton for largest fire regime area occurs, 
## ffp4: climate + veg change under gfdl
## ffpvegc4CNRM: climate + veg change under CNRM

ffp4_rc = ffp4 == 207
ffpvegc4CNRM_rc = ffpvegc4CNRM == 207

writeRaster(ffp4_rc,"D:\\dataset\\MTBSdata\\FireProjection\\revision090315\\ffp4_rc.tif", format="GTiff", overwrite=TRUE)
writeRaster(ffpvegc4CNRM_rc,"D:\\dataset\\MTBSdata\\FireProjection\\revision090315\\ffpvegc4CNRM_rc.tif", format="GTiff", overwrite=TRUE)


plot(ffp4_rc)
plot(EPA, add = T)

plot(ffpvegc4CNRM_rc)
plot(EPA, add = T)

ffp4_rc[ffp4_rc != 1] = NA
ffpvegc4CNRM_rc[ffpvegc4CNRM_rc != 1] = NA


BPS.Pred_gfdl = BPS.Pred*ffp4_rc
BPS.Pred.future_gfdl = BPS.Pred.future*ffp4_rc

change.gfdl.df <- crosstab(BPS.Pred_gfdl, BPS.Pred.future_gfdl)
names(change.gfdl.df)[1:2] <- c("Current", "GFDL")

# for GFDL scenarios
change.gfdl.df <- change.gfdl.df[is.na(change.gfdl.df$Current) == F & is.na(change.gfdl.df$GFDL) == F,]
change.gfdl.df <- matrix(change.gfdl.df$Freq, nrow=14, ncol=14)
change.gfdl.df = change.gfdl.df[-1,]
change.gfdl.df = change.gfdl.df[,-1]

shortnames = c("Gras","PCDF","InDF","PJW","PIPO","Salp","Mixd","Hdwd","CaCr","DeSc","MMsh","Sgbr","Shsp")
rownames(change.gfdl.df) <- shortnames
colnames(change.gfdl.df) <- shortnames

sum01 <- apply(change.gfdl.df, MARGIN=1, FUN=sum)
percchange <- 100 * sweep(change.gfdl.df, MARGIN=1, STATS=sum01, FUN="/")

# for CNRM scenarios
BPS.Pred_cnrm = BPS.Pred*ffpvegc4CNRM_rc
BPS.Pred.future_cnrm = BPS.Pred.future.CNRM*ffpvegc4CNRM_rc

change.cnrm.df <- crosstab(BPS.Pred_cnrm, BPS.Pred.future_cnrm)
names(change.cnrm.df)[1:2] <- c("Current", "CNRM")

change.cnrm.df <- change.cnrm.df[is.na(change.cnrm.df$Current) == F & is.na(change.cnrm.df$CNRM) == F,]
change.cnrm.df <- matrix(change.cnrm.df$Freq, nrow=14, ncol=14)
change.cnrm.df = change.cnrm.df[-1,]
change.cnrm.df = change.cnrm.df[,-1]

rownames(change.cnrm.df) <- shortnames
colnames(change.cnrm.df) <- shortnames

sum02 <- apply(change.cnrm.df, MARGIN=1, FUN=sum)
percchange2 <- 100 * sweep(change.cnrm.df, MARGIN=1, STATS=sum02, FUN="/")


#plot figures

library(ggplot2)
library(reshape2)

percchange_long = data.frame(melt(percchange, id.vars=shortnames), scenario = "GFDL")
percchange2_long = data.frame(melt(percchange2, id.vars=shortnames), scenario = "CNRM")

dat_long = rbind(percchange_long, percchange2_long)
dat_long2 = dat_long[which(dat_long$value > 10), ]

ggplot(data=dat_long2, aes(x=Var1, y=value, fill=Var2)) +
  geom_bar(stat="identity") + 
  facet_grid(Var2 ~ scenario) + 
  scale_fill_discrete(breaks=shortnames,
                      name="",
                      labels=shortnames)+
  theme(legend.text = element_text(size = 18))+
  theme(axis.title.x = element_text(face="bold", colour="black", size=18),axis.text.x  = element_text(colour="black",size=18))+
  theme(axis.title.y = element_text(face="bold", colour="black", size=18),axis.text.y  = element_text(colour="black",size=18))+
  xlab("Current Vegetation") + ylab("Percentage of future vegetation") +
  theme(strip.text.x = element_text(size=18), strip.text.y = element_text(size=18))

setwd("D:\\dataset\\MTBSdata\\FireProjection")
ggsave(".\\revision090315\\VegTransition_largest_fire-regime_change.png", width = 20, height = 15, units = "in")


BPS.Pred_gfdl = BPS.Pred*ffp4_rc
BPS.Pred.future_gfdl = BPS.Pred.future*ffp4_rc
plot(BPS.Pred_gfdl)
plot(BPS.Pred.future_gfdl)



#Figure 2 vegetation distributions under different climate scenarios#
setwd("D:\\dataset\\MTBSdata\\FireProjection\\Gisdata")
mask = readGDAL("mask1km")
mask = raster(mask)

mask2 = readGDAL("D:\\dataset\\MTBSdata\\EM_revision\\Data2\\nlcd01w90er2") ##non burnable areas
mask2 = raster(mask2)
mask2[mask2==2] = NA

#bps = readGDAL("westbps1km")  ###westbps1km has agriculre class, agriculre class derived from EVT
bps = readGDAL("westbpsr1km3") ###westbpsr1km3 is original BPS from LANDFIRE data, has 14 classes, using majority rule from 30 meters. 
bps = raster(bps)
bps[bps==-9999] = NA
bps[bps==1] = 2
bps = bps - 1
bps = crop(bps, extent(mask))
projection(bps) <- projection(mask)
extent(bps) <- extent(mask)
bps = bps*mask

BPS.Pred = raster('D:\\dataset\\MTBSdata\\FireProjection\\Gisdata\\BPS.Pred.asc')
BPS.Pred.future = raster('BPS.Pred.future.asc')
BPS.Pred.future.CNRM = raster('BPS.Pred.future.CNRM.asc')

dsn = "D:\\dataset\\ClimateData\\Regions"
EPA = readOGR(dsn=dsn,layer="EpaWest3")

state = readOGR(dsn="D:\\dataset\\NationalAtlas\\statep010_nt00798.tar",layer="statep010_ables2_west")


arg <- list(at=seq(1,14,1), labels=c("NonB","Gras","PCDF","InDF","PJW","PIPO","Salp","Mixd","Hdwd","CaCr","DeSc","MMsh","Sgbr","Shsp")) #these are the class names
color=colors()[c(1, 7, 25, 26, 33, 42, 48, 53, 68, 75, 86, 95, 129, 143)]
leg = c("NonB","Gras","PCDF","InDF","PJW","PIPO","Salp","Mixd","Hdwd","CaCr","DeSc","MMsh","Sgbr","Shsp")
ext = extent(bps) 

png("BPS2_032514.png",height = 3000, width = 3000, res = 300, units = "px")
par(mfrow=c(2,2),mar=c(0,0,0,0), oma=c(0,0,0,3))
plot(bps, 
     #main = "Real vegetation type distribution",
     col = color, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
) 
plot(mask2, add=T, col = "white", legend = FALSE)
plot(state, add = TRUE)
#plot(EPA, add = TRUE)
text(x=ext@xmin, y=ext@ymax-100000, "a)", cex = 2)

plot(BPS.Pred, 
     #main = "Simulated vegetation type distribution",
     col = color, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
) 
plot(mask2, add=T, col = "white", legend = FALSE)
plot(state, add = TRUE)
#plot(EPA, add = TRUE)
text(x=ext@xmin, y=ext@ymax-100000, "b)", cex = 2)

plot(BPS.Pred.future, 
     #main = "Simulated future vegetation type distribution GFDL",
     col = color, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
) 

plot(mask2, add=T, col = "white", legend = FALSE)
plot(state, add = TRUE)
#plot(EPA, add = TRUE)
text(x=ext@xmin, y=ext@ymax-100000, "c)", cex = 2)

plot(BPS.Pred.future.CNRM, 
     #main = "Simulated future vegetation type distribution CNRM",
     col = color, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
) 
plot(mask2, add=T, col = "white", legend = FALSE)
plot(state, add = TRUE)
#plot(EPA, add = TRUE)
text(x=ext@xmin, y=ext@ymax-100000, "d)", cex = 2)

#legend(x='topright', #'topright',
#      #inset=c(-0.3,0),#horiz = TRUE,
#       legend = leg, fill = color, cex = 1.5,
#       #bg = "transparent",	
#)

scalebar(500000, xy=c(ext@xmin, ext@ymin+100000), type='bar', divs=2, below = "Meters", cex = 1.2)

dev.off()


######reproduce Figure A5
## source R codes:D:\dataset\MTBSdata\NewMTBSdata100114\validation.r

proj.geo = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
proj.eco = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

library("raster")
library("maptools")
library("rgdal")
setwd("D:\\dataset\\MTBSdata\\NewMTBSdata100114\\Validation")

mask2 = readGDAL("D:\\dataset\\MTBSdata\\EM_revision\\Data2\\nlcd01w90er2") ##non burnable areas
mask2 = raster(mask2)
mask2[mask2==2] = NA

dsn = "D:\\dataset\\ClimateData\\Regions"
EPA = readOGR(dsn=dsn,layer="EpaWest3")

state = readOGR(dsn="D:\\dataset\\NationalAtlas\\statep010_nt00798.tar",layer="statep010_ables2_west")

firepatch <- readOGR(dsn = "D:\\dataset\\MTBSdata\\NewMTBSdata100114\\perimeter", layer = "mtbs_west_2011_2012")
firepatch = firepatch[firepatch$FireType=="WF"|firep$FireType=="WFU",]
firepatch = spTransform(firepatch, CRS(proj.eco))

##Historical fires
hfp1 = raster("D:\\dataset\\MTBSdata\\FireProjection\\WestUSbrt2\\historical_run1_brt.asc") #154 replicate
hfp2 = raster("D:\\dataset\\MTBSdata\\FireProjection\\WestUSbrt2\\historical_run2_brt.asc") #153 replicate
hfp3 = raster("D:\\dataset\\MTBSdata\\FireProjection\\WestUSbrt2\\historical_run3_brt.asc") #153 replicate
hfp4 = raster("D:\\dataset\\MTBSdata\\FireProjection\\WestUSbrt2\\historical_run4_brt.asc") #153 replicate
hfp5 = raster("D:\\dataset\\MTBSdata\\FireProjection\\WestUSbrt2\\historical_run5_brt.asc") #153 replicate

hfp = hfp1+hfp2+hfp3+hfp4+hfp5
projection(hfp)<-proj.eco

hfp2 = reclassify(hfp,c(-Inf,1,201, 1,2,202,2,4,203, 4,8,204,8,15,205,15,Inf,206) )
arg1 <- list(at=seq(201,206,1), labels=c("<1","1-2","2-4","4-8","8-15",">15")) #these are the class names
color1=brewer.pal(6,"YlOrRd")

ext = extent(hfp) 

png("Appendix_burnpro_validation_101514_3.png",height = 3000, width = 2500, res = 300, units = "px")

par(mfrow=c(1,1),mar=c(0,0,0,0)) 
plot(hfp2,col = color1, 
     #axis.arg=arg
     #xlim=c(-2500000, -500000), 
     #ylim=c(ext@ymin, ext@ymax),
     legend=FALSE,
     axes=FALSE,
     box=FALSE,
)


plot(mask2, add=T, col = "white", legend = FALSE)
plot(state, add = TRUE)
plot(EPA, add = TRUE)
plot(firepatch, add = TRUE, border="blue")	

legend(x=ext@xmax-150000, y=ext@ymax-100000, legend = arg1[[2]], fill = color1,  cex = 2,  box.lwd = 0,box.col = "white",bg = "white")
legend(x = ext@xmax-250000, y = ext@ymax-5000, legend = "Burn rate",  cex = 2,  box.lwd = 0,box.col = "white",bg = "transparent")

dev.off()

library("raster")
library("maptools")
library("rgdal")
setwd("D:\\dataset\\MTBSdata\\NewMTBSdata100114\\Validation")

getwd()

load("validation020215.RData")


tiff("Figure3.tif",height = 2500, width = 2500, res = 300, units = "px", pointsize = 12)
par(mfrow=c(2,2),mar=c(0,0,0,0))

par(mar = c(5,5,1,1))

names.arg = c("<0.125", "0.125-0.25","0.25-0.375","0.375-0.5", "0.5-0.625", "0.625-0.75","0.75-0.875", ">0.875")
mp <- barplot(relative[c(1:5, 8,6,7)], axes = FALSE, axisnames = FALSE, xlab = "Fire Occurrence Probability", ylab = "Fire Probability", cex.lab = 1.5)
text(mp, par("usr")[3], labels = names.arg, srt = 30, adj = c(1,1), xpd = TRUE, cex=1.25)
axis(2,cex=1.25)
text(x=1, y=0.00045, "a)", cex = 2)

par(mar = c(5,5,1,0))
plot(density(dat.new2$Area_ha_pred), lwd = 2,xlab ="Fire size (ha)", ylab = "Density Functions", main = "", cex.axis=1.5, cex.lab = 1.5,cex.lab = 1.5,font.lab = 2)
lines(density(dat.new2$Area_ha), lwd = 2, col = 2)
#legend("topright", inset=0.05, lty = c(1,1), col = c(1,2), lwd=c(2,2), 
#       c(paste("MFS = ", round(median(dat.new2$Area_ha_pred), 1),sep = ""),
#         paste("MFS = ", round(median(dat.new2$Area_ha), 1),sep = "")),
#  	 text.col = c(1,2),
#		 cex = 1.5,
#      horiz=F, box.col = "white")
legend("topright", inset=0.05, lty = c(1,1), col = c(1,2), lwd=c(2,2), 
       c("Simulated", "Observed"),
       text.col = c(1,2),
       cex = 1.5,
       horiz=F, box.col = "white")
text(x=5000, y=0.00031, "b)", cex = 2)

#fire date
par(mar = c(5,5,0,0))
matplot(HDmean.x,HDmean.y, type = "l", lwd = 2,ylim = c(0, 0.03), xlab ="Day of Year", ylab = "Density Functions", cex.axis=1.5, cex.lab = 1.5,cex.lab = 1.5,font.lab = 2)
#matplot(HDmean.x,HDup.y, type = "l", add = T, xlab ="Fire date", ylab = "Density Functions" )
#matplot(HDmean.x,HDlow.y, type = "l", add = T,xlab ="Fire date", ylab = "Density Functions" )
#polygon(c(HDmean.x, rev(HDmean.x)), c(HDup.y, rev(HDlow.y)),
#        col = "grey90", border = NA)
#matplot(HDmean.x,HDmean.y, type = "l", ylim = c(0, 0.05), lwd = 2, add = T, xlab ="Fire date", ylab = "Density Functions" , cex.axis=1, cex.lab = 1,cex.lab = 1,font.lab = 2)
matplot(density(Fdate)$x, density(Fdate)$y, type = "l",add = T,col = 2, ylim = c(0, 0.05), lwd = 2,  xlab ="Day of Year", ylab = "Density Functions" ,main = "", cex.axis=1.5, cex.lab = 1,cex.lab = 1.5,font.lab = 2)

legend("topright", inset=0.05, lty = c(1,1), col = c(1,2), lwd=c(2,2,2,2), 
       c("Simulated", "Observed"), text.col = c(1,2), cex=1.5, 
       #paste("Climate + Vegetation: \nPeak Fire Date = ", floor(mean(apply(SfdateF, 2, mean))),"\n",sep = ""), 
       #paste("Climate: \nPeak Fire Date = ", floor(mean(apply(SfdateFc, 2, mean))),"\n",sep = "")),
       horiz=F, box.col = "white")
text(x=70, y=0.028, "c)", cex = 2)


par(mar = c(5,5,0,1))
boxplot(p~status, data = Burn.data1, 
        #main = "Intmediate Thin",
        #xlab = "treatment effects",
        ylab = "Burn Rate",
        ylim = c(0, 1),
        #xaxt = "n",
        cex.lab = 1.5, cex.axis = 1.5)
text(x=0.7, y=9, "d)", cex = 2)



dev.off()

