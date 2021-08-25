library(move)
library(scales)
library(oce)
library(ocedata)
library(marmap)
library(rgdal)
library(sp)
library(sf)
library(foieGras)
library(ggplot2)

data(coastlineWorldFine)
data(coastlineWorld)

library(maptools)
data(wrld_simpl)

# load data: hourly predictions from foieGras with 3 behavioral states
load("data-created/3stateHMM_24hr.Rdata")
# object = md24
load("data-created/2stateHMM_24hr_GLS.Rdata") # object name = md24_gls

## To Do when GLS data are ready: Add in the GLS tracks (December 23, 2020)

#############################################
# FIGURE 1 WORLD MAP
#############################################
# To Do -- Add in Geolocator data

# Set line weights and colors
lwd = 1
poCol = "darkturquoise"
paCol = "deepskyblue"
ltCol = "blue"

# projections that work well for this distribution of tracks include mollweide, robinson, natural earth
cl45 <- coastlineCut(coastlineWorldFine, lon_0=-90)
p <- "+proj=natearth +lon_0=-90 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"


### LOAD IN GEOGRAPHIC LAYERS
eezs <- readOGR('/Users/autumn-lynn/Dropbox (Personal)/MATLAB/GeographicFiles/World_EEZ_v8_20140228/World_Maritime_Boundaries_v8.shp')
eezs <- spTransform(eezs,crs(p))

# eezs <- readOGR('/Users/autumn-lynn/Dropbox (Personal)/MATLAB/GeographicFiles/World_EEZ_v8_20140228/World_EEZ_v8_2014_HR.shp')
# eezs <- spTransform(eezs,crs(p))

# lineshape = eezs
# point_coordinates = c()
# i = 1
# nLines = nrow(lineshape)
# for (i in 1: nLines) {
#   line1 <- lineshape[i,]@lines[[1]]@Lines[[1]]
#   line1coords <- line1@coords
#   point_coordinates = rbind(point_coordinates, line1coords)
# }
# pointshape <- data.frame(x=point_coordinates[,1], y=point_coordinates[,2])
# coordinates(pointshape) <- ~x+y
# 
# #test result
# plot(lineshape)
# points(pointshape)
# 
# eezs.ll = as.data.frame(coordinates(pointshape))
# names(eezs.ll) = c("longitude","latitude")
# 
# eezs.coast = as.coastline(eezs.ll$longitude, eezs.ll$latitude)
# eezsCut <- coastlineCut(eezs.coast, lon_0=-100)
# #eezsCut <- coastlineCut(eezs.ll, lon_0=-100)



pdf(file="map_outputs/Fig1-eezs2.pdf", width =11, height = 8.5)
  mapPlot(cl45, projection=p, latitudelim=c(-90,90), col='white', type="l", lonlabels=TRUE, grid=TRUE,drawBox=FALSE)
  #mtext(p, line=line, adj=1, col=pcol, font=font)
  mapPolygon(cl45, col='gray88', lty="blank")
  mapLines(cl45, col="gray50", lwd = 0.1)
  #mapPoints(eezs.ll, col="black") #missing NZ
  lines(eezs, col="gray50", lwd = 0.25)

  # spherical
  # p <- "+proj=nsper +h=90000000"
  # mapPlot(cl45, projection=p, col='gray88')

# POMARINE JAEGER
  po = md24[md24$ID =="POJA-2019-CAN-01",]
  mapLines(po$x, po$y, pch=19, col=alpha(poCol,1), lwd=lwd)
  mapPoints(po$x, po$y, pch=19, col=alpha(poCol,0.5), cex=0.5)

# PARASITIC JAEGER (need to separate each individual so doesn't connect back to breeding area)
  pa01 = md24[md24$ID == "PAJA_CAN_01_2018",]
  pa02 = md24[md24$ID == "PAJA_CAN_02_2018",]

  #mapPoints(pa$x, pa$y, pch=19, col=alpha("deepskyblue",0.5), cex=0.5)
  mapLines(pa01$x, pa01$y, pch=19, col=alpha(paCol,1), lwd=lwd)
  mapPoints(pa01$x, pa01$y, pch=19, col=alpha(paCol,0.5), cex=0.5)
  
  mapLines(pa02$x, pa02$y, pch=19, col=alpha(paCol,1), lwd=lwd)
  mapPoints(pa02$x, pa02$y, pch=19, col=alpha(paCol,0.5), cex=0.5)
  
  pa03 = md24_gls[md24_gls$ID == "234",]
  mapLines(pa03$x, pa03$y, pch=19, col=alpha(paCol,1), lwd=lwd)
  mapPoints(pa03$x, pa03$y, pch=19, col=alpha(paCol,0.3), cex=0.5)
  
  pa04 = md24_gls[md24_gls$ID == "281",]
  mapLines(pa04$x, pa04$y, pch=19, col=alpha(paCol,1), lwd=lwd)
  mapPoints(pa04$x, pa04$y, pch=19, col=alpha(paCol,0.3), cex=0.5)
  
  pa05 = md24_gls[md24_gls$ID == "300",]
  mapLines(pa04$x, pa04$y, pch=19, col=alpha(paCol,1), lwd=lwd)
  mapPoints(pa04$x, pa04$y, pch=19, col=alpha(paCol,0.3), cex=0.5)

  # LONG-TAILED JAEGER
  lt01 = md24[md24$ID == "LTJA-2019-CAN-01",]
  lt02 = md24[md24$ID == "LTJA-2019-CAN-02",]

  #mapPoints(lt01$x, lt01$y, pch=19, col=alpha(rgb(0,0,1), 0.5), cex=0.5)
  mapLines(lt01$x, lt01$y, pch=19, col=alpha(ltCol, 0.75), lwd = lwd)
  mapPoints(lt01$x, lt01$y, pch=19, col=alpha(ltCol,0.25), cex=0.5)
  
  mapLines(lt02$x, lt02$y, pch=19, col=alpha(ltCol, 1), lwd=lwd)
  mapPoints(lt02$x, lt02$y, pch=19, col=alpha(ltCol,0.25), cex=0.5)
  
dev.off()


##############################################
# FIGURE 4a POMARINE JAEGER NOMADIC MOVEMENTS
##############################################

library(PBSmapping)
library(maptools)

# Plot land function using PBS mapping for a Pacific Centered simple (but unprojected) plotting function
data(wrld_simpl)
data(worldLLhigh) # PBSmapping

plotland <- function(){
  for(i in 0:length(unique(worldLLhigh$PID))) polygon(worldLLhigh$X[worldLLhigh$PID == i], worldLLhigh$Y[worldLLhigh$PID == i], col = "gray88", lty="blank")
}

plot(worldLLhigh)

# Load breeding tracks
load("data-created/foiegras_breeding_resampledTimes.Rdata") #variable = fitRS
#extract lat and long
fit=fitRS
po <- grab(fit, what = "p", as_sf = FALSE)
po = po[po$id %in% c("POJA-2019-CAN-01_2ndBr"),]

po = po[po$date>"2020-06-01 00:00:56",]

# Set line weights and colors
lwd = 1

# #Converting +ve/-ve longitude to +ve longitude
 convlon <- function(lon){
   ifelse(lon < 0, lon + 360, lon)
 }
# 
 po$long360 = convlon(po$lon)

 # 0 to 360 EEZ layer, YAY!
eezs <- readOGR('/Users/autumn-lynn/Dropbox (Personal)/MATLAB/GeographicFiles/World_EEZ_v11_20191118_HR_0_360/eez_v11_0_360.shp') 
#eezs <- spTransform(eezs,crs("+proj=longlat +ellps=WGS84 +datum=WGS84")) 

# Argos Pomarine Jaeger
pdf(file="map_outputs/POJA_nomadicMovements_June2020_taller.pdf",width=8, height=4)
  plot(po$long360,po$lat, pch=16, col="white", ylim=c(56,max(po$lat)+3), xlim=c(min(po$long360)-7,max(po$long360)+50))
  plotland()
  lines(eezs, col="gray50", lwd = 0.25)
  lines(po$long360,po$lat, pch=19, col=alpha("red",0.75))
  points(po$long360,po$lat, pch=20, col=alpha("red",0.55))
  #title(main="POJA 2020")
dev.off()



############ PLOT INDIVIDUAL PARASITIC JAEGER MAPS ############
data(wrld_simpl)
plotlanda <- function(){
  plot(wrld_simpl, col = "darkgray", border = "black", lwd = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, add=TRUE)
}

### RAW Data ###
pa_gls = d
# "234" "281" "300"
ind = pa_gls[pa_gls$Geo==281,]
plot(ind$LON, ind$LAT, pch=19, col=alpha("blue",0.25))
plotlanda()
points(ind$LON, ind$LAT, pch=19, col=alpha("blue",0.25))
lines(ind$LON, ind$LAT, pch=19, col=alpha("blue",0.75))
title(main="281")

pa_gls = md24_gls
pa_gls = fit24_eqF # equinox points filtered out
# "234" "281" "300"
ind = pa_gls[pa_gls$ID==300,]
plot(ind$x, ind$y, pch=19, col=alpha("deeppink2",0.25))
plotlanda()
points(ind$x, ind$y, pch=19, col=alpha("deeppink2",0.25))
lines(ind$x, ind$y, pch=19, col=alpha("deeppink2",0.75))
title(main="300")

# Argos Parasitic Jaegers
ind = pa[pa$ID=="PAJA_CAN_02_2018",]
plot(ind$x, ind$y, pch=19, col=alpha("deeppink2",0.25))
plotlanda()
points(ind$x, ind$y, pch=19, col=alpha("deeppink2",0.25))
lines(ind$x, ind$y, pch=19, col=alpha("deeppink2",0.75))
title(main="PAJA_CAN_02_2018")

#mapPoints(pa_gls$x, pa_gls$y, pch=19, col=alpha("deeppink2",0.5), cex=0.5)
mapLines(pa_gls$x, pa_gls$y, pch=19, col=alpha("deeppink2",0.75))

# unmodeled GLS data
mapPoints(paja_f$LON, paja_f$LAT, pch=19, col=alpha("deepskyblue",0.5), cex=0.5)
mapLines(paja_f$LON, paja_f$LAT, pch=19, col=alpha("deepskyblue",0.75))

# Long-tailed Jaeger
lt = md24[md24$ID %in% c("LTJA-2019-CAN-01", "LTJA-2019-CAN-02"),]
#mapPoints(lt$x, lt$y, pch=19, col=alpha(rgb(0,0,1), 0.5), cex=0.5)
#mapLines(lt$x, lt$y, pch=19, col=alpha(rgb(0,0,1), 0.75))
mapLines(lt$x, lt$y, pch=19, col=alpha("green",0.75), lwd=2)

