library(rerddapXtracto)

# packages used in the example
library("gganimate")
library("ggplot2")
library("plotdap")

require("rerddap")

#Converting +ve/-ve longitude to +ve longitude
convlon <- function(lon){
  ifelse(lon < 0, lon + 360, lon)
}

##########################################################################################
# Load Data
##########################################################################################

# foieGras modeled data
load("data-created/3stateHMM_24hr.Rdata") # object name = md24

# Create separate data frames for each species, primarily to test the oceanographic datasets, and 
# rextract pulls and to make data pulls faster
pa = md24[md24$ID %in% c("PAJA_CAN_01_2018", "PAJA_CAN_02_2018"),]
lt = md24[md24$ID %in% c("LTJA-2019-CAN-01", "LTJA-2019-CAN-02"),]
po = md24[md24$ID =="POJA-2019-CAN-01",]

## Modeled GLS
load("data-created/ukfsstGLS_filtered_finaltracks.Rdata") #object = ukfsstGLS
pa_gls = ukfsstGLS

# Raw GLS to be able to compare insitu SST measurements
pa_gls_raw = read.csv("data/paja_f.csv")
pa_gls_raw$datetime = paste(pa_gls_raw$Date, "12:00:00", sep=" ")
pa_gls_raw$Date = as.POSIXct(pa_gls_raw$datetime, format = "%m/%d/%y %H:%M:%S", tz="GMT")

##########################################################################################
# SST
##########################################################################################

# https://podaac.jpl.nasa.gov/dataset/Geo_Polar_Blended_Night-OSPO-L4-GLOB-v1.0
# GHRSST Level 4 OSPO Global Nighttime Foundation Sea Surface Temperature Analysis (GDS version 2)
# Provides SST for polar regions unlike some other datasets
# nesdisGeoPolarSSTN5NRT
# 2017 onward
# 0.05 degree grid
# works across the dateline without any other parsing

tagData <- lt
dataInfo <- rerddap::info('nesdisGeoPolarSSTN5SQNRT')                 
parameter <- 'analysed_sst'
xcoord <- convlon(tagData$x)
ycoord <- tagData$y
tcoord <- tagData$date
xlen <- 0.1
ylen <- 0.1
extractLT <- rxtracto(dataInfo, parameter = parameter, xcoord = xcoord,ycoord = ycoord, tcoord= tcoord,xlen = xlen, ylen = ylen, progress_bar = TRUE)
save(extractLT, file = "data-created/sstLT.Rdata")

tagData <- po
dataInfo <- rerddap::info('nesdisGeoPolarSSTN5SQNRT')                 
parameter <- 'analysed_sst'
xcoord <- convlon(tagData$x)
ycoord <- tagData$y
tcoord <- tagData$date
xlen <- 0.1
ylen <- 0.1
extractPO <- rxtracto(dataInfo, parameter = parameter, 
                      xcoord = xcoord, ycoord = ycoord, tcoord= tcoord,
                      xlen = xlen, ylen = ylen, progress_bar = TRUE)
save(extractPO, file = "data-created/sstPO.Rdata")

tagData <- pa
dataInfo <- rerddap::info('nesdisGeoPolarSSTN5SQNRT')                 
parameter <- 'analysed_sst'
xcoord <- convlon(tagData$x)
ycoord <- tagData$y
tcoord <- tagData$date
xlen <- 0.1
ylen <- 0.1
extractPA <- rxtracto(dataInfo, parameter = parameter, xcoord = xcoord,ycoord = ycoord, tcoord= tcoord,xlen = xlen, ylen = ylen, progress_bar = TRUE)
save(extractPA, file = "data-created/sstPA.Rdata")

### Pull remotely sensed data data for GLS to compare to in situ as a test of position estimate
tagData <- pa_gls
dataInfo <- rerddap::info('jplMURSST41')                 
parameter <- 'analysed_sst'
xcoord <- convlon(tagData$x)
ycoord <- tagData$y
tcoord <- tagData$date
xlen <- 0.1
ylen <- 0.1
extractPA_gls <- rxtracto(dataInfo, parameter = parameter, xcoord = xcoord,ycoord = ycoord, tcoord= tcoord,xlen = xlen, ylen = ylen, progress_bar = TRUE)
save(extractPA_gls, file = "data-created/sstRemote_PA_gls_ukfsst.Rdata")

## SST initial plotting
plot(extractPO$`mean analysed_sst`)
plot(extractLT$`mean analysed_sst`)
plot(extractPA$`mean analysed_sst`)

# Compare Insitu and remotely sensed Parasitic Jaeger sst for geolocator birds
paSST_gls = cbind(pa_gls, extractPA_gls)
names(paSST_gls)[names(paSST_gls) == "mean analysed_sst"] <- "meanSST"

pa_gls_raw$Date = as.Date(pa_gls_raw$Date) # for ggplot input

#remotely sensed from ukfsst predicted tracks and insitu in black
ggplot(data=paSST_gls, aes(x=date, y=meanSST, group = tagID)) + geom_point(aes(color = tagID))+ geom_line(aes(color = tagID)) + geom_line(data=pa_gls_raw, aes(x=Date, y=SST, group = Band))

#Insitu only
#ggplot(data=pa_gls_raw, aes(x=Date, y=SST, group = Band)) + geom_point(aes(color = as.factor(Geo)))+ geom_line(aes(color = as.factor(Geo)))


load("data-created/sstPA.Rdata")
load("data-created/sstPO.Rdata")
load("data-created/sstLT.Rdata")

poSST = cbind(po, extractPO)
paSST = cbind(pa, extractPA)
ltSST = cbind(lt, extractLT)

allSST = rbind(poSST, paSST, ltSST)


names(allSST)[names(allSST) == "mean analysed_sst"] <- "meanSST"

ggplot(data=allSST, aes(x=date, y=meanSST, na.rm = TRUE, group=ID)) +geom_point(aes(color = ID)) + geom_line(aes(color = ID))

ggplot(data=pa_gls_raw, aes(x=Time, y=SST, group = Geo)) + geom_point(aes(color = Band))+ geom_line(aes(color = Band))
+geom_point(data=extractPA_gls, x=,colour='red')




ggplot(A,aes(x,y)) +geom_point() +geom_point(data=B,colour='red') + xlim(0, 10) 

# + geom_smooth(aes(color = ID))
ggplot(data=df2, aes(x=dose, y=len, group=supp)) +
  geom_line()+
  geom_point()

require("ggplot2")
require("plotdap")

# Plot track positions colored by sea surface temperature
myPlot <- plotTrack(extractLT, lt$x, lt$y, lt$date, plotColor = 'thermal')
myPlot


########################################################################################
# BATHYMETRY
########################################################################################


# etopo # 0.0166667 degree resolution
# -180 to 180 version and a 360 version
# Need to convert longitude for those individuals that cross the Date Line or Meridian
########################################################################################

# Sample code from vignette using etopo180 which is about 1km resolution at the equator
# Topography, ETOPO1, 0.0166667 degrees, Global (longitude -180 to 180), (Ice Sheet Surface)
# https://cran.r-project.org/web/packages/rerddapXtracto/vignettes/UsingrerddapXtracto.html
tagData <- pa
dataInfo <- rerddap::info('etopo180') # 0.0166667 degree resolution 
xpos <- tagData$x
ypos <- tagData$y
parameter <- 'altitude'
PAbath <- rxtracto(dataInfo, parameter = parameter, xcoord = xpos, ycoord = ypos)
plot(PAbath$`mean altitude`)
save(PAbath, file = "data-created/bathPA_etopo1.Rdata")

# GLS parasitic jaeger birds
tagData <- pa_gls
dataInfo <- rerddap::info('etopo180') # 0.0166667 degree resolution 
xpos <- tagData$x
ypos <- tagData$y
parameter <- 'altitude'
PAbath_gls <- rxtracto(dataInfo, parameter = parameter, xcoord = xpos, ycoord = ypos)
plot(PAbath_gls$`mean altitude`)
save(PAbath_gls, file = "data-created/bathPA_etopo1_gls_ukfsst.Rdata")

# need 360 longitude version
tagData <- po
#dataInfo <- rerddap::info('etopo180') # 0.0166667 degree resolution 
dataInfo <- rerddap::info('etopo360') # 0.0166667 degree resolution 
xpos <- tagData$x
xpos = convlon(xpos)
ypos <- tagData$y
parameter <- 'altitude'
PObath <- rxtracto(dataInfo, parameter = parameter, xcoord = xpos, ycoord = ypos)
plot(PObath$`mean altitude`)
save(PObath, file = "data-created/bathPO_etopo1.Rdata")

# need 360 longitude version
tagData <- lt
#dataInfo <- rerddap::info('etopo180') # 0.0166667 degree resolution 
dataInfo <- rerddap::info('etopo360') # 0.0166667 degree resolution 
xpos <- tagData$x
xpos = convlon(xpos)
ypos <- tagData$y
parameter <- 'altitude'
LTbath <- rxtracto(dataInfo, parameter = parameter, xcoord = xpos, ycoord = ypos)
plot(LTbath$`mean altitude`)
save(LTbath, file = "data-created/bathLT_etopo1.Rdata")


########################################################################################
# Dataset 1: Smith and Sandwell
# Dataset = 0 to 360 degrees
# TOPOGRAPHY (includes land elevation)
########################################################################################
# Dataset 1: Smith and Sandwell
# Dataset = 0 to 360 degrees

tagData <- pa
dataInfo <- rerddap::info('srtm30plus') 
dataInfo
parameter <- 'z'
xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
PAtopo_SS <- rxtracto(dataInfo, parameter = parameter, 
                    xcoord = xcoord, ycoord = ycoord, ,progress_bar = TRUE)
plot(PAtopo_SS$`mean z`)
save(PAtopo_SS, file = "data-created/topoPA_SS.Rdata")

## GLS
tagData <- pa_gls
dataInfo <- rerddap::info('srtm30plus') 
dataInfo
parameter <- 'z'
xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
PAtopo_SS_gls <- rxtracto(dataInfo, parameter = parameter, 
                      xcoord = xcoord, ycoord = ycoord, ,progress_bar = TRUE)
plot(pa_gls$date, PAtopo_SS_gls$`mean z`)
save(PAtopo_SS_gls, file = "data-created/topoPA_SS_gls_ukfsst.Rdata")

tagData <- po
dataInfo <- rerddap::info('srtm30plus') 
dataInfo
parameter <- 'z'
xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
POtopo_SS <- rxtracto(dataInfo, parameter = parameter, 
                      xcoord = xcoord, ycoord = ycoord, ,progress_bar = TRUE)
plot(POtopo_SS$`mean z`)
save(POtopo_SS, file = "data-created/topoPO_SS.Rdata")

tagData <- lt
dataInfo <- rerddap::info('srtm30plus') 
dataInfo
parameter <- 'z'
xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
LTtopo_SS <- rxtracto(dataInfo, parameter = parameter, 
                      xcoord = xcoord, ycoord = ycoord, ,progress_bar = TRUE)
plot(LTtopo_SS$`mean z`)
save(LTtopo_SS, file = "data-created/topoLT_SS.Rdata")

########################################################################################
# Smith and Sandwell Bathymetry only (no land topography)

#Global 1-km resolution bathymetry from the 30 arc-second SRTM30+ gridded digital elevation model (DEM). Based on the Smith and Sandwell global 1 arc-minute grid between latitudes +/- 81 degrees. Higher resolution grids have been added from the LDEO Ridge Multibeam Synthesis Project, the JAMSTEC Data Site for Research Cruises, and the NGDC Coastal Relief Model. Arctic bathymetry is from the International Bathymetric Chart of the Oceans (IBCAO).</gco:CharacterString>

### This is bathymetry only (i.e. does not provide values over land, but otherwise the same) Might be better as it's really about ocean use. 
########################################################################################

tagData <- pa
dataInfo <- rerddap::info('srtm30plus_v11_bathy') 
dataInfo
parameter <- 'elev'
xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
PAbath_SS <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, ,progress_bar = TRUE)
save(PAbath_SS, file = "data-created/bathPA_SS.Rdata")

## GLS
tagData <- pa_gls
dataInfo <- rerddap::info('srtm30plus_v11_bathy') 
dataInfo
parameter <- 'elev'
xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
PAbath_SS_gls <- rxtracto(dataInfo, parameter = parameter, 
                      xcoord = xcoord, ycoord = ycoord, ,progress_bar = TRUE)
save(PAbath_SS_gls, file = "data-created/bathPA_SS_gls_ukfsst.Rdata")

tagData <- po
dataInfo <- rerddap::info('srtm30plus_v11_bathy') 
dataInfo
parameter <- 'elev'
xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
PObath_SS <- rxtracto(dataInfo, parameter = parameter, 
                      xcoord = xcoord, ycoord = ycoord, ,progress_bar = TRUE)
save(PObath_SS, file = "data-created/bathPO_SS.Rdata")

tagData <- lt
dataInfo <- rerddap::info('srtm30plus_v11_bathy') 
dataInfo
parameter <- 'elev'
xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
LTbath_SS <- rxtracto(dataInfo, parameter = parameter, 
                      xcoord = xcoord, ycoord = ycoord, ,progress_bar = TRUE)
save(LTbath_SS, file = "data-created/bathLT_SS.Rdata")



########################
### chla VIIRS
########################

# Chlorophyll, NOAA VIIRS, Science Quality, Global, Level 3, 2012-present, Daily
# nesdisVHNSQchlaDaily
# nesdisVHNSQchlaWeekly

# Chlorophyll-a, Aqua MODIS, NPP, L3SMI, Global, 4km, Science Quality, 2003-present (1 Day Composite and 8 day composite)
# erdMH1chla1day
# erdMH1chla8day

tagData <- pa
dataInfo <- rerddap::info('nesdisVHNSQchlaWeekly') 
dataInfo
parameter <- 'chlor_a'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
altitude <- rep(0., length(xcoord))
# get "Requested coordinate names do not match" error if a paramter dimension is included in the function that isn't necessary
# in the case of bathymetry, this was the tcoord (no time in the bathymetry dataset)
PAchla <- rxtracto(dataInfo, parameter = parameter, 
                   xcoord = xcoord, ycoord = ycoord, tcoord = time, zcoord = altitude, progress_bar = TRUE)
plot(pa$date, PAchla$`mean chlor_a`) # check 
save(PAchla, file = "data-created/chlaPA.Rdata")

## GLS this dataset not available for the time frame of the GLS data
# tagData <- pa_gls
# dataInfo <- rerddap::info('nesdisVHNSQchlaWeekly') 
# dataInfo
# parameter <- 'chlor_a'
# xcoord <- tagData$x
# ycoord <- tagData$y
# time <- tagData$date
# altitude <- rep(0., length(xcoord))
# PAchla_gls <- rxtracto(dataInfo, parameter = parameter, 
#                   xcoord = xcoord, ycoord = ycoord, tcoord = time, zcoord = altitude, progress_bar = # TRUE)
# plot(pa$date, PAchla_gls$`mean chlor_a`) # check 
# save(PAchla_gls, file = "data-created/chlaPA_gls_ukfsst.Rdata")

# R Extract is supposed to be capable across the dateline, but not working for chlorophyll-a at least
# So, separated into postivie and negative longitudes to pull the data and then recombine
# Positive Longitudes

######### POMARINE JAEGER ##############

po$mean_chl = NaN #initialize new column

## POSTIVE LONGITUDES
tagData <- po[po$x>0,] # All postivie longitudes
dataInfo <- rerddap::info('nesdisVHNSQchlaWeekly') 
dataInfo
parameter <- 'chlor_a'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
altitude <- rep(0., length(xcoord))
POchla_pos <- rxtracto(dataInfo, parameter = parameter, 
                   xcoord = xcoord, ycoord = ycoord, tcoord = time, zcoord = altitude, progress_bar = TRUE)

i = which(po$x>0)
po$mean_chl[i] = POchla_pos$`mean chlor_a`

## NEGATIVE LONGITUDES
tagData <- po[po$x<0,] # All postivie longitudes
dataInfo <- rerddap::info('nesdisVHNSQchlaWeekly') 
dataInfo
parameter <- 'chlor_a'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
altitude <- rep(0., length(xcoord))
POchla_neg <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, tcoord = time, zcoord = altitude, progress_bar = TRUE)
i = which(po$x<0)
po$mean_chl[i] = POchla_neg$`mean chlor_a`

plot(po$date, po$mean_chl) # check 
POchla = po
save(POchla, file = "data-created/chlaPO.Rdata")

######### LONG-TAILED JAEGER ##############
lt$mean_chl = NaN #initialize new column

## POSTIVE LONGITUDES
tagData <- lt[lt$x>0,] # All postivie longitudes
dataInfo <- rerddap::info('nesdisVHNSQchlaWeekly') 
dataInfo
parameter <- 'chlor_a'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
altitude <- rep(0., length(xcoord))
LTchla_pos <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, tcoord = time, zcoord = altitude, progress_bar = TRUE)

i = which(lt$x>0)
lt$mean_chl[i] = LTchla_pos$`mean chlor_a`

## NEGATIVE LONGITUDES
tagData <- lt[lt$x<0,] # All postivie longitudes
dataInfo <- rerddap::info('nesdisVHNSQchlaWeekly') 
dataInfo
parameter <- 'chlor_a'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
altitude <- rep(0., length(xcoord))
LTchla_neg <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, tcoord = time, zcoord = altitude, progress_bar = TRUE)
i = which(lt$x<0)
lt$mean_chl[i] = LTchla_neg$`mean chlor_a`

plot(lt$date, lt$mean_chl) # check 
LTchla = lt
save(LTchla, file = "data-created/chlaLT.Rdata")

##########################
### chla AQUA MODIS 8 day
##########################
rm(LTchla_neg,LTchla_pos,POchla_neg,POchla_pos,PAchla)

# Chlorophyll-a, Aqua MODIS, NPP, L3SMI, Global, 4km, Science Quality, 2003-present (1 Day Composite and 8 day composite)
# erdMH1chla1day
# erdMH1chla8day

tagData <- pa
dataInfo <- rerddap::info('erdMH1chla8day') 
dataInfo
parameter <- 'chlorophyll'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
# get "Requested coordinate names do not match" error if a paramter dimension is included in the function that isn't necessary
# in the case of bathymetry, this was the tcoord (no time in the bathymetry dataset)
PAchla <- rxtracto(dataInfo, parameter = parameter, 
                   xcoord = xcoord, ycoord = ycoord, tcoord = time, progress_bar = TRUE)
plot(pa$date, PAchla$`mean chlorophyll`) # check 
plot(pa$date[PAchla$`mean chlorophyll`<10], PAchla$`mean chlorophyll`[PAchla$`mean chlorophyll`<10])
save(PAchla, file = "data-created/chlaPA_aquaModis.Rdata")

## GLS
tagData <- pa_gls
dataInfo <- rerddap::info('erdMH1chla8day') 
dataInfo
parameter <- 'chlorophyll'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
PAchla_gls <- rxtracto(dataInfo, parameter = parameter, 
                   xcoord = xcoord, ycoord = ycoord, tcoord = time, progress_bar = TRUE)
plot(pa_gls$date, PAchla_gls$`mean chlorophyll`) # check 
save(PAchla_gls, file = "data-created/chlaPA_aquaModis_gls_ukfsst.Rdata")

# R Extract is supposed to be capable across the dateline, but not working for chlorophyll-a at least
# So, separated into postivie and negative longitudes to pull the data and then recombine
# Positive Longitudes

######### POMARINE JAEGER ##############

po$mean_chl = NaN #initialize new column

## POSTIVE LONGITUDES
tagData <- po[po$x>0,] # All postivie longitudes
dataInfo <- rerddap::info('erdMH1chla8day') 
dataInfo
parameter <- 'chlorophyll'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
POchla_pos <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, tcoord = time, progress_bar = TRUE)

i = which(po$x>0)
po$mean_chl[i] = POchla_pos$`mean chlorophyll`

## NEGATIVE LONGITUDES
tagData <- po[po$x<0,] # All postivie longitudes
dataInfo <- rerddap::info('erdMH1chla8day') 
dataInfo
parameter <- 'chlorophyll'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
POchla_neg <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, tcoord = time, progress_bar = TRUE)
i = which(po$x<0)
po$mean_chl[i] = POchla_neg$`mean chlorophyll`

plot(po$date, po$mean_chl) # check 
POchla = po
save(POchla, file = "data-created/chlaPO_aquaModis.Rdata")

######### LONG-TAILED JAEGER ##############
lt$mean_chl = NaN #initialize new column

## POSTIVE LONGITUDES
tagData <- lt[lt$x>0,] # All postivie longitudes
dataInfo <- rerddap::info('erdMH1chla8day') 
dataInfo
parameter <- 'chlorophyll'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
LTchla_pos <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, tcoord = time, progress_bar = TRUE)

i = which(lt$x>0)
lt$mean_chl[i] = LTchla_pos$`mean chlorophyll`

## NEGATIVE LONGITUDES
tagData <- lt[lt$x<0,] # All postivie longitudes
dataInfo <- rerddap::info('erdMH1chla8day') 
dataInfo
parameter <- 'chlorophyll'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
LTchla_neg <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, tcoord = time, progress_bar = TRUE)
i = which(lt$x<0)
lt$mean_chl[i] = LTchla_neg$`mean chlorophyll`

plot(lt$date, lt$mean_chl) # check 
LTchla = lt
save(LTchla, file = "data-created/chlaLT_aquaModis.Rdata")

####################################################
### chla AQUA MODIS 8 day RUNNING COMPOSITE
# erdMB1chla8day (MB instead of MH)
####################################################
rm(LTchla_neg,LTchla_pos,POchla_neg,POchla_pos,PAchla)


# Chlorophyll-a, Aqua MODIS, NPP, L3SMI, Global, 4km, Science Quality, 2003-present (1 Day Composite and 8 day composite)
# erdMH1chla1day
# erdMH1chla8day

tagData <- pa
dataInfo <- rerddap::info('erdMB1chla8day') 
dataInfo
parameter <- 'chlorophyll'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
# get "Requested coordinate names do not match" error if a paramter dimension is included in the function that isn't necessary
# in the case of bathymetry, this was the tcoord (no time in the bathymetry dataset)
PAchla <- rxtracto(dataInfo, parameter = parameter, 
                   xcoord = xcoord, ycoord = ycoord, tcoord = time, progress_bar = TRUE)
plot(pa$date, PAchla$`mean chlorophyll`) # check 
plot(pa$date[PAchla$`mean chlorophyll`<10], PAchla$`mean chlorophyll`[PAchla$`mean chlorophyll`<10])
save(PAchla, file = "data-created/chlaPA_aquaModis_running.Rdata")

# R Extract is supposed to be capable across the dateline, but not working for chlorophyll-a at least
# So, separated into postivie and negative longitudes to pull the data and then recombine
# Positive Longitudes

######### POMARINE JAEGER ##############

po$mean_chl = NaN #initialize new column

## POSTIVE LONGITUDES
tagData <- po[po$x>0,] # All postivie longitudes
dataInfo <- rerddap::info('erdMB1chla8day') 
dataInfo
parameter <- 'chlorophyll'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
POchla_pos <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, tcoord = time, progress_bar = TRUE)

i = which(po$x>0)
po$mean_chl[i] = POchla_pos$`mean chlorophyll`

## NEGATIVE LONGITUDES
tagData <- po[po$x<0,] # All postivie longitudes
dataInfo <- rerddap::info('erdMB1chla8day') 
dataInfo
parameter <- 'chlorophyll'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
POchla_neg <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, tcoord = time, progress_bar = TRUE)
i = which(po$x<0)
po$mean_chl[i] = POchla_neg$`mean chlorophyll`

plot(po$date, po$mean_chl) # check 
POchla = po
save(POchla, file = "data-created/chlaPO_aquaModis_running.Rdata")

######### LONG-TAILED JAEGER ##############
lt$mean_chl = NaN #initialize new column

## POSTIVE LONGITUDES
tagData <- lt[lt$x>0,] # All postivie longitudes
dataInfo <- rerddap::info('erdMB1chla8day') 
dataInfo
parameter <- 'chlorophyll'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
LTchla_pos <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, tcoord = time, progress_bar = TRUE)

i = which(lt$x>0)
lt$mean_chl[i] = LTchla_pos$`mean chlorophyll`

## NEGATIVE LONGITUDES
tagData <- lt[lt$x<0,] # All postivie longitudes
dataInfo <- rerddap::info('erdMB1chla8day') 
dataInfo
parameter <- 'chlorophyll'
#xcoord <- convlon(tagData$x)
xcoord <- tagData$x
ycoord <- tagData$y
time <- tagData$date
LTchla_neg <- rxtracto(dataInfo, parameter = parameter, 
                       xcoord = xcoord, ycoord = ycoord, tcoord = time, progress_bar = TRUE)
i = which(lt$x<0)
lt$mean_chl[i] = LTchla_neg$`mean chlorophyll`

plot(lt$date, lt$mean_chl) # check 
LTchla = lt
save(LTchla, file = "data-created/chlaLT_aquaModis_running.Rdata")



