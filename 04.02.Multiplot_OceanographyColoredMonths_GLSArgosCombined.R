library(ggplot2)
library(rerddapXtracto)
library(rerddap) 
library("dichromat")
library(Rmisc) #multiplot function
library(PBSmapping)
library(maptools)

# Plot land function using PBS mapping for a Pacific Centered simple (but unprojected) plotting function
data(wrld_simpl)
data(worldLLhigh)

plotland <- function(){
  for(i in 0:length(unique(worldLLhigh$PID))) polygon(worldLLhigh$X[worldLLhigh$PID == i], worldLLhigh$Y[worldLLhigh$PID == i], col = grey(0.4), lty="blank")
}

plotlanda <- function(){
  plot(wrld_simpl, col = "darkgray", border = "black", lwd = 1, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, add=TRUE)
}

## Plot all individuals oceanography

# To Do
# 1) For LTJA 1 map, use only first year of data

#Converting +ve/-ve longitude to +ve longitude
convlon <- function(lon){
  ifelse(lon < 0, lon + 360, lon)
}

####################################
## LOAD foieGras Modeled Argos
# variables  = po, pa, lt, d. d = all species bound together
####################################

# output of "CombiningOceanographyWithModeledData
load("data-created/3stateHMM_24hr_oceanography.Rdata")
d = d[,c(1,4:6,12:15)] # to get same columns as gls data and combine into single plot

# exclude second year of LTJA-2019-CAN-01 so all plots go from july to july
d = d[d$date < "2020-07-01 00:00:00 GMT",]

d$month = lubridate::month(d$date)
d$julian = as.numeric(format(d$date, "%j"))

## LTJA-2019-CAN-02 (tagged on June 23 so only has a week of points on land) and PAJA_CAN_01_2018 need june cut out for plots that go from July to June (PAJA 01 was tagged on June 28, so only has three points on land)
# PAJA 02 needs April, May, June cut out, and possible March from when the tag was transmitting from land

del = which(d$ID %in% c("LTJA-2019-CAN-02","PAJA_CAN_01_2018") & d$month ==6)
del2 = which(d$ID == "PAJA_CAN_02_2018" & d$month %in% c(3,4,5,6))
del = c(del,del2)
d = d[-del,]

####################################
## LOAD modeled GLS data & Raw data
# remotely sensed oceanography attached. 
# Output of "CombiningOceanographyWithModeledData
####################################

load("data-created/ukfsstGLS_filtered_finaltracks_RemoteOceanography.Rdata") # d_gls
d_gls = pa_gls
d_gls$julian = as.numeric(format(d_gls$date, "%j"))

# load Raw gls data (for tag-recorded SST)
d_gls_raw = read.csv("data/paja_f.csv")
d_gls_raw$Date = as.Date(d_gls_raw$Date, format = "%m/%d/%y")

d_gls_raw$month = lubridate::month(d_gls_raw$Date)
d_gls_raw$julian = as.numeric(format(d_gls_raw$Date, "%j"))

##################################################
## COMBINE Argos and GLS into single data frame
## Ignore the fact that chla are actually from different sensors because of data of GLS data
## put that into the caption
## Doing all of this to put all the data on the same scale
##################################################

d_gls = d_gls[,c(7,4,5,6,8:11,2,13)]
names(d_gls) = names(d)
d_gls = d_gls[d_gls$ID %in% c("234","281"),] #use only the first year of data
i = which(d_gls$ID=="234")
d_gls$ID[i]="PAJA-2010-NS-01"
i = which(d_gls$ID=="281")
d_gls$ID[i]="PAJA-2011-NS-02"

d = rbind(d,d_gls)

#Categorical.12
# 12 categorical colors suitable for red-green color blind from the dichromat package
# colorschemes$Categorical.12
# palette(colorschemes$Categorical.12)

########################################################
#PLOT by BINNED MONTHS (all monthly oceanography values on same column)
########################################################

library(plyr)
d$month_julSt = mapvalues(d$month, from = c(1:12), to=c(7,8,9,10,11,12,1,2,3,4,5,6))

#d$month_julSt = mapvalues(d$month, from = c(1:12), to=c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun"))

# starts x axis at July

######
# Plot BATHYMETRY by binned months
######

d = d[d$meanElev_SmithSand<100,] #remove super high elevations

pdf(file="plotting_outputs/FAC_bathymetry_binnedMonths_withGLS.pdf", width = 13.75, height=
      2.75)
  ggplot(data = d, aes(x = month_julSt, y = meanElev_SmithSand, group = ID)) +  
    facet_grid(.  ~ ID) +
    geom_point(aes(color = as.factor(month_julSt)), alpha = 0.4, shape=16, size=2) + 
    geom_smooth(span = 0.4, color ="black", size = 0.5, fullrange=TRUE) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background=element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) +  
    scale_x_continuous(breaks = c(1:12), labels=c("J","A","S","O","N","D","J","F","M","A","M","J")) +
    xlab("Month") +
    ylab("Depth") +
    scale_y_continuous(limits = c(-6700, 100))
dev.off()


# Plot Chlorophyll by binned months

pdf(file="plotting_outputs/FAC_chl_binnedMonths_withGLS.pdf", width = 13.75, height=
      2.75)
  ggplot(data=d, aes(x = month_julSt, y=log(meanChla_VIIRSwk), group=ID)) +  
    facet_grid(.  ~ ID) +
    geom_point(aes(color = as.factor(month_julSt)), alpha = 0.4, shape=16, size=2) + 
    geom_smooth(span = 0.4, color="black", size = 0.5) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background=element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + 
    scale_x_continuous(breaks = c(1:12), labels=c("J","A","S","O","N","D","J","F","M","A","M","J")) +
    scale_y_continuous(breaks = c(-4,-3,-2,-1,0,1,2)) +
    xlab("Month") +
    ylab("log(Chlorophyll") 
dev.off()
# inverse log
# 10^-2.5

# to have a spot blank in the axis labes, just have labels = "", i.e. c("Jul","","Oct")

# Plot SST by binned months
pdf(file="plotting_outputs/FAC_SST_binnedMonths_withGLS.pdf", width = 13.75, height=
      2.75)
  ggplot(data=d, aes(x = month_julSt, y=meanSST, group=ID)) +  
    facet_grid(.  ~ ID) +
    geom_point(aes(color = as.factor(month_julSt)), alpha = 0.4, shape=16, size=2) + 
    geom_smooth(span = 0.4, color="black", size = 0.5, fullrange=FALSE) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background=element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + 
    scale_x_continuous(breaks = c(1:12), labels=c("J","A","S","O","N","D","J","F","M","A","M","J")) +
    scale_y_continuous(label = label_number(suffix = "°C"), limits = c(-5, 32)) +
    xlab("Month") +
    ylab("SST") 
dev.off()

# labels=c("Jul","","","Oct","","","Jan","","","Apr","","")

###########
# GLS RAW INSITU SST
###########
d_gls_raw$month_julSt = mapvalues(d_gls_raw$month, from = c(1:12), to=c(7,8,9,10,11,12,1,2,3,4,5,6))

# Plot by binned months
pdf(file="plotting_outputs/FAC_glsSST_InSitu_binnedMonths.pdf", width = 7.4, height=
      2.75)
    ggplot(data=d_gls_raw, aes(x = month_julSt, y=SST, group=Geo)) +  
      facet_grid(.  ~ Geo) +
      geom_point(aes(color = as.factor(month_julSt)), alpha = 0.4, shape=16, size=2) + 
      geom_smooth(span = 0.4, color="black", size = 0.5) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background=element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5)) + 
      scale_x_continuous(breaks = c(1:12), labels=c("J","A","S","O","N","D","J","F","M","A","M","J")) +
      scale_y_continuous(label = label_number(suffix = "°C"), limits = c(-5, 32)) +
      xlab("Month") +
      ylab("SST") 
dev.off()

# Leftover from when I used a mean for the month instead of showing all points. Idea was to show a point and a spread
#+ geom_pointrange(aes(color=as.factor(month),ymin=meanSST-sd, ymax=meanSST+sd)) 

########################################################
# Plot All values (full daily time series of points)
# Maybe too much information to ingest
# By binning by month you can see the range in each month 
# Plus th emean pattern. I like it.
########################################################

s = ggplot(data=d, aes(x = date, y=meanSST, group=ID)) +  facet_grid(.  ~ ID) + scale_y_continuous(label = label_number(suffix = "°C")) + ggtitle("SST")+
  geom_point(aes(color=as.factor(month)), alpha=0.25) +
  geom_smooth(span = 0.3, color="black", size = 0.5) +
  #geom_smooth(color="black", method = "gam", formula = y ~ s(x, bs = "cs")) +
  #geom_point(alpha=0.15) +
  xlab("Date") +
  ylab("SST") +
  scale_x_datetime(labels = label_date_short()) +  
  theme_classic() + 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(), 
    legend.position = "none"
  )


b = ggplot(data=d, aes(x = date, y=meanElev_SmithSand, group=ID)) +  facet_grid(.  ~ ID) + scale_y_continuous(labels = scales::comma) + ggtitle("Bathymetry")+
  #geom_point(alpha=0.15) +
  geom_point(aes(color=as.factor(month)), alpha=0.25) +
  geom_smooth(span = 0.3, color="black",size = 0.5) +
  xlab("Date") +
  ylab("Depth") +
  theme_classic()+ 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(), 
    legend.position = "none"
  )

# facet_grid(.  ~ ID, scales="free")
c = ggplot(data=d, aes(x = date, y=log(meanChla_VIIRSwk), group=ID)) +  facet_grid(.  ~ ID) + scale_y_continuous(labels = scales::comma) + ggtitle("chlorophyll-a")+
  geom_point(aes(color=as.factor(month)), alpha=0.25) +
  geom_smooth(span = 0.3, color="black",size = 0.5) +
  #geom_point(alpha=0.15) +
  xlab("Date") +
  ylab("chlorophyll") +
  theme_classic()+ 
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(), 
    legend.position = "none"
  )

### Inverse natural log
## To label the y axis
exp(2.5)
exp(-2.5)
exp(0)


multiplot(s, b, c,rows=3)

##########################################
# PLOTTING MAPS
##########################################

######################
# Get GGPLOT 2 COLORS
######################
library("scales")                                       # Load scales R package
hex_codes1 <- hue_pal()(12)                             # Identify hex codes
hex_codes1    
show_col(hex_codes1)   

d$ggplotCol = mapvalues(d$month_julSt, from = c(1:12), to=hex_codes1)

plotlanda <- function(){
  plot(wrld_simpl, col = "lightgray", border = "black", lwd = 0.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, add=TRUE)
}

# Argos 
pdf(file="plotting_outputs/map_LTJA_01_year1.pdf", height = 3.9, width=3)
  ind = d[d$ID=="LTJA-2019-CAN-01",]
  plot(ind$x+2,ind$y, pch=19, col="white")
  plotlanda()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=19, col=alpha(ind$ggplotCol,0.55))
  title(main="LTJA-2019-CAN-01")
dev.off()

pdf(file="plotting_outputs/map_LTJA_02.pdf", height = 3.9, width=3)
  ind = d[d$ID=="LTJA-2019-CAN-02",]
  plot(ind$x,ind$y, pch=19, col="white")
  plotlanda()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=19, col=alpha(ind$ggplotCol,0.55))
  title(main="LTJA-2019-CAN-02")
dev.off()

#points(ind$x, ind$y, pch=19, col=alpha(ind$month_julSt,0.55)) # Rdefault color scheme


pdf(file="plotting_outputs/map_PAJA_01.pdf", height = 3.9, width=3)
  ind = d[d$ID=="PAJA_CAN_01_2018",]
  plot(ind$x,ind$y, pch=19, col="white")
  plotlanda()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=19, col=alpha(ind$ggplotCol,0.55))
  title(main="PAJA_CAN_01_2018")
dev.off()

# Argos Parasitic Jaegers
pdf(file="plotting_outputs/map_PAJA_02_NoNicLandPositions.pdf", height = 3.9, width=3)
  ind = d[d$ID=="PAJA_CAN_02_2018",]
  plot(ind$x,ind$y, pch=19, col="white")
  plotlanda()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=19, col=alpha(ind$ggplotCol,0.55))
  title(main="PAJA_CAN_02_2018")
dev.off()

################################
### SMALLER POINT SIZE
# Argos 
pdf(file="plotting_outputs/map_LTJA_01_year1_smPoints.pdf", height = 3.9, width=3)
  ind = d[d$ID=="LTJA-2019-CAN-01",]
  plot(ind$x+2,ind$y, pch=19, col="white")
  plotlanda()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=20, col=alpha(ind$ggplotCol,0.55))
  title(main="LTJA-2019-CAN-01")
dev.off()

pdf(file="plotting_outputs/map_LTJA_02_smPoints.pdf", height = 3.9, width=3)
  ind = d[d$ID=="LTJA-2019-CAN-02",]
  plot(ind$x,ind$y, pch=19, col="white")
  plotlanda()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=20, col=alpha(ind$ggplotCol,0.55))
  title(main="LTJA-2019-CAN-02")
dev.off()

#points(ind$x, ind$y, pch=19, col=alpha(ind$month_julSt,0.55)) # Rdefault color scheme


pdf(file="plotting_outputs/map_PAJA_01_smPoints.pdf", height = 3.9, width=3)
  ind = d[d$ID=="PAJA_CAN_01_2018",]
  plot(ind$x,ind$y, pch=19, col="white")
  plotlanda()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=20, col=alpha(ind$ggplotCol,0.55))
  title(main="PAJA_CAN_01_2018")
dev.off()

# Argos Parasitic Jaegers
pdf(file="plotting_outputs/map_PAJA_02_NoNicLandPositions_smPoints.pdf", height = 3.9, width=3)
  ind = d[d$ID=="PAJA_CAN_02_2018",]
  plot(ind$x,ind$y, pch=19, col="white")
  plotlanda()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=20, col=alpha(ind$ggplotCol,0.55))
  title(main="PAJA_CAN_02_2018")
dev.off()

plotland <- function(){
  for(i in 0:length(unique(worldLLhigh$PID))) polygon(worldLLhigh$X[worldLLhigh$PID == i], worldLLhigh$Y[worldLLhigh$PID == i], col = "lightgrey", border = "black",lwd = 0.5)
}

#########
# POMARINE JAEGER 0-360 version
#########

library(maptools)
# ? nowrapRecenter
## see reference manual for long function to do this...still left lines on the map with plotland function but those were easily removed in illustrator...
run <- FALSE
if (require(maps)) run <- TRUE
## Not run:
if (run) {
  world <- map("world", fill=TRUE, col="transparent", plot=FALSE)
  worldSpP <- map2SpatialPolygons(world, world$names, CRS("+proj=longlat +ellps=WGS84"))
  worldSpP <- worldSpP[-grep("Antarctica", row.names(worldSpP)),]
  # incomplete polygons
  worldSpP <- worldSpP[-grep("Ghana", row.names(worldSpP)),]
  # self-intersection mouth of Volta
  worldSpP <- worldSpP[-grep("UK:Great Britain", row.names(worldSpP)),]
  # self-intersection Humber estuary
  worldSpPr <- recenter(worldSpP)
  plot(worldSpPr)
  title("Pacific view without polygon splitting")
}
if (run) {
  worldSpPnr <- nowrapRecenter(worldSpP)
  plot(worldSpPnr)
  title("Pacific view with polygon splitting")
}

plotland_split <- function(){
  plot(worldSpPr, col = "lightgray", border = "black", lwd = 0.5, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, add=TRUE)
}


# Argos Pomarine Jaeger
pdf(file="plotting_outputs/map_POJA_smPoints.pdf", height = 3.9, width=3)
  ind = d[d$ID=="POJA-2019-CAN-01",]
  ind$x =convlon(ind$x)
  plot(ind$x,ind$y, pch=19, col="white")
  plotland_split()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=20, col=alpha(ind$ggplotCol,0.55))
  title(main="POJA_01")
dev.off()

###########################
# MAP MODELED GLS DATA
###########################
# load modeled gls data
load("data-created/ukfsstGLS_filtered_finaltracks.Rdata") #object = ukfsstGLS
gls = ukfsstGLS
gls$month_julSt = mapvalues(gls$month, from = c(1:12), to=c(7,8,9,10,11,12,1,2,3,4,5,6))
gls$ggplotCol = mapvalues(gls$month_julSt, from = c(1:12), to=hex_codes1)

# GLS Parasitic Jaegers
pdf(file="plotting_outputs/PAJA_234_GLS.pdf", height = 3.9, width=3)
  ind = gls[gls$tagID=="234",]
  plot(ind$x,ind$y, pch=19, col="white")
  plotlanda()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=20, col=alpha(ind$ggplotCol,0.55))
  title(main="PAJA 234 GLS")
dev.off()

# Argos Parasitic Jaegers
pdf(file="plotting_outputs/PAJA_281_GLS.pdf", height = 3.9, width=3)
  ind = gls[gls$tagID=="281",]
  plot(ind$x,ind$y, pch=19, col="white")
  plotlanda()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=20, col=alpha(ind$ggplotCol,0.55))
  title(main="PAJA 281 GLS")
dev.off()

pdf(file="plotting_outputs/PAJA_300_GLS.pdf", height = 3.9, width=3)
  ind = gls[gls$tagID=="300",]
  plot(ind$x,ind$y, pch=19, col="white")
  plotlanda()
  lines(ind$x, ind$y, pch=19, col=alpha("black",0.75))
  points(ind$x, ind$y, pch=20, col=alpha(ind$ggplotCol,0.55))
  title(main="PAJA 300 (year 2, 234) GLS")
dev.off()

###############################################
## Plotting States
###############################################


## PLOTTING PREDICTED STATES (1 is stationary)
ggplot(data=d, aes(x = date, y=probState2, group=ID)) +  facet_grid(.  ~ ID, scales="free") + 
  geom_point(aes(color=as.factor(month))) + theme_classic()

# color by State (State 1 is breeding and shed)
# State 2 is stationary period
# State 3 is migration (but predicted State column only has 1 and 3? classified as 1 and 2? the probs and the binary don't match up)
ggplot(data=d, aes(x = x, y=y, group=ID)) +  geom_point(aes(color=probState2)) + facet_grid(.  ~ ID, scales="free") + ggtitle("Locations")+ theme_classic()

ggplot(data=d, aes(x = x, y=y, group=ID)) +  geom_point(aes(color=probState2)) + facet_grid(.  ~ ID, scales="free") + ggtitle("Locations")+ theme_classic()



## Original attempt at plotting facet plots of months

# ## Nah, dots too big
# world_map <- map_data("world")
# pdf(file="plotting_outputs/FAC_maps_binnedMonths.pdf", width = 11, height=
#       3)
# ggplot(world_map, aes(x = long, y = lat, group = group)) +
#   geom_polygon(fill="lightgray", colour = "darkgray") + geom_point(data = d, aes(x=x,y=y,group=ID,color=as.factor(month))) + facet_grid(.  ~ ID, scales="free") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background=element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=0.5))
# dev.off()
# 
# p = ggplot(data=d, aes(x = x, y=y, group=ID)) +  geom_point(aes(color=as.factor(month))) + facet_grid(.  ~ ID, scales="free") + scale_y_continuous(labels = scales::comma) + ggtitle("Locations")+
#   theme_classic(legend.position = "none")
