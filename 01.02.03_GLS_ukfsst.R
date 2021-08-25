#Autumn-Lynn Harrison, HarrisonAL@si.edu
# GLS data contributed by Mark Mallory, mark.mallory@acadiau.ca. Daily SST recorded by the tag, not wet/dry or sunrise/sunset

# Try Ukfsst to process the raw geolocation tracks

# install kftrack/ukfsst from source. This install function (install.r, has 64-bit install set to False, but Catalina does not support 32 bit, modified install function and will try again SST)
# if (!any('devtools' == installed.packages()[,"Package"])){
#  install.packages('devtools', repos="http://cran.rstudio.com/")}
# require(devtools)
# source_url("https://raw.githubusercontent.com/positioning/kalmanfilter/master/install.r")

# install.packages("/Users/autumn-lynn/Downloads/ukfsst_0.3-x64.tar.gz", repos = NULL, type="source")
# install.packages("/Users/autumn-lynn/Downloads/kftrack_0.70-x64.tar.gz", repos = NULL, type="source")

library(lubridate)
library(kftrack) 
library(ukfsst)

# load raw geolocator data for Parasitic Jaegers provided by Mark Mallory
d = read.csv("data/paja_f.csv")
# 1 position per day, no time given (only date)

d$Date = as.POSIXct(d$Date, format = "%m/%d/%y", tz="GMT")

# i = d
# Troubleshooting: Date range looks correct
# Troubleshooting:test test by individual in case it's an individual-level issue (ids = 234, 281, 300)

## Try limiting data to September 1st onward. 
# That should also remove the period of time with 24 hour daylight when Longitude locations are very wonky

# Might also need to run a simple speed-distance-angle filter before processing

#Converting +ve/-ve longitude to +ve longitude
convlon <- function(lon){
  ifelse(lon < 0, lon + 360, lon)
}

d$LON = convlon(d$LON)

########################
# 234
########################

i = d[d$Geo=="234",] 
i = i[i$LAT>0,] # remove position <0
plot(i$Date, i$LON)
i = i[i$Date>"2010-08-25",]
plot(i$Date, i$LON)

i = i[i$Date<"2011-06-01",] #311
plot(i$Date, i$LON)

track <- data.frame(day = day(i$Date),
                    month = month(i$Date),
                    year = year(i$Date),
                    obsLon = as.numeric(i$LON),
                    obsLat = as.numeric(i$LAT),
                    obsSst = i$SST)

sst.path <- get.sst.from.server(track, removeland=FALSE) #birds can fly over land 
#sst.path <- get.blended.sst(track) # # for usage with higher-resolution SST imagery
#sst.path <- get.avhrr.sst(track) 

fit<-kfsst(track, fix.first = FALSE, fix.last = FALSE, var.struct="solstice") # Need to add in known deployment and recovery locations (ask Mark), but for now, set to FALSE

plot.kfsst(fit)
fit234 = fit
rm(fit)

########################
# 281
########################

i = d[d$Geo=="281",] 
plot(i$Date, i$LON)
# i = i[i$LAT>0,] # remove position <0 for GEO 234
i = i[i$Date>"2011-08-31",] 

plot(i$Date, i$LON)
i = i[i$Date<"2012-05-31",] #the last position on may 31 was throwing off the model
plot(i$Date, i$LON)
i = i[-11,] #9/14/2011...throwing off equinox

track <- data.frame(day = day(i$Date),
                    month = month(i$Date),
                    year = year(i$Date),
                    obsLon = as.numeric(i$LON),
                    obsLat = as.numeric(i$LAT),
                    obsSst = i$SST)

sst.path <- get.sst.from.server(track, removeland=FALSE) #birds can fly over land 
#sst.path <- get.blended.sst(track) # # for usage with higher-resolution SST imagery, time consuming call, but works
#sst.path <- get.avhrr.sst(track) 

fit<-kfsst(track, fix.first = FALSE, fix.last = FALSE, var.struct="solstice") 
#fit<-kfsst(track, fix.first = FALSE, fix.last = FALSE, var.struct="uniform") 

plot.kfsst(fit)
fit281 = fit
plot.kfsst(fit281)
rm(fit)

########################
# 300
########################

i = d[d$Geo=="300",] 
plot(i$Date, i$LON)

# i = i[i$LAT>0,] # remove position <0 for GEO 234
i = i[i$Date>"2011-08-31",] #311
plot(i$Date, i$LON)

i = i[i$Date<"2012-06-05",] #311
plot(i$Date, i$LON)

track <- data.frame(day = day(i$Date),
                    month = month(i$Date),
                    year = year(i$Date),
                    obsLon = as.numeric(i$LON),
                    obsLat = as.numeric(i$LAT),
                    obsSst = i$SST)

sst.path <- get.sst.from.server(track, removeland=FALSE) #birds can fly over land 
#sst.path <- get.blended.sst(track) # # for usage with higher-resolution SST imagery, time consuming call, but works
#sst.path <- get.avhrr.sst(track) 

fit<-kfsst(track, fix.first = FALSE, fix.last = FALSE, var.struct="solstice") 
#fit<-kfsst(track, fix.first = FALSE, fix.last = FALSE, var.struct="uniform") 

plot.kfsst(fit)
fit300 = fit
rm(fit)

save(fit234,fit281,fit300, file = "data-created/ukfsst_GLS.Rdata")

## This looks the most reasonable of any I've seen, but why is SST zero in the predictions (because no predicated SST?) 
# A previous version showed predicted SST values...how do you know it it is pulling and comparing SST?
# I think it's not actually pulling the correct SST fields...it indicates in step 1 that the "individual is outside of the map" and doesn't show the same output as the blue shark example. I think it's fitting just the simple Kalman filter

# what it should be
# head(fit$SST)
#       o       p     smooth
# [1,] 24.73 24.6965 24.6965
# [2,] 24.37 24.4923 24.5424
#       
# # What mine is:
# head(fit$SST)
#       o p smooth
# [1,] -1.30 0      0
# [2,] -1.40 0      0
# [3,]  1.84 0      0
# [4,]  2.12 0      0
# [5,]  3.26 0      0
# [6,]  5.04 0      0
# 
# # should I compare to the version without SST?




