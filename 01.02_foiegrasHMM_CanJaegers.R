# title: "FoieGras_Jaegers"
# author: "Autumn-Lynn Harrison"
# date: May 12, 2020, edited December 15, 2020

# April 22, 2021. Error when running foieGras:
# Error in .Call("FreeADFunObject", ptr, PACKAGE = DLL) : 
  "FreeADFunObject" not available for .Call() for package "foieGras"
# James Grecian similar experience and recommended installing TMB from source
# https://github.com/ianjonsen/foieGras/issues/25
# Did not work for me: After installing TMB from source, ran into same problem. I installed matrix from source. Then uninstalled TMB and reinstalled TMB from cran and that solved the problem 

# install.packages("matrix", type = "source") 
# install.packages("TMB", type = "source") 
# install.packages("glmmTMB", type="source")
# install.packages("foieGras", type = "source") 

library(foieGras)
library(tidyverse)
library(moveHMM)
library(ggplot2)
library(sf)
library(rnaturalearthdata) 
# data(world) # world not found

#load("CanJaegerData.RData")
load("data/CanJaegerData_2020_1215.RData")

# Run Argos filter

# Prepare Dataframe for foieGras
fg <- data.frame(id = d$individual_local_identifier,
                 date = as.POSIXct(d$timestamp,format = "%Y-%m-%d %H:%M:%S"),
                 lc = as.character(d$argos_lc),
                 lon = d$location_long,
                 lat = d$location_lat,
                 smaj = d$argos_semi_major,
                 smin = d$argos_semi_minor,
                 eor = d$argos_orientation) 

# remove any rows with missing values for date
fg <- fg[!is.na(fg$date),]

# remove any rows with missing values for lat and long
fg <- fg[!is.na(fg$lat),]
fg <- fg[!is.na(fg$lon),]

# Fit model using 24-hour time step
fit24 <- fit_ssm(fg, 
                 time.step = 24, 
                 vmax = 25, 
                 ang = c(5,15), 
                 min.dt = 10, 
                 model = "rw") 
save(fit24, file = "data-created/foiegras_24.Rdata")

# Plot individual panels with error ellipses
plot(fit24, what = "p", type = 2, ncol=5)

# to extract the lats and longs in an easy dataframe for plotting on a map:
ll = grab(fit24, what = "predicted", as_sf = FALSE) 
head(ll)

#############################################
# Hidden Markov Model
#############################################
g = grab(fit24, what = "predicted", as_sf = FALSE) # returns dataframe with lat and long
g = data.frame(g[,1:4]) #retain only the 4 columns required by moveHMM
colnames(g)[1] <- "ID" #rename for format required by moveHMM (uppercase ID instead of id)
md <- prepData(g, coordNames = c("lon","lat"))

# returns an sf object with projected mercator geometry
# gsf = grab(fit24, what = "predicted", as_sf = TRUE) 

## initial parameters for gamma and von Mises distributions
## TWO STATE MODEL
mu0 <- c(1,30) #(state1,state2)
sigma0 <- c(50,50) #(state1,state2)
zero_mass0 <- c(0,1)
stepPar0 <- c(mu0,sigma0)
angleMean0 <- c(pi,0)
kappa0 <- c(1,1) # angle concentration
anglePar0 <- c(angleMean0,kappa0)

m <- fitHMM(data=md, 
            nbStates = 2, 
            stepPar0 = stepPar0, 
            anglePar0 = anglePar0, 
            formula = ~1)

# Plot the results #
plot(m, plotCI = TRUE)
# Two state model combines breeding/stationary/ARS into a single state

# THREE STATE MODEL
# initial parameters pulled completely out of thin air?? How to decide?
mu0 <- c(1,15,30)
sigma0 <- c(50,50,100)
zeromass0 <- c(0,1,1)
stepPar0 <- c(mu0,sigma0)
angleMean0 <- c(pi,pi,0)
kappa0 <- c(1,1,1)
anglePar0 <- c(angleMean0,kappa0)

m3 <- fitHMM(data=md, 
             nbStates = 3, 
             stepPar0 = stepPar0, 
             anglePar0 = anglePar0, 
             formula = ~1)

# Plot the results #
plot(m3, plotCI = TRUE)

# Three state model seems like it correctly separates breeding from migration from staging and over-wintering. 
AIC(m,m3)

# Model      AIC
# 1    m3 24053.94
# 2     m 24886.88

# In terms of AIC, the 3-state model is favoured over the 2-state model in this example and does seem to correctly separate breeding/stationary from transit and foraging

# Get the states #
states <- viterbi(m3)
state_probs <- stateProbs(m3)

plotStates(m3)

###########################
### SAVE OUT TWO STATE MODEL
###########################

# Add states to the data set #
md$predictedState <- viterbi(m)
md$probState1 <- stateProbs(m)[,1]
md$probState2 <- stateProbs(m)[,2]

md24 <- md
save(md24, file = "data-created/2stateHMM_24hr.Rdata")

###########################
### SAVE OUT THREE STATE MODEL
###########################

# Add states to the data set #
md$predictedState <- viterbi(m)
md$probState1 <- stateProbs(m3)[,1]
md$probState2 <- stateProbs(m3)[,2]
md$probState3 <- stateProbs(m3)[,3]

md24 <- md
save(md24, file = "data-created/3stateHMM_24hr.Rdata")



