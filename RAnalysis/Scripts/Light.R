#Project:
#Data published in: X
#Title: 
#Contact: 
#Supported by: 
#last modified:
#See Readme file for details on data files and metadata

rm(list=ls()) # removes all prior objects

#R Version: R version
#RStudio Version: 
######Read in required libraries#####
library(car) #version 2.1-4 Date: 2016-11-30 Depends: R (>= 3.2.0) Imports:MASS, mgcv, nnet, pbkrtest (>= 0.3-2), quantreg, grDevices, utils, stats, graphics, Rcpp
library(ggplot2) #version 2.2.1 Date/Publication: 2016-12-30 Depends: R (>= 3.1) Imports: digest, grid, gtable (>= 0.1.1), MASS, plyr (>= 1.7.1),reshape2, scales (>= 0.4.1), stats, tibble, lazyeval
library(gridExtra) #version: 2.2.1 Date/Publication: 2016-02-29 Depends: R(>= 2.5.0) Imports: gtable, grid, grDevices, graphics, utils
library(lsmeans)  #version: 2.26-3 Date: 2017-05-09 Depends: estimability, methods, R (>= 3.0) Imports: graphics, stats, utils, nlme, coda (>= 0.17), multcomp, plyr,mvtnorm, xtable (>= 1.8-2)
library(multcomp) #version: 1.4-6 Date: 2016-07-14 Depends: stats, graphics, mvtnorm (>= 1.0-3), survival (>= 2.39-4), TH.data (>= 1.0-2)
library(nlme) #version: 3.1-131 Date: 2017-02-06 Depends: R (>= 3.0.2) Imports: graphics, stats, utils, lattice
library(plotrix) #version: 3.6-5 Date: 2017-05-09 Depends: NA Imports: grDevices, graphics, stats, utils
library(plyr) #version: 1.8.4 Date/Publication: 2016-06-08 Depends: R (>= 3.1.0) Imports: Rcpp (>= 0.11.0)
library(reshape) #version: 3.3.1 Date/Publication: 2016-06-24  Depends: R (>= 3.3.1)
library(seacarb) #version: 3.2 Date/Publication: 2017-06-19 Depends: R (>= 2.10), oce, gsw Imports: NA
library(grid) #version: 3.3.1 Date/Publication: 2016-06-24  Depends: R (>= 3.3.1)
library(xtable) #version 1.8-2 Date/Publication: 2016-01-08 Depends: R (>= 2.10.0)
library(lme4) #version: 1.1-13 Date/Publication: 2017-04-19 Depends: R (>= 3.0.2), Matrix (>= 1.1.1), methods, stats Imports: graphics, grid, splines, utils, parallel, MASS, lattice, nlme(>= 3.1-123), minqa (>= 1.1.15), nloptr (>= 1.0.4)
library(blmeco) #version: 1.1 Date/Publication: 2015-08-22 Depends: R (>= 3.0.0), stats, MASS Imports: MuMIn, arm, lme4
library(MuMIn) #version: 1.15.6 Date/Publication: 2016-01-07 Depends: R (>= 3.0.0) Imports: graphics, methods, Matrix, stats, stats4

#############################################################
setwd("~/MyProjects/Montipora-Colormorphs-Project/RAnalysis/Data") #set working directory

#############################################################

##### CALIBRATIONS #####
#Light Logger Calibrations
#control logger serial number: 6383 y= 0.1219x + 0
#deep logger serial number: 6376 y= 0.0.1149x + 0
#shade logger serial number: 6375 y= 0.1319x + 0
#shallow logger serial number: 7273 y= 0.1264x + 0

Cal.L.data <- read.csv("Light Data/All Light Data.csv", header=TRUE, sep=",", na.strings="NA") #load data with a header, separated by commas, with NA as NA

Cal.L.data[Cal.L.data == 0] <- NA

Control <- subset(Cal.L.data, Treatment=="Control")
Shallow <- subset(Cal.L.data, Treatment=="Shallow")
Deep <- subset(Cal.L.data, Treatment=="Deep")
Shade <- subset(Cal.L.data, Treatment=="Shade")

#Run linear models of loggers against standard and extract model coeffecients to be applied to raw data
Control$umolm2s1 <- (0.1219*Control$Raw.Value)/(60*60) 
Shallow$umolm2s1 <- (0.1264*Shallow$Raw.Value)/(60*60)
Shallow <- Shallow[1:nrow(Control),]
Deep$umolm2s1 <- (0.1149*Deep$Raw.Value)/(60*60)
Deep <- Deep[1:nrow(Control),]
Shade$umolm2s1 <- (0.1319*Shade$Raw.Value)/(60*60)
Shade <- Shade[1:nrow(Control),]

Light <- cbind(Control, Shallow$umolm2s1, Deep$umolm2s1, Shade$umolm2s1)
colnames(Light) <- c("Logger", "Treatment", "Date", "Time", "Raw", "Cal", "Junk", "Control", "Shallow", "Deep", "Shade")

Light <- rbind(Control, Shallow, Deep, Shade)
plot(Light$Treatment, Light$umolm2s1, las=2)

mod <- lm(umolm2s1 ~ Treatment, data=Light)
summary(mod)
hist(mod$residuals)
boxplot(mod$residuals)
