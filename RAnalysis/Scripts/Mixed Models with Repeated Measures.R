#Mix Models with Repeated Measures ANOVA
library("lme4")
library("carData")
library("nlme")

rm(list=ls()) #clears workspace 

#Set Working Directory
setwd("~/URI/Masters Thesis") #OneDrive is not updating

Data1 <- read.csv("Metadata.csv", header=T, sep=",", na.string="NA") #read in data file
Data1[Data1 == 0] <- NA #All zeroes in data will just be referred to as NA

#Mixed Effects Three Way Repeated Measures ANOVA
# Analysis of Wicksell Data with missing values
attach(Data1)
Time = factor(timepoint)
Group = factor(Colormorph)
Group2 = factor(Treatment)
Subject = factor(frag.id)
library(lme4)


model2 <- lmer(sym.density~timepoint + Group + Group2 + Time:Group:Group2 + (1|Subject))
summary(model2)
anova(model2)
fixef(model2)
ranef(model2)

# Now use the following model which allows for groups to have different slopes over time.
# Compare this with the SAS model with an unstructured correlation matrix.
model3 <- lmer(dv~Time + Group + Time:Group + (Group|Subject))
## For now ignore the error message
summary(model3)
anova(model3)