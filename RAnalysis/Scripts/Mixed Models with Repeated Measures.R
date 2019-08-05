#Mix Models with Repeated Measures ANOVA
library("lme4")
library("carData")
library("nlme")

rm(list=ls()) #clears workspace 

#Set Working Directory
#setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Zoox Data")#set working directory
setwd("~/MyProjects/Montipora-Colormorphs-Project/RAnalysis/Data") #set working directory

#_________________________________#
#Trying with Zoox Data First
Data1 <- read.csv("Metadata.csv") #Data for only Shade & Shallow treatment 
Data1[Data1 == 0] <- NA #All zeroes in data will just be referred to as NA

#Mixed Effects Three Way Repeated Measures ANOVA

# All.Means1 <- ddply(Data1, c("timepoint", "frag.id", "Treatment", "Colormorph"), summarize,
#                     mean= mean(sym.density, na.rm=T), #mean pnet
#                     N = sum(!is.na(sym.density)), # sample size
#                     se = sd(sym.density, na.rm=T)/sqrt(N)) #SE
# All.Means1
# All.Means1$se[is.na(All.Means1$se)] <- 0
# All.Means1$Group <- paste(All.Means1$timepoint, All.Means1$Treatment, All.Means1$Colormorph)
# All.Means1$SpGroup <- paste(All.Means1$Treatment, All.Means1$Colormorph)

# attach(All.Means1)
# Time = factor(timepoint)
# Group = factor(Colormorph)
# Group2 = factor(Treatment)
# Subject = factor(frag.id)


model2 <- lm(protein.density~ Treatment + Colormorph+timepoint + timepoint:Treatment:Colormorph, data = Data1)
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