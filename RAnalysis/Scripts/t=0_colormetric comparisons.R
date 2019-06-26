#Read in required libraries
##### Include Versions of libraries
library("vegan")
library("ggpubr")
library("gridExtra")
library("plyr") 
library("lsmeans")
library("multcompView")

# Set Working Directory:
setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Color Data/Colormetric Data_2019") #set working

#COLORMETRIC COMPARISONS
Data <- read.csv("Colormetric_Data_2019.csv", header=T, sep=",", na.string="NA") #read in data file
Data$Red.Norm.Coral <- Data$Red.Coral/Data$Red.Standard #normalize to color standard
Data$Green.Norm.Coral <- Data$Green.Coral/Data$Green.Standard #normalize to color standard
Data$Blue.Norm.Coral <- Data$Blue.Coral/Data$Blue.Standard #normalize to color standard

t0=subset(Data, Timepoint == "0")

boxplot(Red.Norm.Coral~Colormorph, data = t0, main = "Color Morph Red Channel Comparisons", xlab = "Color Morph", ylab = "Normalized Red values", col = c("orange", "brown"))
boxplot(Green.Norm.Coral~Colormorph, data = t0, main = "Color Morph Green Channel Comparisons", xlab = "Color Morph", ylab = "Normalized Red values", col = c("orange", "brown"))
boxplot(Blue.Norm.Coral~Colormorph, data = t0, main = "Color Morph Blue Channel Comparisons", xlab = "Color Morph", ylab = "Normalized Red values", col = c("orange", "brown"))

t.test(Red.Norm.Coral~Colormorph, data = t0)
t.test(Green.Norm.Coral~Colormorph, data = t0)
t.test(Blue.Norm.Coral~Colormorph, data = t0)
