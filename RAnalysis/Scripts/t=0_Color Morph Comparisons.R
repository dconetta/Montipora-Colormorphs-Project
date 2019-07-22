#Read in required libraries
##### Include Versions of libraries
library("vegan")
library("ggpubr")
library("gridExtra")
library("plyr") 
library("lsmeans")
library("multcompView")

rm(list=ls()) #clears workspace

# Set Working Directory:
setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Color Data/Colormetric Data_2019") #set working

#COLORMETRIC COMPARISONS
Data <- read.csv("Colormetric_Data_2019.csv", header=T, sep=",", na.string="NA") #read in data file; for all treatments

Data <- read.csv("Colormetric_Data_2019_w.out Shade.csv", header=T, sep=",", na.string="NA") #read in data file ; data without Shade treatments

Data$Red.Norm.Coral <- Data$Red.Coral/Data$Red.Standard #normalize to color standard
Data$Green.Norm.Coral <- Data$Green.Coral/Data$Green.Standard #normalize to color standard
Data$Blue.Norm.Coral <- Data$Blue.Coral/Data$Blue.Standard #normalize to color standard

t0=subset(Data, Timepoint == "0")

boxplot(Red.Norm.Coral~Colormorph, data = t0, main = "Color Morph Red Channel Comparisons at T=0", xlab = "Color Morph", ylab = "Normalized Red values", col = c("orange", "brown"))
boxplot(Green.Norm.Coral~Colormorph, data = t0, main = "Color Morph Green Channel Comparisons", xlab = "Color Morph", ylab = "Normalized Red values", col = c("orange", "brown"))
boxplot(Blue.Norm.Coral~Colormorph, data = t0, main = "Color Morph Blue Channel Comparisons", xlab = "Color Morph", ylab = "Normalized Red values", col = c("orange", "brown"))

t.test(Red.Norm.Coral~Colormorph, data = t0) #significantly different
t.test(Green.Norm.Coral~Colormorph, data = t0) #not significantly different
t.test(Blue.Norm.Coral~Colormorph, data = t0) #not significantly different


#ZOOX DENSITY COMPARISON
setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Zoox Data") #set working
Data1 <- read.csv("t=0_data.csv", header=T, sep=",", na.string="NA") #read in data file

Data1 <- read.csv("t=0_data_w.out Shade.csv", header=T, sep=",", na.string="NA") #read in data file


boxplot(sym_density~colormorph, data = Data1, main = "Color Morph Zoox Density Comparisons at T=0", xlab = "Color Morph", ylab = "Zoox Density (cells/cm2)", ylim=c(0, 1.5e+07), col = c("orange", "brown"))
t.test(sym_density~colormorph, data = Data1) #significantly different between colormorphs

#Took out major outliers which were B1_144, B1_23, B2_170, O9_95, O13_202 and O13_196 (they were way above the rest and still have n=46)
#For data without Shade treatements, did not remove outliers (B5_176, B6_13, B6_166) because did not help.

#PROTEIN DENSITY COMPARISON
setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Protein Assay Data/2019 Data") #set working
Data2 <- read.csv("Protein_Data.csv", header=T, sep=",", na.string="NA") #read in data file
t_0=subset(Data2, timepoint == "06/09/17" & protein_density < 15000) #Excluding 2 major outliers (B1_144 and O13_196)

setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Protein Assay Data/2019 Data") #set working
Data2 <- read.csv("Protein_Data_w.out Shade.csv", header=T, sep=",", na.string="NA") #read in data file ; #Without Shade Treatment Data
t_0=subset(Data2, timepoint == "06/09/17" & protein_density < 11000) #Excluding 5 major outliers (B1_144 and O13_196)

boxplot(protein_density~colormorph, data = t_0, main = "Color Morph Protein Density Comparisons at T=0", xlab = "Color Morph", ylab = "Protein Density at T=0 (ug/cm2)", col = c("orange", "brown"))
t.test(protein_density~colormorph, data = t_0) #not significantly different 

#ALL PLOTS TOGETHER
par(mfrow=c(1,3))
boxplot(Red.Norm.Coral~Colormorph, data = t0, main = "Color Morph Red Channel Comparisons at T=0", xlab = "Color Morph", ylab = "Normalized Red Values", col = c("orange", "brown"))
boxplot(sym_density~colormorph, data = Data1, main = "Color Morph Zoox Density Comparisons at T=0", xlab = "Color Morph", ylab = "Zoox Density (cells/cm2)", ylim=c(0, 1.5e+07), col = c("orange", "brown"))
boxplot(protein_density~colormorph, data = t_0, main = "Color Morph Protein Density Comparisons at T=0", xlab = "Color Morph", ylab = "Protein Density (ug/cm2)", col = c("orange", "brown"))
dev.off()
