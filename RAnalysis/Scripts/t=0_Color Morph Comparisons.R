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
Data <- read.csv("Colormetric_Data_2019.csv", header=T, sep=",", na.string="NA") #read in data file
Data$Red.Norm.Coral <- Data$Red.Coral/Data$Red.Standard #normalize to color standard
Data$Green.Norm.Coral <- Data$Green.Coral/Data$Green.Standard #normalize to color standard
Data$Blue.Norm.Coral <- Data$Blue.Coral/Data$Blue.Standard #normalize to color standard

t0=subset(Data, Timepoint == "0")

boxplot(Red.Norm.Coral~Colormorph, data = t0, main = "Color Morph Red Channel Comparisons at T=0", xlab = "Color Morph", ylab = "Normalized Red values", col = c("orange", "brown"))
boxplot(Green.Norm.Coral~Colormorph, data = t0, main = "Color Morph Green Channel Comparisons", xlab = "Color Morph", ylab = "Normalized Red values", col = c("orange", "brown"))
boxplot(Blue.Norm.Coral~Colormorph, data = t0, main = "Color Morph Blue Channel Comparisons", xlab = "Color Morph", ylab = "Normalized Red values", col = c("orange", "brown"))

> t.test(Red.Norm.Coral~Colormorph, data = t0)

	Welch Two Sample t-test

data:  Red.Norm.Coral by Colormorph
t = 2.297, df = 36.885, p-value = 0.0274
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 0.006285161 0.100430086
sample estimates:
mean in group Orange    mean in group Red 
           0.5878015            0.5344439 

> t.test(Green.Norm.Coral~Colormorph, data = t0)

	Welch Two Sample t-test

data:  Green.Norm.Coral by Colormorph
t = 1.5954, df = 39.842, p-value = 0.1185
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.01072012  0.09102036
sample estimates:
mean in group Orange    mean in group Red 
           0.3834943            0.3433442 

> t.test(Blue.Norm.Coral~Colormorph, data = t0)

	Welch Two Sample t-test

data:  Blue.Norm.Coral by Colormorph
t = -1.9227, df = 47.182, p-value = 0.06056
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.167359789  0.003776948
sample estimates:
mean in group Orange    mean in group Red 
           0.2177272            0.2995187 
___________________________________________________________________________________________________________________________
#ZOOX DENSITY COMPARISON
setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Zoox Data") #set working
Data1 <- read.csv("t=0_data.csv", header=T, sep=",", na.string="NA") #read in data file


boxplot(sym_density~colormorph, data = Data1, main = "Color Morph Zoox Density Comparisons at T=0", xlab = "Color Morph", ylab = "Zoox Density (cells/cm2)", ylim=c(0, 1.5e+07), col = c("orange", "brown"))
> t.test(sym_density~colormorph, data = Data1) #significantly different between colormorphs

	Welch Two Sample t-test

data:  sym_density by colormorph
t = 2.2268, df = 43.747, p-value = 0.03116
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
  177418.8 3566236.7
sample estimates:
mean in group orange    mean in group red 
             6531727              4659900 

#Took out major outliers which were B1_144, B1_23, B2_170, O9_95, O13_202 and O13_196 (they were way above the rest and still have n=46)

_______________________________________________________________________________________________________________________________
#PROTEIN DENSITY COMPARISON
setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Protein Assay Data/2019 Data") #set working
Data2 <- read.csv("Protein_Data.csv", header=T, sep=",", na.string="NA") #read in data file
t_0=subset(Data2, timepoint == "06/09/17" & protein_density < 15000) #Excluding 2 major outliers (B1_144 and O13_196)


boxplot(protein_density~colormorph, data = t_0, main = "Color Morph Protein Density Comparisons at T=0", xlab = "Color Morph", ylab = "Protein Density at T=0 (ug/cm2)", col = c("orange", "brown"))
> t.test(protein_density~colormorph, data = t_0) #not significantly different

	Welch Two Sample t-test

data:  protein_density by colormorph
t = 0.24158, df = 43.982, p-value = 0.8102
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -1078.639  1372.442
sample estimates:
mean in group orange    mean in group red 
            3947.324             3800.423 

__________________________________________________________________________________________________________________________________
#ALL PLOTS TOGETHER
par(mfrow=c(1,3))
boxplot(Red.Norm.Coral~Colormorph, data = t0, main = "Color Morph Red Channel Comparisons at T=0", xlab = "Color Morph", ylab = "Normalized Red values", col = c("orange", "brown"))
boxplot(sym_density~colormorph, data = Data1, main = "Color Morph Zoox Density Comparisons at T=0", xlab = "Color Morph", ylab = "Zoox Density (cells/cm2)", ylim=c(0, 1.5e+07), col = c("orange", "brown"))
boxplot(protein_density~colormorph, data = t_0, main = "Color Morph Protein Density Comparisons at T=0", xlab = "Color Morph", ylab = "Protein Density (ug/cm2)", col = c("orange", "brown"))
dev.off()
