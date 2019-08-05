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

#COLORMETRIC (RGB) COMPARISONS
Data <- read.csv("Colormetric_Data_2019.csv", header=T, sep=",", na.string="NA") #read in data file; for all treatments
Data <- read.csv("Colormetric Data_Shade.v.Shallow.csv", header=T, sep=",", na.string="NA") #read in data file; for just Shade and Shallow treatments

Data <- read.csv("Colormetric_Data_2019_w.out Shade.csv", header=T, sep=",", na.string="NA") #read in data file ; data without Shade treatments

Data$Red.Norm.Coral <- Data$Red.Coral/Data$Red.Standard #normalize to color standard
Data$Green.Norm.Coral <- Data$Green.Coral/Data$Green.Standard #normalize to color standard
Data$Blue.Norm.Coral <- Data$Blue.Coral/Data$Blue.Standard #normalize to color standard

t0=subset(Data, Timepoint == "0")

par(mfrow=c(1,3))
boxplot(Red.Norm.Coral~Colormorph, data = t0, main = "Color Morph Red Channel Comparisons at T=0", xlab = "Color Morph", ylab = "Normalized Red values", col = c("orange", "brown"))
boxplot(Green.Norm.Coral~Colormorph, data = t0, main = "Color Morph Green Channel Comparisonsat T=0", xlab = "Color Morph", ylab = "Normalized Green values", col = c("orange", "brown"))
boxplot(Blue.Norm.Coral~Colormorph, data = t0, main = "Color Morph Blue Channel Comparisonsat T=0", xlab = "Color Morph", ylab = "Normalized Blue values", col = c("orange", "brown"))

t.test(Red.Norm.Coral~Colormorph, data = t0) #significantly different
t.test(Green.Norm.Coral~Colormorph, data = t0) #not significantly different
t.test(Blue.Norm.Coral~Colormorph, data = t0) #not significantly different

#COLOR SCORE COMPARISONS
setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Color Data/Colormetric Data_2019")
Data3 <- read.csv("Colormetric Data_Shade.v.Shallow.csv", header=T, sep=",", na.string="NA") #read in data file

t.0=subset(Data3, Timepoint == "0")

t.0$Red.Norm.Coral <- t.0$Red.Coral/t.0$Red.Standard #normalize to color standard
t.0$Green.Norm.Coral <- t.0$Green.Coral/t.0$Green.Standard #normalize to color standard
t.0$Blue.Norm.Coral <- t.0$Blue.Coral/t.0$Blue.Standard #normalize to color standard

par(mfrow=c(1,3))
plot(t.0$Red.Coral ~ t.0$Colormorph)
plot(t.0$Green.Coral ~ t.0$Colormorph)
plot(t.0$Blue.Coral ~ t.0$Colormorph)

par(mfrow=c(1,3))
plot(t.0$Red.Norm.Coral ~ t.0$Colormorph)
plot(t.0$Green.Norm.Coral ~ t.0$Colormorph)
plot(t.0$Blue.Norm.Coral ~ t.0$Colormorph)


blch.scor <- as.matrix(cbind(t.0$Red.Norm.Coral,t.0$Green.Norm.Coral,t.0$Blue.Norm.Coral)) #create matrix
rownames(blch.scor) <- t.0$Plug.ID #name columns in dataframe

dist <- vegdist(blch.scor, method="euclidean") #calculate distance matrix of color scores

PCA.color <- princomp(dist) #run principal components Analysis
summary(PCA.color) # view variance explained by PCs

Blch <- as.data.frame(PCA.color$scores[,1]) #extract PC1
Blch$Plug.ID <- rownames(blch.scor)
#Blch <- merge(Blch, Data, by="Colormorph") #not sure if this is where
Blch  <- cbind(Blch, t0$Date, t0$Treatment, t0$Colormorph) #make a dataframe of PC1 and experiment factors
colnames(Blch) <- c("Bleaching.Score", "Plug.ID", "Timepoint", "Treatment", "Colormorph")
Blch$Group <- paste(Blch$Timepoint, Blch$Treatment, Blch$Species)
Blch$SpGroup <- paste(Blch$Treatment, Blch$Species)
#Blch$Bleaching.Score <- Blch$`PCA.color$scores[, 1]`

Blch$Bleaching.Score <- -Blch$Bleaching.Score

# write.table(Blch,"C:/Users/dcone/Downloads/Bleaching_Score.csv",sep=",", row.names=FALSE)

write.table(Blch, file = "C:/Users/dcone/Downloads/Bleaching_Score.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

x<- Blch #assign values to test data frame to look for outliers
dev.off()

boxplot(Bleaching.Score~Colormorph, data = Blch, main = "Color Score per Color Morph at T=0", xlab = "Color Morph", ylab = "Color Score", col = c("orange", "brown"))
t.test(Bleaching.Score~Colormorph, data = Blch)
#________________________________________________________

#ZOOX DENSITY COMPARISON
setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Zoox Data") #set working
Data1 <- read.csv("t=0_data.csv", header=T, sep=",", na.string="NA") #read in data file

Data1 <- read.csv("t=0_data_w.out Shade.csv", header=T, sep=",", na.string="NA") #read in data file

#Only Shade and Shallow Treatment Data
Data1 <- read.csv("Zoox_Count_Data_Shade.v.Shallow.csv") #Data for only Shade & Shallow treatment 


t0=subset(Data, Timepoint == "0")

boxplot(sym.density~Colormorph, data = Data1, main = "Color Morph Zoox Density Comparisons at T=0", xlab = "Color Morph", ylab = "Zoox Density (cells/cm2)", ylim=c(0, 1.5e+07), col = c("orange", "brown"))
t.test(sym.density~Colormorph, data = Data1) #significantly different between colormorphs

#Took out major outliers which were B1_144, B1_23, B2_170, O9_95, O13_202 and O13_196 (they were way above the rest and still have n=46)
#For data without Shade treatements, did not remove outliers (B5_176, B6_13, B6_166) because did not help.

#PROTEIN DENSITY COMPARISON
setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Protein Assay Data/2019 Data") #set working
Data2 <- read.csv("Protein_Data.csv", header=T, sep=",", na.string="NA") #read in data file

Data2 <- read.csv("Protein_Data_Shade.v.Shallow.csv", header=T, sep=",", na.string="NA") #Data with just Shade/Shallow treatments
t_0=subset(Data2, timepoint == "06/09/17" & protein_density < 10) #Excluding 2 major outliers (B1_144 and O13_196)

setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Protein Assay Data/2019 Data") #set working
Data2 <- read.csv("Protein_Data_w.out Shade.csv", header=T, sep=",", na.string="NA") #read in data file ; #Without Shade Treatment Data
t_0=subset(Data2, timepoint == "06/09/17" & protein_density < 11000) #Excluding 5 major outliers (B1_144 and O13_196)

boxplot(protein_density~Colormorph, data = t_0, main = "Color Morph Protein Density Comparisons at T=0", xlab = "Color Morph", ylab = "Protein Density at T=0 (ug/cm2)", col = c("orange", "brown"))
t.test(protein_density~Colormorph, data = t_0) #not significantly different 

#ALL PLOTS TOGETHER
par(mfrow=c(1,3))
boxplot(Bleaching.Score~Colormorph, data = Blch, main = "Color Score per Color Morph at T=0", xlab = "Color Morph", ylab = "Color Score", col = c("orange", "brown"))
boxplot(sym.density~Colormorph, data = Data1, main = "Color Morph Zoox Density Comparisons at T=0", xlab = "Color Morph", ylab = "Zoox Density (cells/cm2)", ylim=c(0, 1.5e+07), col = c("orange", "brown"))
boxplot(protein_density~Colormorph, data = t_0, main = "Color Morph Protein Density Comparisons at T=0", xlab = "Color Morph", ylab = "Protein Density (ug/cm2)", col = c("orange", "brown"))

dev.off()
