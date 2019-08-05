#Bleaching Score Data w/out Shade Treatment

#Bleaching Data
rm(list=ls()) #clears workspace 

if ("vegan" %in% rownames(installed.packages()) == 'FALSE') install.packages('vegan') 
if ("ggpubr" %in% rownames(installed.packages()) == 'FALSE') install.packages('ggpubr') 
if ("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
if ("plyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('plyr') 
if ("lsmeans" %in% rownames(installed.packages()) == 'FALSE') install.packages('lsmeans') 
if ("multcompView" %in% rownames(installed.packages()) == 'FALSE') install.packages('multcompView') 


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

Data1 <- read.csv("Colormetric_Data_2019_w.out Shade.csv", header=T, sep=",", na.string="NA") #read in data file
#Sample.Info <- read.csv("Data/Master_Fragment_Sheet.csv", header=T, sep=",", na.string="NA") #read in data file 
#Sample.Info <- Sample.Info[,c(7,5,9)]
#Tank.Info <- read.csv("Data/Tank_to_Treatment.csv", header=T, sep=",", na.string="NA") #read in data file 

#Only Shade and Shallow Data#
Data1 <- read.csv("Colormetric Data_Shade.v.Shallow.csv", header=T, sep=",", na.string="NA") #read in data file



Data1$Red.Norm.Coral <- Data1$Red.Coral/Data1$Red.Standard #normalize to color standard
Data1$Green.Norm.Coral <- Data1$Green.Coral/Data1$Green.Standard #normalize to color standard
Data1$Blue.Norm.Coral <- Data1$Blue.Coral/Data1$Blue.Standard #normalize to color standard

par(mfrow=c(1,3))
plot(Data1$Red.Coral ~ Data1$Colormorph)
plot(Data1$Green.Coral ~ Data1$Colormorph)
plot(Data1$Blue.Coral ~ Data1$Colormorph)

par(mfrow=c(1,3))
plot(Data1$Red.Norm.Coral ~ Data1$Colormorph)
plot(Data1$Green.Norm.Coral ~ Data1$Colormorph)
plot(Data1$Blue.Norm.Coral ~ Data1$Colormorph)


blch.scor <- as.matrix(cbind(Data1$Red.Norm.Coral,Data1$Green.Norm.Coral,Data1$Blue.Norm.Coral)) #create matrix
rownames(blch.scor) <- Data1$Plug.ID #name columns in dataframe

dist <- vegdist(blch.scor, method="euclidean") #calculate distance matrix of color scores

PCA.color <- princomp(dist) #run principal components Analysis
summary(PCA.color) # view variance explained by PCs

Blch <- as.data.frame(PCA.color$scores[,1]) #extract PC1
Blch$Plug.ID <- rownames(blch.scor)
#Blch <- merge(Blch, Data, by="Colormorph") #not sure if this is where
Blch  <- cbind(Blch, Data1$Date, Data1$Treatment, Data1$Colormorph) #make a dataframe of PC1 and experiment factors
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
par(mar=c(10,4,2,2)) #bottom, left, top and right margins respectively
boxplot(Bleaching.Score ~ Group, data = x, lwd = 1, ylab = 'PC1Color', las=2, cex=0.8) #plot boxplot of PC1 color score by Genotype and timepoint

##### Repeat investigation until min >-25
#min <- which(x$Bleaching.Score==min(x$Bleaching.Score))
#x[min,]
#x<- x[-min,]
#####

pdf("C:/Users/dcone/Downloads/Photographic_Bleaching.pdf")
par(mar=c(10,4,2,2)) #bottom, left, top and right margins respectively
boxplot(Bleaching.Score ~ Group, data = Blch, lwd = 1, ylab = 'PC1Color', las=2) #plot boxplot of PC1 color score by Genotype and timepoint
stripchart(Bleaching.Score ~ Group, vertical = TRUE, data = Blch, 
           method = "jitter", add = TRUE, pch = 20, col = 'blue', cex=0.2) #include all datapoints in blue overlaid on boxplots
#text(x= 0.5, y= min(Blch$Bleaching.Score), labels= "pale") #add text to indicate dark and pale on graphic
#text(x= 0.5, y= max(Blch$Bleaching.Score)+2, labels= "dark") #add text to indicate dark and pale on graphic
dev.off()

mod1 <- aov(sqrt(Bleaching.Score+200) ~ Timepoint*Treatment*Colormorph, data=Blch) #run an ANOVA by Colormorph
par(mfrow=c(1,3))
hist(residuals(mod1)) #look at normality of data
boxplot(residuals(mod1)) #look at normality of data
plot(mod1$fitted.values, mod1$residuals)
summary(mod1)

marginal = lsmeans(mod1, ~ Timepoint*Treatment*Colormorph)

options(max.print=1000000)
TukeyHSD(mod1)
anova(mod1)

CLD = cld(marginal,
          alpha=0.05,
          Letters=letters,
          adjust="tukeyHSD")

CLD <- CLD[order( CLD$Colormorph, CLD$Treatment),]
CLD

All.Means <- ddply(Blch, c('Timepoint','Colormorph', 'Treatment'), summarize,
                   mean= mean(Bleaching.Score, na.rm=T), #mean pnet
                   N = sum(!is.na(Bleaching.Score)), # sample size
                   se = sd(Bleaching.Score, na.rm=T)/sqrt(N)) #SE
All.Means
All.Means$se[is.na(All.Means$se)] <- 0
All.Means$Group <- paste(All.Means$Timepoint, All.Means$Treatment, All.Means$Colormorph)
All.Means$SpGroup <- paste(All.Means$Treatment, All.Means$Colormorph)

cols <- c("orange", "brown")
#All.Means$Timepoint <- factor(All.Means$Timepoint, levels = c("Week1", "Week3", "Week5", "Week8"))
#All.Means$Timepoint <- factor(All.Means$Timepoint, levels = c("Time0", "Time2", "Time4", "Time7"))


Fig.All <- ggplot(All.Means, aes(x=Timepoint, y=mean, group=Treatment)) + 
  geom_line(aes(linetype= Treatment, colour=Colormorph, group=SpGroup), position = position_dodge(width = 0.1), alpha=0.5) + # colour, group both depend on cond2
  geom_errorbar(aes(ymin=All.Means$mean-All.Means$se, ymax=All.Means$mean+All.Means$se), colour="black", width=0, size=0.5, position = position_dodge(width = 0.1)) +
  geom_point(aes(colour=Colormorph, shape=Treatment), size = 4, position = position_dodge(width = 0.1)) +
  scale_colour_manual(values=cols) +
  #annotate("text", x=43, y=1.85, label = "a", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=132, y=2.15, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(43,43), y=c(2.45,2.2), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(141,141), y=c(3.15,3.4), label = "c", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=35, y=0.5, label = ".", size = 1) + #add text to the graphic for posthoc letters
  xlab("Date") +
  ylab(expression(paste("Color Score"))) +
  #ylim(-110,30) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(),  #Set the plot background
        legend.position= "right") + #remove legend background
  ggtitle("Photographic Color Change") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 10, 
                                  hjust = 0))
Fig.All

#Saving data as an Excel Workbook
library(xlsx)
write.xlsx(Blch, "C:/Users/dcone/Downloads/Photographic_Bleaching.xlsx")

