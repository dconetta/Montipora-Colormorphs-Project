#Zoox Density Data

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
setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Zoox Data")#set working directory
Data <- read.csv("Zoox_Count_Data.csv") 

Sym.Density  <- cbind(Data, Data$timepoint, Data$treatment, Data$colormorph) #make a dataframe of PC1 and experiment factors
colnames(Sym.Density) <- c("timepoint", "colony_id", "frag.id", "treatment", "colormorph", "sym.density")
Sym.Density$Group <- paste(Sym.Density$sym.density, Sym.Density$treatment, Sym.Density$colormorph)
Sym.Density$SpGroup <- paste(Sym.Density$treatment, Sym.Density$colormorph)

All.Means <- ddply(Sym.Density, c("timepoint","treatment", "colormorph"), summarize,
                   mean= mean(sym.density, na.rm=T), #mean pnet
                   N = sum(!is.na(sym.density)), # sample size
                   se = sd(sym.density, na.rm=T)/sqrt(N)) #SE
All.Means
All.Means$se[is.na(All.Means$se)] <- 0
All.Means$Group <- paste(All.Means$timepoint, All.Means$treatment, All.Means$colormorph)
All.Means$SpGroup <- paste(All.Means$treatment, All.Means$colormorph)

newdata <- subset(All.Means, mean > 0)

cols <- c("orange", "brown")

Fig.All <- ggplot(newdata, aes(x=timepoint, y=mean, group=SpGroup)) + 
  geom_line(aes(linetype= treatment, colour=colormorph, group=SpGroup), position = position_dodge(width = 0.1), alpha=0.5) + # colour, group both depend on cond2
  geom_errorbar(aes(ymin=newdata$mean-newdata$se, ymax=newdata$mean+newdata$se), colour="black", width=0, size=0.5, position = position_dodge(width = 0.1)) +
  geom_point(aes(colour=colormorph, shape=treatment), size = 4, position = position_dodge(width = 0.1)) +
  scale_colour_manual(values=cols) +
  #annotate("text", x=43, y=1.85, label = "a", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=132, y=2.15, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(43,43), y=c(2.45,2.2), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(141,141), y=c(3.15,3.4), label = "c", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=35, y=0.5, label = ".", size = 1) + #add text to the graphic for posthoc letters
  xlab("Date") +
  ylab(expression(paste("Zoox Denisty (cells/cm2)"))) +
  #ylim(-110,30) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(),  #Set the plot background
        legend.position= "right") + #remove legend background
  ggtitle("Zoox Density Over Time") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 10, 
                                  hjust = 0))
Fig.All


