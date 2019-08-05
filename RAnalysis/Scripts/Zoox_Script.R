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
Data2 <- read.csv("Zoox_Count_Data_w.out Shade.csv") #Data for w/out Shade treatment 
Data2 <- read.csv("Zoox_Count_Data.csv") #Data for all treatments

#Only Shade and Shallow Treatment Data

Data2 <- read.csv("Zoox_Count_Data_Shade.v.Shallow.csv") #Data for only Shade & Shallow treatment 



Sym.Density  <- cbind(Data2, Data2$timepoint, Data2$Treatment, Data2$Colormorph) #make a dataframe of PC1 and experiment factors
colnames(Sym.Density) <- c("timepoint", "colony_id", "frag.id", "Treatment", "Colormorph", "sym.density")
Sym.Density$Group <- paste(Sym.Density$sym.density, Sym.Density$Treatment, Sym.Density$Colormorph)
Sym.Density$SpGroup <- paste(Sym.Density$treatment, Sym.Density$Colormorph)

All.Means1 <- ddply(Sym.Density, c("timepoint","Treatment", "Colormorph"), summarize,
                   mean= mean(sym.density, na.rm=T), #mean pnet
                   N = sum(!is.na(sym.density)), # sample size
                   se = sd(sym.density, na.rm=T)/sqrt(N)) #SE
All.Means1
All.Means1$se[is.na(All.Means1$se)] <- 0
All.Means1$Group <- paste(All.Means1$timepoint, All.Means1$Treatment, All.Means1$Colormorph)
All.Means1$SpGroup <- paste(All.Means1$Treatment, All.Means1$Colormorph)

newdata1 <- subset(All.Means1, mean > 0)

cols <- c("orange", "brown")

Fig.All1 <- ggplot(newdata1, aes(x=timepoint, y=mean, group=SpGroup)) + 
  geom_line(aes(linetype= Treatment, colour=Colormorph, group=SpGroup), position = position_dodge(width = 0.1), alpha=0.5) + # colour, group both depend on cond2
  geom_errorbar(aes(ymin=newdata1$mean-newdata1$se, ymax=newdata1$mean+newdata1$se), colour="black", width=0, size=0.5, position = position_dodge(width = 0.1)) +
  geom_point(aes(colour=Colormorph, shape=Treatment), size = 4, position = position_dodge(width = 0.1)) +
  scale_colour_manual(values=cols) +
  #annotate("text", x=43, y=1.85, label = "a", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=132, y=2.15, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(43,43), y=c(2.45,2.2), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(141,141), y=c(3.15,3.4), label = "c", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=35, y=0.5, label = ".", size = 1) + #add text to the graphic for posthoc letters
  xlab("Date") +
  ylab(expression(paste(""))) +
  #ylim(-110,30) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(),  #Set the plot background
        legend.position= "right") + 
  theme(axis.text.y = element_text(angle = 90))+#remove legend background
  ggtitle("Zoox Density(cells/cm2) Over Time") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 10, 
                                  hjust = 0))
Fig.All1

#Stacked Figure for Colormetric Data, Zoox, and Protein

library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(Fig.All), ggplotGrob(Fig.All1), ggplotGrob(Fig.All2), size = "last"))

dev.off()
