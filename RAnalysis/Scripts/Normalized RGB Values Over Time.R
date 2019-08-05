#Normalized Red, Green and Blue Values Over Time

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

#Red Channel Over Time

setwd("C:/Users/dcone/OneDrive/Documents/Northeastern/Hawaii/Nursery Coloration Experiment/Color Data/Colormetric Data_2019") #set working
Data1 <- read.csv("Colormetric Data_Shade.v.Shallow.csv", header=T, sep=",", na.string="NA") #read in data file

Data1$Red.Norm.Coral <- Data1$Red.Coral/Data1$Red.Standard #normalize to color standard
Data1$Green.Norm.Coral <- Data1$Green.Coral/Data1$Green.Standard #normalize to color standard
Data1$Blue.Norm.Coral <- Data1$Blue.Coral/Data1$Blue.Standard #normalize to color standard

All.Means <- ddply(Data1, c('Date','Colormorph', 'Treatment'), summarize,
                   mean= mean(Red.Norm.Coral, na.rm=T), #mean pnet
                   N = sum(!is.na(Red.Norm.Coral)), # sample size
                   se = sd(Red.Norm.Coral, na.rm=T)/sqrt(N)) #SE
All.Means
All.Means$se[is.na(All.Means$se)] <- 0
All.Means$Group <- paste(All.Means$Date, All.Means$Treatment, All.Means$Colormorph)
All.Means$SpGroup <- paste(All.Means$Treatment, All.Means$Colormorph)

cols <- c("orange", "brown")

Fig.All <- ggplot(All.Means, aes(x=Date, y=mean, group=Treatment)) + 
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
  ylab(expression(paste("Normalized Red Channel Values"))) +
  #ylim(-110,30) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(),  #Set the plot background
        legend.position= "right") + #remove legend background
  ggtitle("Normalized Red Values Over Time") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 10, 
                                  hjust = 0))
Fig.All

#Green Normalized Values Over Time #________________________________________________
All.Means1 <- ddply(Data1, c('Date','Colormorph', 'Treatment'), summarize,
                   mean= mean(Green.Norm.Coral, na.rm=T), #mean pnet
                   N = sum(!is.na(Green.Norm.Coral)), # sample size
                   se = sd(Green.Norm.Coral, na.rm=T)/sqrt(N)) #SE
All.Means1
All.Means1$se[is.na(All.Means1$se)] <- 0
All.Means1$Group <- paste(All.Means1$Date, All.Means1$Treatment, All.Means1$Colormorph)
All.Means1$SpGroup <- paste(All.Means1$Treatment, All.Means1$Colormorph)

cols <- c("orange", "brown")

Fig.All1 <- ggplot(All.Means1, aes(x=Date, y=mean, group=Treatment)) + 
  geom_line(aes(linetype= Treatment, colour=Colormorph, group=SpGroup), position = position_dodge(width = 0.1), alpha=0.5) + # colour, group both depend on cond2
  geom_errorbar(aes(ymin=All.Means1$mean-All.Means1$se, ymax=All.Means1$mean+All.Means1$se), colour="black", width=0, size=0.5, position = position_dodge(width = 0.1)) +
  geom_point(aes(colour=Colormorph, shape=Treatment), size = 4, position = position_dodge(width = 0.1)) +
  scale_colour_manual(values=cols) +
  #annotate("text", x=43, y=1.85, label = "a", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=132, y=2.15, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(43,43), y=c(2.45,2.2), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(141,141), y=c(3.15,3.4), label = "c", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=35, y=0.5, label = ".", size = 1) + #add text to the graphic for posthoc letters
  xlab("Date") +
  ylab(expression(paste("Normalized Green Values"))) +
  #ylim(-110,30) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(),  #Set the plot background
        legend.position= "right") + #remove legend background
  ggtitle("Normalized Green Values Over Time") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 10, 
                                  hjust = 0))
Fig.All1

#Blue Normalized Values #____________________________________________________
All.Means2 <- ddply(Data1, c('Date','Colormorph', 'Treatment'), summarize,
                    mean= mean(Blue.Norm.Coral, na.rm=T), #mean pnet
                    N = sum(!is.na(Blue.Norm.Coral)), # sample size
                    se = sd(Blue.Norm.Coral, na.rm=T)/sqrt(N)) #SE
All.Means2
All.Means2$se[is.na(All.Means2$se)] <- 0
All.Means2$Group <- paste(All.Means2$Date, All.Means2$Treatment, All.Means2$Colormorph)
All.Means2$SpGroup <- paste(All.Means2$Treatment, All.Means2$Colormorph)

cols <- c("orange", "brown")

Fig.All2 <- ggplot(All.Means2, aes(x=Date, y=mean, group=Treatment)) + 
  geom_line(aes(linetype= Treatment, colour=Colormorph, group=SpGroup), position = position_dodge(width = 0.1), alpha=0.5) + # colour, group both depend on cond2
  geom_errorbar(aes(ymin=All.Means2$mean-All.Means2$se, ymax=All.Means2$mean+All.Means2$se), colour="black", width=0, size=0.5, position = position_dodge(width = 0.1)) +
  geom_point(aes(colour=Colormorph, shape=Treatment), size = 4, position = position_dodge(width = 0.1)) +
  scale_colour_manual(values=cols) +
  #annotate("text", x=43, y=1.85, label = "a", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=132, y=2.15, label = "b", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(43,43), y=c(2.45,2.2), label = "ab", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=c(141,141), y=c(3.15,3.4), label = "c", size = 3) + #add text to the graphic for posthoc letters
  #annotate("text", x=35, y=0.5, label = ".", size = 1) + #add text to the graphic for posthoc letters
  xlab("Date") +
  ylab(expression(paste("Normalized Blue Values"))) +
  #ylim(-110,30) +
  theme_bw() + #Set the background color
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1), #Set the text angle
        axis.line = element_line(color = 'black'), #Set the axes color
        panel.border = element_blank(), #Set the border
        panel.grid.major = element_blank(), #Set the major gridlines
        panel.grid.minor = element_blank(), #Set the minor gridlines
        plot.background=element_blank(),  #Set the plot background
        legend.position= "right") + #remove legend background
  ggtitle("Normalized Blue Values Over Time") +
  theme(plot.title = element_text(face = 'bold', 
                                  size = 10, 
                                  hjust = 0))
Fig.All2

#To Make Stacked Plot of all Three Figures 
library(grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(Fig.All), ggplotGrob(Fig.All1), ggplotGrob(Fig.All2), size = "last"))

dev.off()