source("RangeAbundance/DetAbu.R")
source("RangeAbundance/UndAbu.R")
source("RangeAbundance/SpecDist.R")
library(iNEXT)
library(Rcpp)
library(ggplot2)
library(ggrepel)
library(stringr)
library(dplyr)
library(readxl)

## Read data ----
ra <- read.csv('RangeAbundance/RangeAbundance.csv', sep=';')

ra2 <- data.frame(ra)
names(ra2)

# Group data per sites
ra2 <- ra2%>%
  group_by(Species)%>%
  summarise(Null = sum(Null), Low = sum(Low), 
            High = sum(High))

# Convert to data frame
ra2 <- data.frame(ra2)

## PA ----
out1 <- SpecDist(ra2$Null, "abundance")
out1 <- subset(out1, probability>0)
totalNull <- dim(out1)[1]
i=1
for (i in 1:totalNull) {
  out1$number[i] <- i
  out1$sp[i] <- as.numeric(rownames(out1))[i]
}
out1$BurningRegimen  <- "Null"

## SPP ----
out2 <- SpecDist(ra2$Low, "abundance")
out2 <- subset(out2, probability>0)
totalLow <- dim(out2)[1]
i=1
for (i in 1:totalLow) {
  out2$number[i] <- i
  out2$sp[i] <- as.numeric(rownames(out2))[i]
}
out2$BurningRegimen  <- "Low"

## VS ----
out3 <- SpecDist(ra2$High, "abundance")
out3 <- subset(out3, probability>0)
totalHigh <- dim(out3)[1]
i=1
for (i in 1:totalHigh) {
  out3$number[i] <- i
  out3$sp[i] <- as.numeric(rownames(out3))[i]
}
out3$BurningRegimen  <- "High"

## Create a single base
outtotal <- rbind(out1, out2, out3)
outtotal <-subset(outtotal, method %in% c("detected"))

outtotal$BurningRegimen <- factor(outtotal$BurningRegimen, 
                               levels = c("Null", "Low", "High"))

## Plot ----
ggplot(outtotal, aes(x = number, y = probability, 
                     color = BurningRegimen, shape = BurningRegimen))+ 
  geom_point(size = 4)+ 
  geom_line(lwd = 1)+
  scale_color_manual(values = c("lightgoldenrod2", "orange","orangered"))+
  scale_x_continuous("Species rank")+
  scale_y_continuous("Proportional abundance")+
  theme(axis.title.x = element_text(family = "serif", size = 16, face="bold"))+
  theme(axis.text.x = element_text(family = "serif", size = 14, face="bold"))+
  theme(axis.title.y = element_text(family = "serif", size = 16, face="bold"))+
  theme(axis.text.y = element_text(family = "serif", size = 14, face="bold"))+
  facet_grid(cols = vars(BurningRegimen))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), text = element_text(size = 14))+
  theme(strip.background = element_blank(), strip.text.x = element_blank())+
  theme(legend.position="top")+
  labs(col = "Burning regime", shape = "Burning regime")

