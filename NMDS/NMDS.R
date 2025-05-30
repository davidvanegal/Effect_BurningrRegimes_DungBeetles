library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics CRAN v3.5.0
library(GGally) # Extension to 'ggplot2' CRAN v2.2.1
library(CCA) # Canonical Correlation Analysis CRAN v1.2.2
library(MASS) # Support Functions and Datasets for Venables and Ripley's MASS CRAN v7.3-60
library(vegan) # Community Ecology Package CRAN v2.6-4
library(ggforce) # Accelerating 'ggplot2' CRAN v0.4.2
library(png) # Read and write PNG images CRAN v0.1-8 
library(patchwork) # The Composer of Plots CRAN v1.2.0 
library(cowplot) # Streamlined Plot Theme and Plot Annotations for 'ggplot2' CRAN v1.1.3
library(grid)
library(ggrepel) # Automatically Position Non-Overlapping Text Labels with'ggplot2' CRAN v0.9.6
library(multcomp) # Simultaneous Inference in General Parametric Models CRAN v1.4-25
library(pairwiseAdonis) # Pairwise Multilevel Comparison using Adonis [github::pmartinezarbizu/pairwiseAdonis] v0.4.1
library(indicspecies) # Relationship Between Species and Groups of Sites CRAN v1.7.15

## Análisis bajo parámetros de abundancia

# Read data
DBData <- read.csv("NMDS/NMDSBurning.csv", sep = ";")
# Delete firts 3 columns 
DB <- DBData[, 4:21]
View(DB)

# NMDS ----
nmdsDB <- metaMDS(DB, k = 2, distance = "bray")
nmdsDB

# Cálculo de la bondad de ajuste para NMDS
bdanDB <- goodness(nmdsDB)
print(bdanDB)
sum(bdanDB)
mean(bdanDB)  

nmdsDB$points
nmds.scoresDB <-scores(nmdsDB)
nmds.scoresDB

SitioDB <- nmdsDB$points
sppDB <- nmdsDB$species
sppDB

# Determinar sitios
nmdsdataDB <- data.frame(nmds.scoresDB$sites)
nmdsdataDB$sites <- c(1:15)
nmdsdataDB$sites[1:5] <- c("High")
nmdsdataDB$sites[6:10] <- c("Low")
nmdsdataDB$sites[11:15] <- c("Null")

nmdsdataDB$site <- DBData$Sites

nmdsdataDB$sites <- factor(nmdsdataDB$sites, 
                               levels = c("Null", "Low", "High"))


# Plot
ggplot(nmdsdataDB, aes(x = NMDS1, y = NMDS2))+
  geom_label_repel(data = nmdsdataDB, aes(NMDS1, NMDS2, label = site))+
  geom_point(aes(shape = sites), size = 4) +
  geom_mark_ellipse(expand = 0, aes(color = sites, fill = sites))+
  scale_color_manual(values= c("lightgoldenrod2", "orange","orangered"))+
  scale_fill_manual(values = c("lightgoldenrod2", "orange","orangered"))+
  annotate("text", x = 0.7, y = 0.8, label = "Strees: 0.16", size = 5)+
  theme(axis.title.x = element_text(family = "serif", size = 16, face = "bold"))+
  theme(axis.text.x = element_text(family = "serif", size = 14, face = "bold"))+
  theme(axis.title.y = element_text(family = "serif", size = 16, face = "bold"))+
  theme(axis.text.y = element_text(family = "serif", size = 14, face = "bold"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank())+
  labs(x = "NMDS1", y = "NMDS2",
       family = "Arial", size = 14)+
  theme(panel.background = element_blank(), 
        legend.text = element_text(family = "sans", size = 13), 
        legend.title = element_text(family = "sans", size = 13), 
        legend.position = "top")+
  labs(color = "Burning regime", shape = "Burning regime", 
       fill = "Burning regime")


# PERMANOVA ---------

# Transform to matrix
DB.matrix <- as.matrix(DB) # response variables
# sqrt transformation
DB.mat <- sqrt(DB.matrix)
DB.dist <- vegdist(DB.mat, method='bray')
DB.dist

DB.div <- adonis2(DB.dist ~ as.factor(BurningRegime), data = DBData, 
                    permutations = 9999)
DB.div

# Pairwise analysis
pair_perm <- pairwise.adonis2(DB.dist ~ BurningRegime, data = DBData, 
                              permutations = 9999)
pair_perm

# Betadisper ----------
betadisper_result <- betadisper(DB.dist, as.factor(DBData$BurningRegime))
permutest(betadisper_result)

plot(betadisper_result) ##sd ellipse

# Pairwise analysis
tukey_result <- TukeyHSD(betadisper_result)
tukey_result

# Boxplot
boxplot(betadisper_result$distances ~ DBData$BurningRegime)


# Influencia de las especies en los régimenes de fuego ----
BurningRegime <- DBData$BurningRegime

spdif <- multipatt(DB, BurningRegime, func = "r.g",
                               control = how(nperm = 9999))
spdif

summary(spdif)

# Show all species
summary(spdif, alpha = 1)
