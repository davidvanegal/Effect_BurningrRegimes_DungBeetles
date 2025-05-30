library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics CRAN v3.5.0
library(iNEXT) # Interpolation and Extrapolation for Species Diversity CRAN v3.0.1
library(readxl) # Read Excel Files CRAN v1.4.3
library(extrafont) # Tools for Using Fonts CRAN v0.19
library(vegan) # Community Ecology Package CRAN v2.6-4
library(readr) # Read Rectangular Text Data CRAN v2.1.5
library(cowplot) # Streamlined Plot Theme and Plot Annotations for 'ggplot2' CRAN v1.1.3
library(tidyverse) 

dungbeetles <- read.csv('Diversity/Diversity.csv', sep=';')

dungbeetles <- data.frame(dungbeetles[-1])
colSums(dungbeetles)

out <- iNEXT(dungbeetles, q = c(0), datatype="abundance", endpoint = 948)
out2 <- iNEXT(dungbeetles, q = c(1), datatype="abundance", endpoint = 948)
out3 <- iNEXT(dungbeetles, q = c(2), datatype="abundance", endpoint = 948)

## Estimadores

estimadores <- iNEXT(dungbeetles, datatype = "abundance")

##### lo que hace INEXT es dar como resultado una lista de dataframes 
##### la que nos interesa es la que se llama AsyEst

diversidadEstimador <- estimadores$AsyEst
diversidadEstimador <- as.data.frame(diversidadEstimador)
nombresDiversidad <- rownames(diversidadEstimador)

diversidadEstimador <- data.frame(names= nombresDiversidad, diversidadEstimador)

rownames(diversidadEstimador)<- NULL
### Filtramos todos los estimadores que queremos
diversidadEstimador$orden <- c("q0", "q1", "q2")
diversidadEstimador$ordenNumerico <- 1:3

diversidadEstimador <- select(diversidadEstimador, Assemblage, names, orden, ordenNumerico,
                              Estimator, LCL, UCL)


#### Recordemos que Q0, Q1 Y Q2 corresponden a sus respectivos análisis entonces

#### Vamos a graficar los estimadores con sus intervalos de confianza al 95%

ggplot(diversidadEstimador, aes(x = ordenNumerico, y= Estimator, color = Assemblage))+
  geom_point()+
  scale_colour_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  scale_fill_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  geom_line(linewidth = 0.7)+
  theme_classic()+
  scale_x_continuous(breaks = 1:3, labels = c("q0", "q1", "q2"))+
  xlab("Diversity orders")+
  ylab("Estimator")+
  geom_errorbar(aes(ymin = LCL, ymax = UCL, color = Assemblage,),
                linetype = "dotted", linewidth = 0.6)+
  labs(color = "Burning regimes")+
  theme(axis.text.x = element_text(family = "sans", size = 14, colour = "black"))+
  theme(axis.text.y = element_text(family = "sans", size = 14, colour = "black"))+
  theme(axis.title.x = element_text(family = "sans", size = 14, colour = "black"))+
  theme(axis.title.y = element_text(family = "sans", size = 14, colour = "black"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), 
        legend.text = element_text(family = "sans", size = 13), 
        legend.title = element_text(family = "sans", size = 13))


# Plot curves
plotout <- ggiNEXT(out, type = 1)+
  scale_colour_manual(values = c("orange","lightgoldenrod1", "orangered"))+
  scale_fill_manual(values = c("orange","lightgoldenrod1", "orangered"))+
  theme(axis.text.x = element_text(family = "sans", size = 14, colour = "black"))+
  theme(axis.text.y = element_text(family = "sans", size = 14, colour = "black"))+
  theme(axis.title.x = element_text(family = "sans", size = 14, colour = "black"))+
  theme(axis.title.y = element_text(family = "sans", size = 14, colour = "black"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "none")+
  theme(legend.position = "None", legend.title = element_blank(),
        text = element_text(size = 20), legend.key = element_blank(),
        axis.title.x = element_blank(), axis.text.x = element_blank())+
  annotate("text", x = 1000, y = 20, label = expression({ }^{0} * D), size = 6,
           family = "serif")+
  ylab("")

plotout2 <- ggiNEXT(out2, type = 1)+
  scale_colour_manual(values = c("orange","lightgoldenrod1", "orangered"))+
  scale_fill_manual(values = c("orange","lightgoldenrod1", "orangered"))+
  theme(axis.text.x = element_text(size = 14, colour = "black"))+
  theme(axis.text.y = element_text(size = 14, colour = "black"))+
  theme(axis.title.x = element_text(size = 14, colour = "black"))+
  theme(axis.title.y = element_text(size = 14, colour = "black"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "none")+
  theme(panel.background = element_blank(), legend.position = "right",
        axis.title.x = element_blank(), axis.text.x = element_blank())+
  theme(text = element_text(size = 20))+
  annotate("text", x = 1000, y = 7, label = expression({ }^{1} * D), size = 6,
           family = "serif")

plotout3 <- ggiNEXT(out3, type = 1)+
  scale_colour_manual(values = c("orange","lightgoldenrod1", "orangered"))+
  scale_fill_manual(values = c("orange","lightgoldenrod1", "orangered"))+
  theme(axis.text.x = element_text(size = 14, colour = "black"))+
  theme(axis.text.y = element_text(size = 14, colour = "black"))+
  theme(axis.title.x = element_text(size = 14, colour = "black"))+
  theme(axis.title.y = element_text(size = 14, colour = "black"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "none")+
  theme(legend.position = "None", legend.title = element_blank(),
        text = element_text(size = 20), legend.key = element_blank())+
  annotate("text", x = 1000, y = 4, label = expression({ }^{2} * D), size = 6,
           family = "serif")+
  ylab("")

plot_grid(plotout,
                 plotout2,
                 plotout3,
                 nrow = 3,
                 align = "v")
ggsave("Diversity/Diversity.png", width = 9, height = 8)
