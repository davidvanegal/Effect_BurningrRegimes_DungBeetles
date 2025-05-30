library(summarytools)
library(GGally)
library(CCA)
library(vegan)
library(CCP)

ACCDB <- read.csv("ACC/DataACC.csv", header = T, sep = ";")

# Convert factor -----
ACCDB$FireRegime <- factor(ACCDB$FireRegime, levels = c("Null", 
                                                          "Low",
                                                          "High"))
ACCDB$Cover <- factor(ACCDB$Cover, levels = c("Secondary Forest", 
                                              "Biodiverse pasture",
                                              "Grass Monoculture"))

dfSummary(ACCDB, plain.ascii = FALSE, style = "grid", graph.magnif = 0.75, 
          valid.col = FALSE, tmp.img.dir  = "/tmp")

# Filter data
dungbeetles <- ACCDB[, 4:8]
soil <- ACCDB[, 9:18]



ggpairs(dungbeetles)

correlo <- matcor(dungbeetles, soil)  
img.matcor(correlo,  type    =   2)

ggduo(ACCDB,columnsX = 4:8,columnsY = 9:18,
      types = list(continuous = "smooth_lm"))


cc1 <- cc(dungbeetles, soil)
cc1$cor

plot(cc1$scores$xscores[,1],cc1$scores$yscores[,1], xlab = "score x", ylab = "score y")
grid(lty = 1)

plot(cc1$scores$xscores[,2],cc1$scores$yscores[,2], xlab = "score x", ylab = "score y")
grid(lty = 1)

plot(cc1$scores$xscores[,3],cc1$scores$yscores[,3], xlab = "score x", ylab = "score y")
grid(lty = 1)

plot(cc1$scores$xscores[,4],cc1$scores$yscores[,4], xlab = "score x", ylab = "score y")
grid(lty = 1)

plot(cc1$scores$xscores[,5],cc1$scores$yscores[,5], xlab = "score x", ylab = "score y")
grid(lty = 1)

cc1[3:4]

cc <- cca(dungbeetles, soil)
plot(cc)
grid(lty = 1)

plt.cc(cc(dungbeetles, soil), var.label = TRUE)

# Prueba de dimensiones canónicas
rho  <-  cc1$cor 
# Número de observaciones, número de variables en el primer conjunto y número de variables en el segundo conjunto. 
n  <-  dim(ACCDB)[1] 
p  <-  length(dungbeetles) 
q  <-  length(soil) 

## Calcule los valores p utilizando las aproximaciones F de diferentes estadísticas de prueba: 
p.asym(rho, n, p, q, tstat ="Wilks")


