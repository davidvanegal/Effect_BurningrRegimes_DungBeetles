library(multcomp) # Simultaneous Inference in General Parametric Models CRAN v1.4-25
library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4 CRAN v1.1-35.2
library(lmtest) # Testing Linear Regression Models CRAN v0.9-40
library(lattice) # Trellis Graphics for R CRAN v0.21-9
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0 
library(emmeans) # Estimated Marginal Means, aka Least-Squares Means CRAN v1.10.1
library(psych) # Procedures for Psychological, Psychometric, and Personality Research CRAN v2.4.3

library(ggeffects)
library(data.table)
library(lazyWeave)
library(ggpubr)

# Read data
GLMDB <- read.csv("GLM_Soil/Soil.csv", header = T, sep = ";")

# Convert factor -----
GLMDB$ID <- factor(GLMDB$ID)

GLMDB$FireRegime <- factor(GLMDB$FireRegime, levels = c("Null", 
                                                          "Low",
                                                          "High"))
GLMDB$Cover <- factor(GLMDB$Cover, levels = c("Secondary Forest", 
                                                "Biodiverse pasture",
                                                "Grass Monoculture"))


dfSummary(GLMDB, plain.ascii  = FALSE, 
          style        = "grid", 
          graph.magnif = 0.75, 
          valid.col    = FALSE,
          tmp.img.dir  = "/tmp")

## Analysis
par(mfrow=c(2,4))
print(summary(GLMDB))
plot(GLMDB)

## pH - Fire Regimen
pHDB <- glm(pH ~ FireRegime + Cover, family = gaussian(), data = GLMDB)
print(summary(pHDB))
par(mfcol = c(2, 2))
plot(pHDB)
confint(pHDB)

GLMDB |> 
  ggplot(aes(x = FireRegime, y = pH, colour = Cover))+
  geom_boxplot()+
  theme_bw()

####
m1_emm <- emmeans(pHDB, specs = "FireRegime")
m1_pairs <- contrast(m1_emm,
                     method = "revpairwise",
                     adjust = "none") %>%
  summary(infer = TRUE) %>%
  data.table()

m1_pairs[ , p_pretty := pvalString(p.value)]
# also create a column with "p-val: "
m1_pairs[ , pval_pretty := paste("p = ", p_pretty)]

contrast_order <- m1_pairs[, contrast]
m1_pairs[, contrast := factor(contrast, contrast_order)]

gg_effect <- ggplot(data = m1_pairs, 
                    aes(y = contrast,
                        x = estimate)) +
  # confidence level of effect
  geom_errorbar(aes(xmin = lower.CL, 
                    xmax = upper.CL),
                width = 0, 
                color = "black") +
  # estimate of effect
  geom_point(size = 3) +
  
  # draw a line at effect = 0
  geom_vline(xintercept = 0, linetype = 2) +
  
  # p-value. The y coordinates are set by eye
  annotate(geom = "text",
           label = m1_pairs$pval_pretty,
           y = 1:3,
           x = 4.5) +
  
  # x-axis label and aesthetics
  xlab("Effect (mg)") +
  ylab("Contrast") +
  coord_cartesian(xlim = c(-8,5.5)) +
  scale_x_continuous(position="top") +
  
  theme_pubr()
  # theme(axis.title.x = element_blank())

gg_effect


