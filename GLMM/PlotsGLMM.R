library(multcomp) # Simultaneous Inference in General Parametric Models CRAN v1.4-25
library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4 CRAN v1.1-35.2
library(lmtest) # Testing Linear Regression Models CRAN v0.9-40
library(lattice) # Trellis Graphics for R CRAN v0.21-9
library(tidyverse) # Easily Install and Load the 'Tidyverse' CRAN v2.0.0 
library(emmeans) # Estimated Marginal Means, aka Least-Squares Means CRAN v1.10.1
library(ggeffects) # Create Tidy Data Frames of Marginal Effects for 'ggplot' from Model Outputs CRAN v1.6.0
library(psych) # Procedures for Psychological, Psychometric, and Personality Research CRAN v2.4.3
library(ggridges) # Ridgeline Plots in 'ggplot2' CRAN v0.5.6
library(lazyWeave) # LaTeX Wrappers for R Users CRAN v3.0.2 
library(cowplot) # Streamlined Plot Theme and Plot Annotations for 'ggplot2' CRAN v1.1.3
library(data.table) # Extension of `data.frame` CRAN v1.15.4
library(ggpubr) # 'ggplot2' Based Publication Ready Plots CRAN v0.6.0
library(MASS) # Support Functions and Datasets for Venables and Ripley's MASS CRAN v7.3-60
library(car) # Companion to Applied Regression CRAN v3.1-2
library(stats) 
library(nlme) # Linear and Nonlinear Mixed Effects Models CRAN v3.1-166
library(vcd) # Visualizing Categorical Data CRAN v1.4-13
library(tidyr) # Tidy Messy Data CRAN v1.3.1
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics CRAN v3.5.1
library(dplyr) # A Grammar of Data Manipulation CRAN v1.1.4
library(gg.layers) # ggplot layers [github::rpkgs/gg.layers] v0.1.3

GLMMDB <- read.csv("GLMM/Data.csv", sep = ";")

## Convert factor
GLMMDB$BurningRegime <- factor(GLMMDB$BurningRegime, 
                           levels = c("Null", "Low", "High"))

#Plot1 -----

## Abundance ----
plotDB <- ggplot(GLMMDB, aes(x = BurningRegime, y = Abundance, 
                            fill = BurningRegime))+
  geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.6)+
  geom_point(alpha = 0.3, 
             position=position_jitter(width = .2), color = "cyan4")+
  scale_colour_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  scale_fill_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  theme(axis.text.y = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.title.y = element_text(size = 16, family = "serif", face="bold"))+
  theme(axis.text.x = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.title.x = element_text(size = 16, family = "serif", face="bold"))+
  scale_y_continuous("Number of individuals", breaks = seq(0, 160, by = 30))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "none")+
  theme(legend.title = element_blank(), legend.key = element_blank())+
  annotate("text", label = "A", size = 4, x = 1, y = 110, family = "serif")+
  annotate("text", label = "A", size = 4, x = 2, y = 60, family = "serif")+
  annotate("text", label = "A", size = 4, x = 3, y = 140, family = "serif")+
  annotate("text", label = "A", size = 6, x = 3.5, y = 168, family = "serif")+
  xlab("Burning regime")+
  theme(panel.grid.major = element_line(color = "gray95", 
                                        linetype = "dashed"))

## q orders ----
qplot <- GLMMDB |> 
  pivot_longer(cols = c(`q0`, `q1`, `q2`), 
               names_to = "q", 
               values_to = "Total") |> 
  data.frame() |> 
  ggplot(aes(x = BurningRegime, y = Total, fill = q))+
  geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.6)+
  geom_point(alpha = 0.3, 
             position=position_jitter(width = .2), color = "cyan4")+
  scale_colour_manual(labels = c("q0", "q1", "q2"), 
                      values = c("lightgoldenrod1", "orange","orangered"))+
  scale_fill_manual(labels = c("q0", "q1", "q2"), 
                    values = c("lightgoldenrod1", "orange","orangered"))+
  theme(axis.text.y = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.title.y = element_text(size = 16, family = "serif", face="bold"))+
  theme(axis.text.x = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.title.x = element_text(size = 16, family = "serif", face="bold"))+
  scale_y_continuous(breaks = seq(0, 10, by = 2), name = expression(bold({}^{q} * D)))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "top")+
  theme(legend.title = element_blank(), legend.key = element_blank())+
  annotate("text", label = "A", size = 4, x = 0.8, y = 10.5, family = "serif")+
  annotate("text", label = "a", size = 4, x = 1, y = 6.5, family = "serif")+
  annotate("text", label = "A", size = 4, x = 1.2, y = 3.5, family = "serif", 
           fontface = "italic")+
  annotate("text", label = "A", size = 4, x = 1.8, y = 9.5, family = "serif")+
  annotate("text", label = "a", size = 4, x = 2, y = 6.5, family = "serif")+
  annotate("text", label = "A", size = 4, x = 2.2, y = 5.6, family = "serif",
           fontface = "italic")+
  annotate("text", label = "A", size = 4, x = 2.8, y = 9.5, family = "serif")+
  annotate("text", label = "a", size = 4, x = 3, y = 6.5, family = "serif")+
  annotate("text", label = "A", size = 4, x = 3.2, y = 5.5, family = "serif", 
           fontface = "italic")+
  theme(panel.background = element_blank(), 
        legend.text = element_text(family = "sans", size = 14), 
        legend.title = element_blank())+
  annotate("text", label = "B", size = 6, x = 3.5, y = 10.5, family = "serif")+
  xlab("Burning regime")+
  theme(panel.grid.major = element_line(color = "gray95", 
                                        linetype = "dashed"))

## Join plots ----
ggarrange(plotDB, qplot, 
          ncol = 2, nrow = 1, align = "v")

## Save Plot ----
ggsave("GLMM/plot1.png", width = 11, height = 6)

#Plot2 -----

## Removal DB
plotReT <- ggplot(GLMMDB, aes(x = BurningRegime, y = RemovalCommunity,
                             fill = BurningRegime))+
  geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.6)+
  geom_point(alpha = 0.3, 
             position=position_jitter(width = .2), color = "cyan4")+
  scale_y_continuous(breaks = seq(0, 350, by = 50))+
  scale_colour_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  scale_fill_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  theme(axis.title.x = element_text(size = 16, family = "serif", face="bold"))+
  theme(axis.text.y = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.title.y = element_text(size = 16, family = "serif", face="bold"))+
  theme(axis.text.x = element_text(size = 14, family = "serif", face="bold"))+
  ylab(expression(bold(atop("Dung removal by", 
                      paste("dung beetles community (g)")))))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "none")+
  theme(legend.title = element_blank(), legend.key = element_blank())+
  annotate("text", label = "A", size = 4, x = 1, y = 130, family = "serif")+
  annotate("text", label = "A", size = 4, x = 2, y = 210, family = "serif")+
  annotate("text", label = "A", size = 4, x = 3, y = 350, family = "serif")+
  annotate("text", label = "A", size = 6, x = 3.5, y = 350, family = "serif")+
  xlab("Burning regime")+
  theme(panel.grid.major = element_line(color = "gray95", 
                                        linetype = "dashed"))

# Removal D. gazella
plotRe <- ggplot(GLMMDB, aes(x = BurningRegime, y = RemovalSpecies, 
                             fill = BurningRegime))+
  geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.6)+
  geom_point(alpha = 0.3, 
             position=position_jitter(width = .2), color = "cyan4")+
  scale_y_continuous(breaks = seq(0, 100, by = 20))+
  scale_colour_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  scale_fill_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  theme(axis.title.x = element_text(size = 16, family = "serif", face="bold"))+
  theme(axis.text.y = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.title.y = element_text(size = 16, family = "serif", face="bold"))+
  theme(axis.text.x = element_text(size = 14, family = "serif", face="bold"))+
  ylab(expression(bold(atop("Dung removal by", 
                       paste(bolditalic("D. gazella"), " (g)")))))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "none")+
  theme(legend.title = element_blank(), legend.key = element_blank())+
  annotate("text", label = "A", size = 4, x = 1, y = 5, family = "serif")+
  annotate("text", label = "A", size = 4, x = 2, y = 100, family = "serif")+
  annotate("text", label = "A", size = 4, x = 3, y = 95, family = "serif")+
  annotate("text", label = "B", size = 6, x = 3.5, y = 114, family = "serif")+
  xlab("Burning regime")+
  theme(panel.grid.major = element_line(color = "gray95", 
                                        linetype = "dashed"))

## Plot Total ----
ggarrange(plotReT, plotRe, 
                     ncol = 2, nrow = 1)

## Save plot ----
ggsave("GLMM/plot2.png", width = 11, height = 6)

# Boxplots ----

## Food relocation ----
FoodR <- GLMMDB |> 
  pivot_longer(cols = c(Tunnelers, Rollers), 
               names_to = "FoodRelocation", 
               values_to = "Total") |>
  ggplot(aes(x = BurningRegime, y = Total, fill = FoodRelocation)) + 
  geom_boxplot2(width = 0.6, width.errorbar = 0.1, alpha = 0.6)+
  geom_point(alpha = 0.3, 
             position=position_jitter(width = .2), color = "cyan4")+
  scale_fill_manual(labels = c("Rollers", "Tunnelers"),
                    values = c("lightgoldenrod1","orangered"))+
  scale_y_continuous(breaks = seq(0, 140, by = 20))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.title.y = element_text(size = 16, family = "serif", face="bold"))+
  theme(axis.text.x = element_blank())+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "top", 
        legend.text = element_text(family = "sans", size = 12), 
        legend.title = element_text(family = "sans", size = 12))+
  annotate("text", label = "A", size = 4, x = 0.85, y = 60, family = "serif")+
  annotate("text", label = "a", size = 4, x = 1.15, y = 49, family = "serif")+
  annotate("text", label = "A", size = 4, x = 1.85, y = 95, family = "serif")+
  annotate("text", label = "b", size = 4, x = 2.15, y = 26, family = "serif")+
  annotate("text", label = "A", size = 4, x = 2.85, y = 80, family = "serif")+
  annotate("text", label = "a", size = 4, x = 3.15, y = 77, family = "serif")+
  annotate("text", label = "A", size = 6, x = 3.5, y = 140, family = "serif")+
  ylab("Number of individuals")+
  labs(fill = "Food relocation")+
  theme(panel.grid.major = element_line(color = "gray95", 
                                        linetype = "dashed"))

## Size ----
Size <- GLMMDB |> 
  pivot_longer(cols = c(Small, Medium, Large), 
               names_to = "Size", 
               values_to = "Total") |>
  ggplot(aes(x = BurningRegime, y = Total, fill = Size)) + 
  geom_boxplot2(width = 0.6, width.errorbar = 0.1, alpha = 0.6)+
  geom_point(alpha = 0.3, 
             position=position_jitter(width = .2), color = "cyan4")+
  scale_fill_manual(labels = c("Large", "Medium", "Small"),
                    values = c("lightgoldenrod1", "orange","orangered"))+
  scale_y_continuous(breaks = seq(0, 120, by = 20))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "top", 
        legend.text = element_text(family = "sans", size = 12), 
        legend.title = element_text(family = "sans", size = 12))+
  annotate("text", label = "A", size = 4, x = 0.8, y = 19, family = "serif")+
  annotate("text", label = "a", size = 4, x = 1, y = 55, family = "serif")+
  annotate("text", label = "A", size = 4, x = 1.2, y = 57, family = "serif", 
           fontface = "italic")+
  annotate("text", label = "A", size = 4, x = 1.8, y = 16, family = "serif")+
  annotate("text", label = "a", size = 4, x = 2, y = 48, family = "serif")+
  annotate("text", label = "A", size = 4, x = 2.2, y = 19, family = "serif",
           fontface = "italic")+
  annotate("text", label = "A", size = 4, x = 2.8, y = 15, family = "serif")+
  annotate("text", label = "a", size = 4, x = 3, y = 58, family = "serif")+
  annotate("text", label = "A", size = 4, x = 3.2, y = 64, family = "serif", 
           fontface = "italic")+
  annotate("text", label = "B", size = 6, x = 3.5, y = 120, family = "serif")+
  labs(fill = "Size")+
  ylab("Number of individuals")+
  theme(panel.grid.major = element_line(color = "gray95", 
                                        linetype = "dashed"))

## Feeding strategy ----
FS <- GLMMDB |> 
  pivot_longer(cols = c(FeedStratC, FeedStratN, FeedStratG), 
               names_to = "FeedingStrategy", 
               values_to = "Total") |>
  mutate(FeedingStrategy = factor(FeedingStrategy, 
                                  levels = c("FeedStratC", "FeedStratN", 
                                             "FeedStratG"))) |> 
  ggplot(aes(x = BurningRegime, y = Total, fill = FeedingStrategy)) + 
  geom_boxplot2(width = 0.6, width.errorbar = 0.1, alpha = 0.6)+
  geom_point(alpha = 0.3, 
             position=position_jitter(width = .2), color = "cyan4")+
  scale_fill_manual(labels = c("Coprophagous", "Necrophagous", "Generalist"),
                    values = c("lightgoldenrod1", "orange","orangered"))+
  scale_y_continuous(breaks = seq(0, 120, by = 20))+
  theme(axis.title.x = element_text(size = 16, family = "serif", face="bold"))+
  theme(axis.text.y = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.title.y = element_text(size = 16, family = "serif", face="bold"))+
  theme(axis.text.x = element_text(size = 14, family = "serif", face="bold"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "top", 
        legend.text = element_text(family = "sans", size = 12), 
        legend.title = element_text(family = "sans", size = 12))+
  annotate("text", label = "AB", size = 4, x = 0.8, y = 50, family = "serif")+
  annotate("text", label = "a", size = 4, x = 1, y = 52, family = "serif")+
  annotate("text", label = "A", size = 4, x = 1.2, y = 40, family = "serif", 
           fontface = "italic")+
  annotate("text", label = "A", size = 4, x = 1.8, y = 31, family = "serif")+
  annotate("text", label = "ab", size = 4, x = 2, y = 28, family = "serif")+
  annotate("text", label = "A", size = 4, x = 2.2, y = 20, family = "serif",
           fontface = "italic")+
  annotate("text", label = "B", size = 4, x = 2.8, y = 115, family = "serif")+
  annotate("text", label = "b", size = 4, x = 3, y = 22, family = "serif")+
  annotate("text", label = "A", size = 4, x = 3.2, y = 15, family = "serif", 
           fontface = "italic")+
  annotate("text", label = "C", size = 6, x = 3.5, y = 110, family = "serif")+
  ylab("Number of individuals")+
  xlab("Burning regime")+
  labs(fill = "Feeding Strategy")+
  theme(panel.grid.major = element_line(color = "gray95", 
                                        linetype = "dashed"))

## HabitatPrefence ----
HP <- GLMMDB |> 
  pivot_longer(cols = c(HabPrefS, HabPrefG), 
               names_to = "HabitatPrefence", 
               values_to = "Total") |>
  ggplot(aes(x = BurningRegime, y = Total, fill = HabitatPrefence)) + 
  geom_boxplot2(width = 0.6, width.errorbar = 0.1, alpha = 0.6)+
  geom_point(alpha = 0.3, 
             position=position_jitter(width = .2), color = "cyan4")+
  scale_fill_manual(labels = c("Generalist", "Specialist"),
                    values = c("lightgoldenrod1", "orange"))+
  scale_y_continuous(breaks = seq(0, 120, by = 20))+
  theme(axis.title.x = element_text(size = 16, family = "serif", face="bold"))+
  theme(axis.text.y = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size = 14, family = "serif", face="bold"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "top", 
        legend.text = element_text(family = "sans", size = 12), 
        legend.title = element_text(family = "sans", size = 12))+
  annotate("text", label = "A", size = 4, x = 0.85, y = 125, family = "serif")+
  annotate("text", label = "a", size = 4, x = 1.15, y = 42, family = "serif")+
  annotate("text", label = "A", size = 4, x = 1.85, y = 33, family = "serif")+
  annotate("text", label = "a", size = 4, x = 2.15, y = 29, family = "serif")+
  annotate("text", label = "A", size = 4, x = 2.85, y = 21, family = "serif")+
  annotate("text", label = "a", size = 4, x = 3.15, y = 122, family = "serif")+
  annotate("text", label = "D", size = 6, x = 3.5, y = 120, family = "serif")+
  ylab("Number of individuals")+
  xlab("Burning regime")+
  labs(fill = "Habitat Preference")+
  theme(panel.grid.major = element_line(color = "gray95", 
                                        linetype = "dashed"))

## Plot Total ----
ggarrange(FoodR, Size, 
          FS, HP,
          ncol = 2, nrow = 2)

## Save plot ----
ggsave("GLMM/plot3.png", width = 13, height = 9)

