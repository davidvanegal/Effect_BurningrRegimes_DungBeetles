library(vcd) # Visualizing Categorical Data CRAN v1.4-13
library(tidyr) # Tidy Messy Data CRAN v1.3.1
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics CRAN v3.5.1
library(dplyr) # A Grammar of Data Manipulation CRAN v1.1.4
library(gg.layers) # ggplot layers [github::rpkgs/gg.layers] v0.1.3
library(ggpubr) # 'ggplot2' Based Publication Ready Plots CRAN v0.6.0

plotGF <- read.csv("CWM/Data.csv", sep = ";") 

# Convert factor -----
plotGF$BurningRegime <- factor(plotGF$BurningRegime, levels = c("Null", 
                                                        "Low",
                                                        "High"))

# Boxplots
## Tunnellers
Tun <- plotGF |> 
  pivot_longer(cols = c(SmallTunn, MediumTunn, LargeTunn), 
               names_to = "Types", 
               values_to = "Total") |>
  mutate(Types = factor(Types, 
                        levels = c("SmallTunn", "MediumTunn", "LargeTunn"))) |> 
  ggplot(aes(x = BurningRegime, y = Total, fill = Types)) + 
  geom_boxplot2(width = 0.6, width.errorbar = 0.1, alpha = 0.6)+
  geom_point(alpha = 0.3, 
             position=position_jitter(width = .2), color = "cyan4")+
  scale_fill_manual(labels = c("Small ", "Medium", "Large"),
                               values = c("lightgoldenrod1", "orange","orangered"))+
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
  annotate("text", label = "A", size = 4, x = 0.8, y = 24, family = "serif")+
  annotate("text", label = "a", size = 4, x = 1, y = 23, family = "serif")+
  annotate("text", label = "A", size = 4, x = 1.2, y = 9, family = "serif", 
           fontface = "italic")+
  annotate("text", label = "B", size = 4, x = 1.8, y = 15, family = "serif")+
  annotate("text", label = "a", size = 4, x = 2, y = 10, family = "serif")+
  annotate("text", label = "A", size = 4, x = 2.2, y = 7, family = "serif",
           fontface = "italic")+
  annotate("text", label = "A", size = 4, x = 2.8, y = 63, family = "serif")+
  annotate("text", label = "a", size = 4, x = 3, y = 17, family = "serif")+
  annotate("text", label = "A", size = 4, x = 3.2, y = 9, family = "serif", 
           fontface = "italic")+
  annotate("text", label = "A", size = 6, x = 3.5, y = 60, family = "serif")+
  ylab("Abundance")+
  labs(fill = "Tunnelers size")

## Rollers
Ro <- plotGF |> 
  pivot_longer(cols = c(SmallRoll, MediumRoll, LargeRoll), 
               names_to = "Types", 
               values_to = "Total") |>
  mutate(Types = factor(Types, 
                        levels = c("SmallRoll", "MediumRoll", "LargeRoll"))) |> 
  ggplot(aes(x = BurningRegime, y = Total, fill = Types)) + 
  geom_boxplot2(width = 0.6, width.errorbar = 0.1, alpha = 0.6)+
  scale_fill_manual(labels = c("Small ", "Medium", "Large"),
                    values = c("lightgoldenrod1", "orange","orangered"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "top", 
        legend.text = element_text(family = "sans", size = 12), 
        legend.title = element_text(family = "sans", size = 12))+
  annotate("text", label = "A", size = 4, x = 0.8, y = 54, family = "serif")+
  annotate("text", label = "a", size = 4, x = 1, y = 7, family = "serif")+
  annotate("text", label = "A", size = 4, x = 1.2, y = 16, family = "serif", 
           fontface = "italic")+
  annotate("text", label = "A", size = 4, x = 1.8, y = 31, family = "serif")+
  annotate("text", label = "b", size = 4, x = 2, y = 15, family = "serif")+
  annotate("text", label = "A", size = 4, x = 2.2, y = 15, family = "serif",
           fontface = "italic")+
  annotate("text", label = "A", size = 4, x = 2.8, y = 26, family = "serif")+
  annotate("text", label = "b", size = 4, x = 3, y = 64, family = "serif")+
  annotate("text", label = "A", size = 4, x = 3.2, y = 8, family = "serif", 
           fontface = "italic")+
  annotate("text", label = "B", size = 6, x = 3.5, y = 60, family = "serif")+
  labs(fill = "Rollers size")

## Feeding strategy
FS <- plotGF |> 
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
  annotate("text", label = "AB", size = 4, x = 0.8, y = 48, family = "serif")+
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
  ylab("Abundance")+
  xlab("Burning regime")+
  labs(fill = "Feeding Strategy")

## HabitatPrefence
HP <- plotGF |> 
  pivot_longer(cols = c(HabPrefS, HabPrefG), 
               names_to = "HabitatPrefence", 
               values_to = "Total") |>
  ggplot(aes(x = BurningRegime, y = Total, fill = HabitatPrefence)) + 
  geom_boxplot2(width = 0.6, width.errorbar = 0.1, alpha = 0.6)+
  geom_point(alpha = 0.3, 
             position=position_jitter(width = .2), color = "cyan4")+
  scale_fill_manual(labels = c("Generalist", "Specialist"),
                    values = c("lightgoldenrod1", "orange"))+
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
  ylab("Abundance")+
  xlab("Burning regime")+
  labs(fill = "Habitat Preference")


## Plot Total ----
ggarrange(Tun, Ro, 
          FS, HP,
                      ncol = 2, nrow = 2)

