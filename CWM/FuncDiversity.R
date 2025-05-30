library(lme4) # Linear Mixed-Effects Models using 'Eigen' and S4 CRAN v1.1-36
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics CRAN v3.5.1
library(dplyr) # A Grammar of Data Manipulation CRAN v1.1.4
library(tidyr) # Tidy Messy Data CRAN v1.3.1
library(FD) # Measuring Functional Diversity (FD) from Multiple Traits, and Other Tools for Functional Ecology CRAN v1.0-12.3 returns the frequencies of each class
library(RVAideMemoire) # Testing and Plotting Procedures for Biostatistics [::NA/NA] v0.9-83-7 # Testing and Plotting Procedures for Biostatistics CRAN v0.9-83-7
library(multcomp) # Simultaneous Inference in General Parametric Models CRAN v1.4-25
library(performance)
library(ggpubr)

# Community Weighted Mean -----

DB <- read.csv("CWM/Data.csv", sep = ";") #com
DB2 <- read.csv("CWM/Dom2.csv", sep = ";") # env
GFDB <- read.csv("CWM/GroupFunc.csv", sep = ";") # traits

DB2$BurningRegime <- factor(DB2$BurningRegime, 
                            levels = c("Null", "Low", "High"))

DB3 <- as.matrix(DB2[,-1])
row.names(GFDB) <- colnames(DB2[,-1])

GFDB <- GFDB[,-1]

# Chi-squared test
DBTest <- DB %>% 
  pivot_longer(cols = c(FeedStratC, FeedStratN, FeedStratG), 
               names_to = "Size", 
               values_to = "GTotal") |> 
  data.frame()

# Rs
contRs <- xtabs(GTotal ~ Size + BurningRegime, data = DBTest)
contRs 
# Perform chi-squared test
print(chisq.test(contRs))
# Post hoc
fisher.multcomp(contRs, p.method = "bonferroni")


# Analysis
plotGF <- dbFD(GFDB, DB3)
plotGF$BurningRegime <- DB$BurningRegime
plotGF$GrazingEnviroments <- DB$GrazingEnviroments

plotGF$BurningRegime <- factor(plotGF$BurningRegime, 
                               levels = c("Null", "Low", "High"))
plotGF$GrazingEnviroments <- factor(plotGF$GrazingEnviroments, levels = c("SF", 
                                                                          "BP",
                                                                          "GM"))

DBTest <- plotGF %>% 
  pivot_longer(cols = c(BurningRegime, Feeding_FoodC, Feeding_FoodG, Feeding_FoodN), 
               names_to = "Size", 
               values_to = "GTotal") |> 
  data.frame()

cont <- xtabs(GTotal ~ Size + BurningRegime, data = DBTest)
fisher.multcomp(cont, p.method = "bonferroni")



## Dominance functional groups 
DomDB <- read.csv("CWM/Dom2.csv", sep = ";") # env
GFDB2 <- read.csv("CWM/GroupFunc.csv", sep = ";") # env

DomDB  <- as.matrix(DomDB[,-1])
row.names(GFDB2) <- colnames(DB2[,-1])

DomGF <- functcomp(GFDB2, DomDB, CWM.type = "all")
View(DomGF)

write.csv(DomGF, "CWM/RelAbund.csv")

RelDB <- read.csv("CWM/RelAbund.csv", sep = ";")

RelDB$BurningRegime <- factor(RelDB$BurningRegime,
                                 levels = c("Null", "Low", "High"))


# Food relocation
FRPlot <- RelDB |> 
  rename(`Tunnelers` = RelocationStrategy_Tunellers,
         `Rollers` = RelocationStrategy_Rollers,) |> 
  pivot_longer(cols = c(`Tunnelers`, `Rollers`), 
               names_to = "FoodRelocation", 
               values_to = "Total") |> 
  data.frame() |> 
  ggplot(aes(x = BurningRegime, y = Total, fill = FoodRelocation)) + 
  geom_col(position = 'fill', width = .6)+
  scale_colour_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  scale_fill_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_text(size = 12, family = "serif", face="bold"))+
  theme(axis.title.y = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.text.x = element_blank())+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "top")+
  annotate("text", label = "A", size = 6, x = 3.5, y = 1, family = "serif")+
  ylab("Proportional abundance")+
  xlab("Burning regime")+
  labs(fill = "Food Relocation")

# Size
SPlot <- RelDB |> 
  rename(`Small` = Size_SmallSize,
         `Medium` = Size_MediumSize,
         `Large` = Size_LargeSize) |> 
  pivot_longer(cols = c(`Small`, `Medium`, `Large`), 
               names_to = "Size", 
               values_to = "Total") |> 
  data.frame() |> 
  ggplot(aes(x = BurningRegime, y = Total, fill = Size)) + 
  geom_col(position = 'fill', width = .6)+
  scale_colour_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  scale_fill_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  theme(axis.title.x = element_blank())+
  theme(axis.text.y = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_blank())+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "top")+
  annotate("text", label = "B", size = 6, x = 3.5, y = 1, family = "serif")+
  ylab("Proportional abundance")+
  xlab("Burning regime")+
  labs(fill = "Size")

# Feeding strategy
FSPlot <- RelDB |> 
  rename(`Coprophagous` = FeedingStrategy_FeedingStrategyC,
         `Generalist` = FeedingStrategy_FeedingStrategyG,
         `Necrophagous` = FeedingStrategy_FeedingStrategyN) |> 
  pivot_longer(cols = c(`Coprophagous`, `Necrophagous`, `Generalist`), 
               names_to = "FeedingStrategy", 
               values_to = "Total") |> 
  data.frame() |> 
  ggplot(aes(x = BurningRegime, y = Total, fill = FeedingStrategy)) + 
  geom_col(position = 'fill', width = .6)+
  scale_colour_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  scale_fill_manual(values = c("lightgoldenrod1", "orange","orangered"))+
  theme(axis.title.x = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.text.y = element_text(size = 12, family = "serif", face="bold"))+
  theme(axis.title.y = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.text.x = element_text(size = 12, family = "serif", face="bold"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "top")+
  annotate("text", label = "C", size = 6, x = 3.5, y = 1, family = "serif")+
  annotate("text", label = "*", size = 4, x = 1, y = 0.27, family = "serif")+
  annotate("text", label = "°", size = 4, x = 3, y = 0.06, family = "serif")+
  annotate("text", label = "°", size = 4, x = 1, y = 0.84, family = "serif")+
  annotate("text", label = "*", size = 4, x = 3, y = 0.62, family = "serif")+
  ylab("Proportional abundance")+
  xlab("Burning regime")+
  labs(fill = "Feeding strategy")

# Habitat preference
HPPlot <- RelDB %>% 
  rename(`Specialist` = `HabitatPrefence_HabitatPrefenceS`,
         `Generalist` = `HabitatPrefence_HabitatPrefenceG`) |> 
  pivot_longer(cols = c(`Specialist`, `Generalist`), 
               names_to = "HabitatPrefence", 
               values_to = "Total") |> 
  ggplot(aes(x = BurningRegime, y = Total, fill = HabitatPrefence)) + 
  geom_col(position = 'fill', width = .6)+
  scale_colour_manual(values = c("lightgoldenrod1", "orange"))+
  scale_fill_manual(values = c("lightgoldenrod1", "orange"))+
  theme(axis.title.x = element_text(size = 14, family = "serif", face="bold"))+
  theme(axis.text.y = element_blank())+
  theme(axis.title.y = element_blank())+
  theme(axis.text.x = element_text(size = 12, family = "serif", face="bold"))+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  theme(panel.background = element_blank(), legend.position = "top")+
  annotate("text", label = "D", size = 6, x = 3.5, y = 1, family = "serif")+
  annotate("text", label = "*", size = 4, x = 1, y = 0.65, family = "serif")+
  annotate("text", label = "°", size = 4, x = 3, y = 0.89, family = "serif")+
  annotate("text", label = "°", size = 4, x = 1, y = 0.15, family = "serif")+
  annotate("text", label = "*", size = 4, x = 3, y = 0.41, family = "serif")+
  ylab("Proportional abundance")+
  xlab("Burning regime")+
  labs(fill = "Habitat preference")

ggarrange(FRPlot,
          SPlot,
          FSPlot, 
          HPPlot, 
          ncol = 2, nrow = 2, align = "v")

ggsave("CWM/CWM.png", width = 12, height = 9)




