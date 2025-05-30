library(factoextra)

datos <- read.csv('PCA/SoilData.csv', sep=';')
head(datos)
rownames(datos) <-  datos$ID
rownames(datos)

mod <- prcomp(~ pH + Humidity + CA + K + CIC + DA + MO + CO + N + C + PD + PT +
                Sand + Clay + Silt,
              data = datos, center = TRUE, scale = TRUE)
summary(mod)

# PCA Analysis

fviz_eig(mod, addlabels = TRUE, ylim = c(0, 100))

fviz_pca_var(mod,
             col.var = "tomato",
             repel = TRUE,     # Avoid text overlapping
)

fviz_pca_ind(mod,
             habillage = datos$Cover,
             ellipse.alpha = 0,
             addEllipses=TRUE
)

fviz_pca_biplot(mod, repel = TRUE,
                habillage = datos$FireRegime,
                #ellipse.alpha = 0,
                #addEllipses=TRUE
                ) +
  scale_color_manual(values=c("tomato", "blue4", "black"))

