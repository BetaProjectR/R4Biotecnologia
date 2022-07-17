# title: "Ejemplo PERMANOVA"
# subtitle: 'Métodos de análisis no paramétricos'
# author: Dr. José A. Gallardo.
# affiliation: Profesor adjunto de la Pontificia Universidad Católica de Valparaíso
#  email: <jose.gallardo@pucv.cl

# Objetivos de aprendizaje
# Elaborar un análisis PERMANOVA usando el software R.

# Paquetes

library(readxl)
library(dplyr)
library(ggplot2)
library(vegan) # 	Community Ecology Package: Ordination, Diversity and Dissimilarities
library(factoextra)

# Data dune meadow vegetation
data(dune) # Diversidad de especies.
data(dune.env) # Variables de clasificación.

summary(dune.env)

# $H_0$= No existe diferencia entre los grupos (Management).
# $H_1$= Al menos dos grupos son diferentes (Management).
# Niveles de Management: 
# BF (Biological farming)
# HF (Hobby farming)
# NM (Nature Conservation Management)
# SF (Standard Farming)

permanova <- adonis(dune ~ Management, method = "bray", data=dune.env, permutations=999)
permanova[["aov.tab"]]

# Realice un análisis de componentes principales.
dunepca <- prcomp(dune, scale = TRUE)
dunepca

# Obtenga la varianza explicada por cada CP con la función **get_eigenvalue**
# y grafique con la función **fviz_eig**.
get_eigenvalue(dunepca)
fviz_eig(dunepca)

# Elabore gráficas para representar la distribución de los sitios **fviz_pca_ind()**
fviz_pca_ind(dunepca, 
             label = "none", # hide individual labels
             habillage = dune.env$Management, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "#2E9FDF"),
             addEllipses = FALSE, # Concentration ellipses
             repel = TRUE) # evita que se solape el nombre de los sitios

fviz_pca_ind(dunepca, 
             label = "none", # hide individual labels
             habillage = dune.env$Management, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "#2E9FDF"),
             addEllipses = TRUE, # Concentration ellipses
             repel = TRUE) # evita que se solape el nombre de los sitios

# Gráficas para representar la distribución de los sitios usando ggplot2
PCA1 <- dunepca$x[,1]
PCA2 <- dunepca$x[,2]
plot<-cbind(rownames(dune), PCA1, PCA2, dune.env$Management)
colnames(plot) <- c("dune", "PCA1", "PCA2", "Management")

plot<-as.data.frame(plot)

plot$PCA1 <- as.numeric(plot$PCA1)
plot$PCA2 <- as.numeric(plot$PCA2)

# Plot
p<-ggplot(plot, aes(PCA1, PCA2, shape=Management))+
  geom_point(aes(color=Management), size = 2)+##separates overlapping points
  stat_ellipse(type='t',size =0.5)+ ##draws 95% confidence interval ellipses
  theme_minimal()
p

# EJERCICIO 1
# a) Para el set de datos dune evalue la siguiente hipótesis.
# $H_0$= No existe diferencia entre los grupos (Use).
# $H_1$= Al menos dos grupos son diferentes (Use).
# Niveles de Use: 
# Hayfield < Haypastu < Pasture.
# b) Realice un gráfico fviz_pca_ind, incluya como argumento habillage = dune.env$Use y
# addEllipses = TRUE

# EJERCICIO 2
# Usando el set de datos bioenv realice un PERMANOVA para investigar la siguiente hipótesis
# $H_0$= No existe diferencia entre los grupos (Sediment).
# $H_1$= Al menos dos grupos son diferentes (Sediment).
# Niveles de Sediment: 
# C : Clay (arcilla)
# S : Sand (arena)
# G : Gravel (grabilla)

# b) Realice un gráfico fviz_pca_ind, incluya como argumento habillage = dune.env$Use y
# addEllipses = TRUE

# EJERCICIO 3
# Usando el set de datos BV_PCA_subset realice un PERMANOVA para investigar la siguiente hipótesis
# $H_0$= No existe diferencia entre los grupos (BV_state).
# $H_1$= Al menos dos grupos son diferentes (BV_state).
# Niveles de BV_state: positive or negative

# b) Realice un gráfico fviz_pca_ind, incluya como argumento habillage = metadata$BV_state y
# addEllipses = TRUE y ellipse.level=0.60.







































# Respuesta ejercicio 1
permanova <- adonis(dune ~ Use, method = "bray", data=dune.env, permutations=999)
permanova[["aov.tab"]]

fviz_pca_ind(dunepca, 
             label = "none", # hide individual labels
             habillage = dune.env$Use, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             repel = TRUE) # evita que se solape el nombre de los sitios


# **Ejercicio 2.** Importar y explorar
bioenv <- read_excel("bioenv.xlsx", sheet = 1)
summary(bioenv)
bioenv$Sitio <- as.factor(bioenv$Sitio)
bioenv$Sediment <- as.factor(bioenv$Sediment)
str(bioenv)

bioenv_new <- as.data.frame(bioenv[,2:6])
row.names(bioenv_new) <- bioenv$Sitio
head(bioenv_new)

# PERMANOVA
permanova_2 <- adonis(bioenv_new ~ Sediment, method = "bray", data=bioenv, permutations=1000)
permanova_2[["aov.tab"]]

bioenvpca <- prcomp(bioenv_new, scale = TRUE)
bioenvpca

# Gráfica
fviz_pca_ind(bioenvpca, 
             label = "none", # hide individual labels
             habillage = bioenv$Sediment, # color by groups
             palette = c("#00AFBB", "#E7B800", "#2E9FDF"),
             addEllipses = TRUE, # Concentration ellipses
             repel = TRUE) # evita que se solape el nombre de los sitios


# **Ejercicio 3.** Importar y explorar
metadata <- read_excel("BV_PCA_subset.xlsx", sheet = 1)
microbiota <- read_excel("BV_PCA_subset.xlsx", sheet = 2)

# Ttransforma datos a formato adecuado para PCA
microbiota_new <- microbiota[2:104]
rownames(microbiota_new) <- microbiota$sample

#PCA
microbiota_PCA <- prcomp(microbiota_new, scale = TRUE)

# PERMANOVA
permanova_3 <- adonis(microbiota_new ~ BV_state, method = "bray", data=metadata, permutations=1000)
permanova_3[["aov.tab"]]

# Gráfica PCA
fviz_pca_ind(microbiota_PCA, 
             label = "none", # hide individual labels
             habillage = metadata$BV_state, # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.level=0.70,
             repel = TRUE) # evita que se solape el nombre de los sitios

