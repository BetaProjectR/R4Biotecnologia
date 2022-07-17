# title: "Ejemplo Análisis de Componentes Principales"
# subtitle: 'DBT 845 - Investigación reproducible y análisis de datos biotecnológicos con R.'
# author: Dr. José Gallardo Matus.
#  affiliation: Profesor adjunto de la Pontificia Universidad Católica de Valparaíso
#  email: <jose.gallardo@pucv.cl

# Objetivos de aprendizaje
# Elaborar un análisis de componentes principales con un set de datos simulado usando el software R.

# Paquetes

library(readxl)
library(dplyr)
library(psych) # Procedures for Psychological, Psychometric, and Personality Research
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses.
library(ggplot2)

# Realizar ACP a partir del set de datos **bioenv.xlsx** disponibles en el libro MULTIVARIATE ANALYSIS OF ECOLOGICAL DATA
# de los autores [Michael Greenacre and Raul Primicerio](https://www.fbbva.es/microsite/multivariate-statistics/).

# **Ejercicio 1.** Importar y explorar
bioenv <- read_excel("bioenv.xlsx", sheet = 1)
summary(bioenv)
bioenv$Sitio <- as.factor(bioenv$Sitio)
bioenv$Sediment <- as.factor(bioenv$Sediment)
str(bioenv)

# Elabore una gráfica de correlaciones de las variables Depth, Pollution y Temperature
# usando la función **pairs.panels()**.

pairs.panels(bioenv[7:9])
cor(bioenv[7:9], method = "pearson")

# **Ejercicio 2.** Análisis de Componentes Principales
# Elabore un nuevo set de datos llamado **bioenv_new** solo con las variables Depth, Pollution y Temperature.
# utilice la columna Sitio para agregar el nombre de las filas.

bioenv_new <- as.data.frame(bioenv[,7:9])
row.names(bioenv_new) <- bioenv$Sitio
head(bioenv_new)

# Realice un análisis de componentes principales para el nuevo set de datos **bioenv_new**
# con la función **prcomp**, use el argumento *scale = TRUE* para estandarizar las variables de forma automática. 

bioenvpca <- prcomp(bioenv_new, scale = TRUE)
bioenvpca

# Obtenga la varianza explicada por cada CP con la función **get_eigenvalue**
# y grafique con la función **fviz_eig**.
get_eigenvalue(bioenvpca)
fviz_eig(bioenvpca)


# Elabore gráficas para representar la distribución de los sitios **fviz_pca_ind()**,
# de las variables **fviz_pca_var()** y gráficas Biplot **fviz_pca_biplot()**

fviz_pca_ind(bioenvpca, repel = TRUE) # repel= TRUE evita que se solape el nombre de los sitios
fviz_pca_ind(bioenvpca, repel = TRUE,
             label = "none") # label = "none" hide individual labels

fviz_pca_ind(bioenvpca, repel = TRUE,
             label = "none",
             habillage = bioenv$Sediment) # Identifica individuos por factor de clasificación


fviz_pca_ind(bioenvpca, repel = TRUE,
             label = "none",
             habillage = bioenv$Sediment,
             addEllipses = TRUE) # añade elipses para identificar factores de clasificación

fviz_pca_ind(bioenvpca, repel = TRUE,
             label = "none",
             habillage = bioenv$Sediment,
             addEllipses = TRUE,
             ellipse.level=0.60) # ajusta tamaño de elipses (intervalo de confianza).

fviz_pca_ind(bioenvpca, repel = TRUE,
             label = "none",
             habillage = bioenv$Sediment,
             addEllipses = TRUE,
             ellipse.level=0.60,
             select.ind = list(cos2 = 15)) # selecciona individuos que mas contrbuyen al PCA


fviz_pca_var(bioenvpca)

fviz_pca_var(bioenvpca, select.var=list(name = c("Pollution","Depth")))

fviz_pca_biplot(bioenvpca,
                repel = TRUE,
                label = "var",
                habillage = bioenv$Sediment,
                addEllipses = TRUE,
                ellipse.level=0.60,
                select.ind = list(cos2 = 15),
                select.var=list(name = c("Pollution","Depth")))

                