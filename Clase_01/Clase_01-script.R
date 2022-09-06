# ----------------------------------------------------------
# Clase 01 - Programación con R
# Dr. José Gallardo Matus
# 03 septiembre 2022
# 'Diplomado en Análisis de Datos con R e Investigación reproducible para Biociencias.'
# ----------------------------------------------------------

#R reconoce funciones matemáticos
29+29
29*29
29==29

# Error en R
29 + diez

# Crear un objeto
diez <- 10
29 + diez

# Crea un objeto llamado cromosomas.
# Agrega en el objeto cromosomas el número 23, para representar en número los cromosomas humanos

# Version R
version
R.version.string

# ¿Como citar R?
citation()

# En que directorio estoy
getwd()

# Listar librerías o packages disponibles en mi entorno de trabajo
search()

# Listar archivos en el directorio actual
list.files()

# Crear un objeto
Nombres <- c("José Gallardo", "Paz Caballero")

# Características de un objeto
class(Nombres)
colnames(Nombres)

Sexo <- c(1,2) # Codificamos 1= varón ; 2= mujer
Estatura <- c(1.73,1.63) # Punto indica decimales.
Albinismo <- c(0,0) # # Codificamos 0= No ; 1= Si
Genotipo <- c("TT","Tt")
Profesores <- data.frame(Nombres,Sexo,Estatura,Albinismo,Genotipo)

# Use los comandos class(), dim(), str() y summary() para identificar atributos de Sexo, Profesores


# Explorar un objeto con [] y con $
Profesores[1,1]
Profesores[1,]
Profesores[2,6] # da NULL
Profesores[c(1:2),"Albinismo"]
Profesores$Nombres
Profesores$Nombres[-1] # excluye datos


# Usando [] y $ extraer y excluir datos de Genotipo desde el data.frame Profesores


# Listar objetos 
ls()

# Obtener ayuda de un comando
help("mean")
mean(Profesores$Estatura)
mean(Profesores$Nombres) # da error porque nombres no es numérico.
Profesores$Sexo <- as.factor(Profesores$Sexo)
mean(Profesores$Sexo)
class(Profesores$Sexo)
Profesores$Sexo

# Trabajando con matrices.
# Simularemos y exploraremos abundancia de 3 especies de bacterias 
# y como varian en el tiempo.
abundancia=c(1:21)
dim(abundancia)
M  = matrix(abundancia, ncol=3)
M
class(M)
dim(M) # dimensiones de una matriz
colnames(M) <- c("Coli", "Salmonella", "Streptococcus")
M
rownames(M) <- paste("day",c(seq(1:7)))
M

M[3,]
M[,c(1,2)]

mean(M)
summary(M)
M>=4 # greater than or equal to
M!=12 # not equal to

# Trabajando con listas
proyecto <- list(Profesores, M)
proyecto
# agregar nombres a una lista
proyecto <- list(Datos=Profesores, Bacterias=M)
proyecto
str(proyecto)

# Acceso a componentes de una lista
proyecto$Datos
proyecto$Bacterias
proyecto[[2]] 

# Remover objetos de la sesión de trabajo
rm(list = ls())
