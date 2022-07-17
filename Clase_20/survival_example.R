# ----------------------------------------------------------
# Survival example: Uso de librerías para análisis de sobrevivencia.
# José Gallardo
# 07de julio 2022
# 'DBT 845 - Investigación reproducible y análisis de datos biotecnológicos con R.
# ----------------------------------------------------------

# Habilita librerías
library(readxl)
library(dplyr)
library(survival)
library(survminer)
library(ggpubr)
library(ggplot2)
library(tidyverse)


# Importa, explora set de datos y transforma variables de sobrevivencia
larv <- read_excel("surv_dat.xlsx", sheet = 1)
head(larv)
larv$sample_id = as.factor(larv$sample_id)
larv$antibiotico = as.factor(larv$antibiotico)
summary(larv)

# Crea objeto tipo sobrevivencia
surv_obj <- Surv(larv$stime, larv$status) # library(survival)
class(surv_obj)
surv_obj

# Cálcula probabilidad de sobrevivencia de Kaplan-Meier y otras.
ps = survfit(formula=Surv(stime, status) ~ 
               antibiotico, data=larv, na.action= na.exclude,type="kaplan-meier")
class(ps)
summary(ps)

# Permite probar si existen o no diferencias entre dos o más curvas de sobrevivencia
test_surv <- survdiff(formula=Surv(stime, status) ~ antibiotico, data=larv)
class(test_surv)
test_surv

# Grafica de sobrevivencia usando librería survminer
# Necesita objeto tipo "survfit"
ggsurvplot(ps, pval = TRUE, ggtheme = theme_bw())
