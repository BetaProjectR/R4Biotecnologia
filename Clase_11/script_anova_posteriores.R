# ----------------------------------------------------------
# Clase 11 y 12 - Script Anova, posteriores y supuestos
# Dr. José Gallardo
# 217 mayo 2022
# DBT 845 - Investigación reproducible y análisis de datos biotecnológicos con R.
# ----------------------------------------------------------

# LIBRERÍAS
library(ggplot2)
library(dplyr)
library(knitr)
library(broom)
library(lmtest)
library(car)
library(gridExtra)

# ESTUDIO DE CASO: CRECIMIENTO DE PLANTAS

rm(list = ls())

help(PlantGrowth)

my_data <- PlantGrowth
table(my_data$group)
colnames(my_data) <- c("Dried weight","Group")

p <-  ggplot(my_data, aes(x=`Dried weight`)) +
  geom_histogram(color="darkblue", fill="lightblue", bins = 10)

q <- my_data%>% 
  ggplot(aes(x=Group,y=`Dried weight`,fill=Group))+
  geom_boxplot()+
  theme(legend.position="none")+
  labs(x="Group",y="Dried weight")


grid.arrange(p, q, ncol=2, nrow =1)

# ajusta modelo lineal
res.aov <- lm(`Dried weight` ~ Group, data = my_data)

# Realiza inferencia con anova
anova(res.aov)
kable(anova(res.aov))

# Realiza prueba de comparaciones múltiples
fit_anova <- aov(res.aov)
tk <- TukeyHSD(fit_anova)
tk

tidy(tk) %>% kable(caption = "Prueba de Tukey.", digits=2,
                   col.names=c("Trat.","Contraste", "H0",
                               "Diferencia", "IC-bajo","IC-alto",
                               "p-ajustado"))

# Imprime anova como modelo lineal
summary(res.aov)

# Imprime anova como modelo lineal sin intercepto 
res.aov1 <- lm(`Dried weight` ~ -1 + Group, data = my_data)
summary(res.aov1)

# EVALUACIÓN DE SUPUESTOS
# Independencia
plot(res.aov$residuals, pch=20, col = "blue")

help(dwtest)
dwtest(`Dried weight` ~ Group, data = my_data,
       alternative = c("two.sided"), 
       iterations = 15)

# Homogeneidad de varianzas
plot(res.aov, 1, pch=20, col = "blue")

help(leveneTest)
leveneTest(`Dried weight` ~ Group, data = my_data,
           center = "median")

# Normalidad
aov_residuals <- residuals(object = res.aov)
hist(x= aov_residuals, main = "Histograma de residuales")
plot(res.aov, 2, pch=20, col = "blue")

qqPlot(res.aov) # library(car)

shapiro.test(x= aov_residuals)


# ESTUDIO DE CASO: GUINEA PIGS

help(ToothGrowth)
my_data1 <- ToothGrowth
summary(my_data1)

my_data1$dose <- as.factor(my_data1$dose)
summary(my_data1)
table(my_data1$supp, my_data1$dose)

p <-  ggplot(my_data1, aes(x=len)) +
  geom_histogram(color="darkblue", fill="lightblue", bins = 10)

q<- my_data1%>% 
  ggplot(aes(x=dose,y=len,fill=supp))+
  geom_boxplot()+
  labs(x="Dose vitamin c",y="Length of odontoblasts")

grid.arrange(p, q, ncol=2, nrow =1)

# Anova de dos vías con interacción
res.aov2 <- aov(len ~ dose * supp,
                data = my_data1)

# Imprime resultado anova
anova(res.aov2)

# Imprime resultado en formato tabla.
anova(res.aov2)%>% kable(caption = "Anova de dos vías.",
                         digits=3)

# EVALUACIÓN DE SUPUESTOS
# Independencia
plot(res.aov2$residuals, pch=20, col = "blue")

dwtest(len ~ dose * supp, data = my_data1,
       alternative = c("two.sided"), 
       iterations = 15)

# Homogeneidad de varianzas
plot(res.aov2, 1, pch=20, col = "blue")

help(leveneTest)
leveneTest(len ~ dose * supp, data = my_data1,
           center = "median")

# Normalidad
aov_residuals <- residuals(object = res.aov2)
hist(x= aov_residuals, main = "Histograma de residuales")
plot(res.aov2, 2, pch=20, col = "blue")

qqPlot(res.aov2) # library(car)

shapiro.test(x= aov_residuals)

