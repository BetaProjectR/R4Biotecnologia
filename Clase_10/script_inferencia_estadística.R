# ----------------------------------------------------------
# Clase 05 - Script inferencia estadística
# Dr. José Gallardo
# 21 enero 2022
# Curso R para Biociencias
# ----------------------------------------------------------

# LIBRERÍAS
library(UsingR)
library(ggplot2)
library(dplyr)
library(knitr)

# ESTUDIO DE CASO: RELACIÓN ESTATURA PADRES - HIJOS

father.son
plot(father.son$fheight, father.son$sheight, xlab = "ESTATURA PADRES", ylab = "ESTATURA HIJOS")

#  **Pearson's product-moment correlation**
cor.test(father.son$fheight, father.son$sheight)

# ESTUDIO DE CASO: COMPARACIÓN TAMAÑO ENTRE SEXOS
animal <- data.frame(Sexo=rep(c("Male", "Female"), each=10), Peso=c(rnorm(10, 180, 10),rnorm(10, 140, 10)))
boxplot(animal$Peso ~ animal$Sexo, xlab = "Sexo", ylab = "Peso")

# **Two Sample t-test**

t.test(Peso ~ Sexo, animal, alternative = c("two.sided"), var.equal=TRUE)

# ESTUDIO DE CASO: EVALUACIÓN CRECIMIENTO DE PLANTAS

# Creo objeto y grafica bosplot con ggplot2
my_data <- PlantGrowth
my_data%>% 
  ggplot(aes(x=group,y=weight,fill=group))+
  geom_boxplot()+
  theme(legend.position="none")+
  labs(x="Treatment",y="Weight")

# Crea objeto anova
res.aov <- aov(weight ~ group, data = my_data)

# Imprime resultado anova
anova(res.aov)

# Imprime resultado en formato tabla.
anova(res.aov) %>% 
  kable(caption = "Anova de una vía.",
                        digits=2)

# ESTUDIO DE CASO: EFECTO DE LA VITAMINA C
# EN EL CRECIMIENTO DE CONEJILLOS DE INDIA

my_data1 <- ToothGrowth
my_data1
my_data1$dose <- as.factor(my_data1$dose)
table(my_data1$supp, my_data1$dose)

my_data1%>% 
  ggplot(aes(x=dose, y=len, fill=supp))+
  geom_boxplot()+
  labs(x="Dossis",y="length of odontoblasts")


# Anova de dos vías con interacción
res.aov2 <- aov(len ~ dose * supp,
                data = my_data1)

# Imprime resultado anova
anova(res.aov2)

# Imprime resultado en formato tabla.
anova(res.aov2)%>% kable(caption = "Anova de dos vías.",
                         digits=3)
