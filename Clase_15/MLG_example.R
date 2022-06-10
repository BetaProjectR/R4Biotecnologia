# ----------------------------------------------------------
# Clase 15 - Script Modelos Lineales Generales
# Dr. José Gallardo y Dra. María Angélica Rueda
# 07 junio 2022
# DBT 845 - Investigación reproducible y análisis de datos biotecnológicos con R
# ----------------------------------------------------------

# Paquetes   
library(car)
library(lmtest)
library(psych)
library(readxl)
library(nlme)
library(lme4)
library(stats)
library(boot)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpmisc)
library(knitr)
library(car)
library(lmtest)
library(psych)
library(gridExtra)
library(nortest)
library(lme4)
library(reshape2)

# **REGRESIÓN NO LINEAL CUADRÁTICA**

# **ESTUDIO DE CASO 1: TASA DE ACLARACIÓN MOLUSCOS FILTRADORES**

clearance <-  read_excel("ParticleClearance.xlsx", sheet = 1)

# Create data filters
mussel <- filter(clearance, sample == "mussel")
control <- filter(clearance, sample == "control")

# Formato de gráficas ggplot2
My_Theme = theme(
  axis.title.x = element_text(size = 20),
  axis.text.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(size = 20))

microplot <- ggplot(data = mussel, aes(x = time, y = microparticle_concentration)) +
  geom_point(position = position_jitter(w = 0, h = 0.1) ) +
  labs(x = "Time (minutes)", y = expression(Concentration~microparticles~ml^-1)) +
  scale_shape_manual(values=c(1,2)) +
  stat_smooth(method='loess',formula=y~x, se=T)+
  scale_color_brewer(palette="Set1") + 
  theme(legend.position="none") +
  theme(panel.border=element_blank(), axis.line=element_line())
microplot+My_Theme


# **MODELO LINEAL** 

reg_mussel <- lm(log_microparticle_concentration ~ time, data=mussel)

summary(reg_mussel)$coef %>% kable()

# R^2 modelo lineal
round(summary(reg_mussel)$r.squared,2)

# p-val modelo lineal
anova(reg_mussel)$'Pr(>F)'[1]

# **MODELO NO LINEAL (INCLUYE TÉRMINO CUADRÁTICO)**

reg_mussel_2 <- lm(log_microparticle_concentration ~ poly(time,2), data=mussel)

summary(reg_mussel_2)$coef %>% kable()

# R^2 modelo cuadrático
round(summary(reg_mussel_2)$r.squared,2)

# p-val modelo  cuadrático
anova(reg_mussel_2)$'Pr(>F)'[1]

# **COMPARACIÓN DE MODELOS** 

# Modelo 1: log_microparticle_concentration** = $\beta_{0} + \beta_{1} time + \epsilon$

# Modelo 2: log_microparticle_concentration** = $\beta_{0} + \beta_{1} time + \beta_{2} {time}^2 + \epsilon$

anova(reg_mussel,reg_mussel_2) %>% kable()


# **REGRESIÓN LOGÍSTICA**

# **REGRESIÓN LOGÍSTICA SIMPLE**

# **ESTUDIO DE CASO 2: MADURACIÓN EN SALMÓN DEL ATLÁNTICO**

maduracion <- read_excel("Maturation.xlsx")

maduracion$Genotype <- as.factor(maduracion$Genotype)

maduracion <- maduracion%>% 
              select("Fish","Genotype","Gonad","GSI","Maturation")


mod_lineal <- lm(Maturation ~ Gonad, data = maduracion)
mod_logit <- glm(Maturation ~ Gonad, 
                family= binomial, data = maduracion)
formula1 <- y ~ x

# **RELACIÓN ENTRE MADURACIÓN VS PESO DE GÓNADA**

p<- ggplot(data = maduracion, aes(x = Gonad, y = Maturation)) +
  geom_point(aes(color = as.factor(Maturation)), shape = 1) + 
  theme(axis.text.x = element_text(size = 10,face="bold",colour="black"))+
  theme(axis.text.y = element_text(size = 10,face="bold",colour="black"))+
  theme_bw()  +
  labs(x= "Peso de gónada", y = "Maduración")+
  theme(legend.position = "none")+ 
  theme(panel.border=element_blank(), axis.line=element_line())
p+My_Theme


# **REGRESIÓN LINEAL ENTRE MADURACIÓN VS PESO DE GÓNADA**

q <- ggplot(data = maduracion, aes(x = Gonad, y = Maturation)) +
  geom_point(aes(color = as.factor(Maturation)), shape = 1) + 
  theme(axis.text.x = element_text(size = 10,face="bold",colour="black"))+
  theme(axis.text.y = element_text(size = 10,face="bold",colour="black"))+
  geom_smooth(method = "lm", color = "gray20", se = FALSE) +
  stat_poly_eq(aes(label = paste0("atop(", ..eq.label.., ",", ..rr.label.., ")")), formula = formula1, parse = TRUE, size = 8)+
  theme_bw()  +
  labs(x="Peso de gónada",y = "Probabilidad de Maduración") +
    scale_y_continuous(limits = c(0, 1)) +
  theme(legend.position = "none")+
  theme(panel.border=element_blank(), axis.line=element_line())
q+My_Theme


# **MODELO LINEAL**

# **Maduración** = $\beta_{0} + \beta_{1}$ Peso de gónada + $\epsilon$

mod_lineal <- lm(Maturation ~ Gonad, data = maduracion)
summary(mod_lineal)$coef %>% kable()

# R^2 modelo regresión lineal
round(summary(mod_lineal)$r.squared,2)

# p-val modelo  lineal
anova(mod_lineal)$'Pr(>F)'[1]


# **PREDICCIÓN LOGÍSTICA**

# Representación gráfica del modelo.
s <- ggplot(data = maduracion, aes(x = Gonad, y = Maturation)) +
  geom_point(aes(color = as.factor(Maturation)), shape = 1) + 
  theme(axis.text.x = element_text(size = 10,face="bold",colour="black"))+
  theme(axis.text.y = element_text(size = 10,face="bold",colour="black"))+
  geom_hline(aes(yintercept=0.50), color="red")+ 
  stat_function(fun = function(x){predict(mod_logit,
                                          newdata = data.frame(Gonad = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística", x="Peso de gónada",
       y = "Probabilidad de Maduración") +
  theme(legend.position = "none")+
  theme(panel.border=element_blank(), axis.line=element_line())
s+My_Theme


# **REGRESIÓN LOGÍSTICA SIMPLE**

mod_logit <- glm(Maturation ~ Gonad, 
                family= binomial, data = maduracion)
summary(mod_logit)$coef %>% kable()


# EJEMPLO PREDICCIÓN DE AMBOS MODELOS

# Predicción con modelo lineal
#Predecimos si un salmón madura o no para un peso de gónada de 4
Prob.mad_lm <- data.frame(Gonad=4)
Prediccion_lm <- predict(mod_lineal, Prob.mad_lm, type = "response")
Prediccion_lm <- data.frame(Prediccion_lm)
colnames(Prediccion_lm) <- c("Probabilidad de maduración")

Prediccion_lm%>% kable() 

if (Prediccion_lm >= 0.5) {
  print("Madura")
}else{
  print("No madura")
}


# Predicción con modelo de regresión logística
#Predecimos si un salmón madura o no para un peso de gónada de 4

Prob.mad <- data.frame(Gonad=4)
Prediccion_logis <- predict(mod_logit, Prob.mad, type = "response")
Prediccion_logis <- data.frame(Prediccion_logis)
colnames(Prediccion_logis) <- c("Probabilidad de maduración")


Prediccion_logis%>% kable() 

if (Prediccion_logis >= 0.5) {
  print("Madura")
}else{
  print("No madura")
}

# **RELACIÓN ENTRE MADURACIÓN VS GENOTIPO**

# Genotipo E = Maduración temprana o Early.  
# Genotipo L = Maduración tardía o Late.  
# ¿Qué genotipo tiene mayor probabilidad de maduración?  

table(maduracion$Maturation, maduracion$Genotype) %>% 
  kable()

# **REGRESIÓN LOGÍSTICA MÚLTIPLE**


mod_logit_mult <- glm(Maturation ~ Gonad + 
                      Genotype,family= binomial, 
                      data = maduracion)
summary(mod_logit_mult)$coef %>% kable()


# **REGRESIÓN LOGÍSTICA (MODELO NULO)**
# SOLO INTERCEPTO
mod_nulo <- glm(Maturation ~ 1, 
                family= binomial, data = maduracion)
summary(mod_nulo)$coef %>% kable()



# **COMPARACIÓN DE MODELOS AIC**
AIC(mod_nulo,mod_logit,mod_logit_mult)%>% kable()

BIC(mod_nulo,mod_logit,mod_logit_mult)%>% kable()


# **COMPARACIÓN DE MODELOS (ANOVA)**
anova(mod_nulo,mod_logit,mod_logit_mult, test ='Chisq')%>% kable()

    