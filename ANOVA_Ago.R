setwd("G:/clases/UCC/2023_ucc_estad/Semana 13 agos")

library(readxl)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(glmm.hp)

# Importar Dataset de un archivo de excel

a <- read_excel("at1.xlsx")
names(a)
View(a)

#Confiramos normalidad

boxplot(Phi~Cat_atr, data=a,main='Atractivo',ylab='Phi')

## boxplot(y ~ X, )

a1<-ggplot(a, aes(Phi)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

a2<-ggplot(a, aes(log(Phi)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

a3<-ggplot(a, aes(sqrt(Phi)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

ggarrange(a1, a2, a3, ncol = 2, nrow = 2)

# Homogeneidad de Varianza; un p < 0.05 indica que 
#  la variables no son homgóneas (si hay diferencia)

bartlett.test(Phi~Cat_atr, data=a)

fligner.test(Phi~Cat_atr, data=a)

# Prueba de ANOVA

mod_1 <- lm(Phi~Cat_atr, data=a)
an1=anova(mod_1);an1
an2=aov(mod_1)

# Revisamos modelos

plot(mod_1, which=1)
plot(mod_1, which=2)

#Calculamos R-cuadrado

summary.lm(mod_1)$adj.r.squared

#Pprueba de comparación

TukeyHSD(an2)

# usando paquete agricolae

install.packages("agricolae")
library(agricolae)

LSD <-LSD.test(an2, "Cat_atr")
LSD

View(LSD$groups)

### Con los datos de las mujeres

