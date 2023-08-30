setwd("G:/clases/UCC/2023_ucc_estad/Semana 21 ago")

library(readxl)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(glmm.hp)

# Importar Dataset de un archivo de excel

ad <- read_excel("at.xlsx")
names(ad)
View(ad)

#Confiramos normalidad

boxplot(y~x, data=ad,main='título',ylab='#')

## boxplot(y ~ X, )

a1<-ggplot(a, aes(y)) +
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

a2<-ggplot(a, aes(log(y)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

a3<-ggplot(a, aes(sqrt(y)))+
  geom_histogram(aes(y =..density..), fill = "orange") +
  geom_density()

ggarrange(a1, a2, a3, ncol = 2, nrow = 2)

# Homogeneidad de Varianza; un p < 0.05 indica que 
#  la variables no son homgóneas (si hay diferencia)

bartlett.test(y~x, data=ad)

fligner.test(y~x, data=ad)

# Prueba de ANOVA

mod_1 <- lm(log(y~x, data=ad)
an1=anova(mod_1);an1
an2=aov(mod_1)

# Revisamos modelos

plot(mod_1, which=1)
plot(mod_1, which=2)

#Calculamos R-cuadrado

summary.lm(mod_1)$adj.r.squared

# Calculamos TUkey

p1<-TukeyHSD(an2, conf.level=.95, )
plot(p1)
