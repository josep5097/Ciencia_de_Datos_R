# Pruebas de Normalidad ====

# Librerias ====
library(moments)
library(nortest)

# Creacion de un vector normal
unaNormal <- seq(-3,3,0.01)
plot(unaNormal, dnorm(unaNormal)) 
abline(v=0)

# Densidad normal
plot(unaNormal, pnorm(unaNormal))

# Poner ambas curvas en un plot
par(mfrow = c(1,2))

# Curva Izq
plot(unaNormal, dnorm(unaNormal)) 
abline(v=0)

# Curva Der
plot(unaNormal, pnorm(unaNormal))

Inform <- read.csv("Data/REG_1.csv")

Inform <- Inform[2:6]
# Grafica de 1f y 1c 
par(mfrow = c(1,1))

hist(Inform$M1)

# Realizar contrastes de normalidad para determinar
# si posee una dist norm
# Usando Shapiro test

# Regla General de la estadistica:
#   Si p<0.05 -> Rechaza H0, donde
#     H0: Normalidad
#     H1: No normal

shapiro.test(na.omit(Inform$M1))

# Con p = 0.01394 -> No sigue una distribucion normal

# Usando el contraste jarque-Bera -> No parametrico
# JB cercano a 2 o p no significativo para que no se rechace H0
jarque.test(as.vector(na.omit(Inform$M1)))
# Con p>0.05 no se rechaza

# Usando el contraste Anderson-Darling de lib nortest
ad.test(na.omit(Inform$M1))
# Con p>0.05 --- Sigue una dist normal


# Kurtosis
kurtosis(Inform$M1)
# kurtosis alejada de 3!, es una dist normal sesgada.
# No es simetrica completa!

# Asimetria
skewness(Inform$M1)
# Informacion cercana a cero, posee un rango asim?trico