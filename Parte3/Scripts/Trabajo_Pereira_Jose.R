# Librerias ==== 
library(forecast)
library(lmtest)
library(nlme)
library(sandwich)
library(car)
library(ggplot2)
library(dynlm)
library(openxlsx)
library(GGally)
library(strucchange)
library(memisc)
library(moments)
library(nortest)

# Data ==== 
Reg_BD <- read.xlsx("Data/enemde_db.xlsx")
attach(Reg_BD)

# Modelo Establecido 

# Se determina el modelo de regresion lineal multiple
# para determinar la relacion que existe entre las variables 
# Monto recibido frente al Sexo y area

# Se describen las variables:
# Salario: Es una variable cuantitativa - Monto de dinero ganado en el mes de noviembre 2020
# Sexo: Variable dicotomica: 
#       0 - Mujer
#       1 - Hombre
# ?rea: Variable dicotomica
#       0 - Urbana
#       1 - Rural

# Modelo de Regresion Lineal ====

# Se establece el modelo de regresion lineal
modelo <- lm(Salario ~ Sexo + area, 
             data=Reg_BD)

summary(modelo)

# Antes de analizar el modelo obtenido se observa si existe problemas en la estimacion

## Contrastes problemas de Estimacion ====
### Autocorrelaci?n ====
qplot(x=c(tail(modelo$residuals,-1),0),
      y=modelo$residuals)
# Se pueden observar una relacion de los residuos rezagados en el tiempo

# Contrastes:
# Probando con contrastes formalmente establecidos
dwtest (modelo)

# HO: No Autocorrelacion
# H1: Autocorrelacion

# Debido a que el valor DW es menor que 2 y el valor p,
# es significativo se rechaza H0, con lo cual, 
# se establece que existe una autocorrelacion positiva.

# Contraste de Breusch-Godfrey
bgtest(modelo,order=1)

# H0: No autoc de orden 1
# H1: Autoc de orden 1
# Como el valor p, es significativo, 
# se obtiene que se tiene al menos una autocorrelaci?n de orden 1.

# Correccion
# Minimos cuadrados generalizados - gls
modelfitted <- gls(Salario ~ Sexo + area,
                   data = Reg_BD,
                   # Correlaci?n de orden AR1
                   correlation = corAR1(),
                   # M?todo de M?xima verosimilitud
                   method = "ML")

summary(modelfitted)
#### Analisis ====
# El phi, ro, o coef de corr que se obtiene es de 0.9986
# Se corrige toda la data y se obtiene un nuevo valor para los betas
# Ya no presentan problemas de autocorrelacion, sin embargo, se obtiene que los 
# coeficientes no son significativos para el an?lisis 

# Se prueba con un modelo no invasivo
model_newey <- coeftest(modelo, 
                        vcov = NeweyWest(modelo))
summary(model_newey)
# Los betas siguen siguendo los mismos, sin embargo, los errores
# estandares se ven modificados, aun asi el modelo obtenido sigue siendo
# significativo

### Multicolinealidad ====
# Identificaci?n del problema 
Nueva <- data.frame(Reg_BD$Salario,Reg_BD$area,Reg_BD$Sexo)

# Generando la matriz de correlacion
cor(Nueva,use="complete.obs")

# Realizar un grafico de corr materializado
pairs(Nueva)

# GGallery, gr?fico de correlacion
ggpairs(Nueva)

# De forma grafica no se aprecia que exista Multicolinealidad

# factor de inflacion de la varianza
vif(modelo)
# Con un valor de 1 en los factores de inflacion de la varianza
# Se aprecia que no se tiene problema de multicolinealidad

### Normalidad ====
# Se prueban los siguientes 3 contrastes para normalidad
# El contraste toma como vector los residuos
jarque.test(as.vector(modelo$residuals))
ad.test(modelo$residuals)
shapiro.test(modelo$residuals)
# H0: Normalidad
# H1: No normalidad

# A pesar de que los residuos no siguen normalidad, se tiene un modelo 
# representativo.

### Heterocedasticidad ====

qplot(x=log(Reg_BD$Sexo),
      y=(modelo$residuals))+
  geom_point()

qplot(x=log(Reg_BD$area),
      y=(modelo$residuals))+
  geom_point()

# Mediante grafica no es posible determinarlo

# Contraste Breusch-Pagan
bptest(modelo)

# Si el valor p es sig, rechazo hipotesis nula
# HO: Homo
# H1: Hetero

# Debido a que el valor p no es significativo,
# se determina que no existe problema de heterocedasticidad

# Analisis General ====
# Se observa que con el modelo corregido la ecuaci?n de regresi?n lineal que se 
# obtiene es:
# Salario = 81.08 + 96.65*Hombre - 55.81*Rural
# En el cual, los valores probabilisticos son significativos.
# Es decir que se obser que: 
# El salario tiene un incremente de 96.65 si se trata de un trabajor de sexo 
# hombre, mientra que, si el ?rea que se encuentra es rural, su salario obtiene 
# un decremento de 55.81 dolares.