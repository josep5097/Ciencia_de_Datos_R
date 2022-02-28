# Modelo de Regresion Linela Multiple ====
# Autocorrelacion

# Librerias ====
library(forecast)
library(lmtest) #para contraste durbin watson.
library(nlme)
library(sandwich)
library(car)
library(ggplot2)
library(dynlm)

# Data ====
Regiones_DB <- read.csv("Data/REG_1.csv")
attach(Regiones_DB)

# Estimando el modelo 
# Oferta monetaria contra Gasto Publico+Tasa de interes+IPS
modelo <- lm(M1 ~ GP + TI + IPC, data=Regiones_DB)
summary(modelo)
# Este modelo es extremadamente satisfactorio, son estadisticamente significativo.
# R^2 -- El modelo ajusta el 96% de la informacion
# Sera correcto?

# Identificacion del problema ====

# La autocorrelacion afecta a las variables del tipo serie temporal.
# Los residuos del modelo tal vez tengan correlaci?n?

# Metodos para determinar si existe correlacion ====

## Metodo Grafico ====
# Graficando los residuos

plot(modelo$residual, 
     type = "b")

# Evaluar los residuos del modelo
# quick plot, 
# En donde las x son los valores de los residuos rezagando un periodo
# La gr?fica la centra en 0, en donde la referencia seran los residuos
qplot(x=c(tail(modelo$residuals,-1),0),
      y=modelo$residuals)
# Se pueden observar una relacion de los residuos rezados en el tiempo

## Contrastes ====

# Probando con contrastes formalmente establecidos
dwtest (modelo)

# Test DW de R proporciona el estadistico y el valor p
# HO: No Autocorrelacion
# H1: Autocorrelacion
# Si el valor p es significativo, se rechaza HO
# Si el valor de DW es menor que 2, hay evidencia de correlaci?n serial positiva

# Debido a esto se rechaza H0, con lo cual, la alternativa es correcta, y
# se establece que existe una autocorrelacion positiva.


# Contraste de Breusch-Godfrey
# Observar si la correlacion es de algun orden en especifico,
# dato para poder atenuar los residuos.
bgtest(modelo,order=1)
# H0: No autoc de orden 1
# H1: Autoc de orden 1
## Se obtiene que se tiene una autocorrelacion de orden 1.

## Metodos de atenuacion ====

# Atenuando el problema
# Como es de orden 1, se puede realizar por el metodo de MCG

#### Minimos Cuadrados Generalizados ====
# A traves del metodo de maxima verosimilitud, encuentra la estructura
# de correlacion, es decir el ro
# Se trata de un metodo de invasivo, aqui los betas se ven modificados

# Minimos cuadrados generalizados - gls
modelfitted <- gls(model = M1 ~ GP + TI + IPC, 
                   # Correlaci?n de orden AR1
                   correlation = corAR1(),
                   # M?todo de M?xima verosimilitud
                   method = "ML")
summary(modelfitted)
##### Analisis: ====
# El phi, ro, o coef de corr que se obtiene es de 0.8566
# Se corrige toda la data y se obtiene un nuevo valor para los betas
# Ya no presentan problemas de autocorrelacion

### Errores estandar libre de autocorelacion ====
# Este es un metodo no invasivo

coeftest(modelo, 
         vcov = NeweyWest(modelo))
summary(modelo)

# Los betas siguen siguendo los mismos, sin embargo, los errores
# estandares se ven modificados, a?n as? el modelo obtenido sigue siendo
# significativo


### Metodo con rezagos ====
# Metodo con rezagos se aplica cuando se trata de series de tiempo.

# Regresion con rezago

# Definamos como  serie temporal la matriz usada

tsRegiones_DB <- ts(Regiones_DB,start=c(2003,2),frequency = 12)

ts.plot(tsRegiones_DB[,4])

# Incorporemos la variable dependiente rezagada L(Var, #Rezagos)
modelolag <- dynlm(M1 ~ GP + TI + IPC + L(M1,1),
                   data = tsRegiones_DB)
summary(modelolag)

# Se observan que los betas se ven afectados, sin embargo, al trabajar 
# mediante TS, se puede contrastar debido a que se toman en consideracion
# los eventos pasados.

# Comprobando si atenuacion funciono
# DW se empleado donde no se considere las variables no rezagadas
dwtest (modelolag)
# Al tener una variable rezagada en este caso M1, NO SE DEBE de emplear

# Contraste idoneo Breusch-Godfrey
bgtest(modelolag,order=1)

# H0: No Autoc de orden 1
# H1: Autoc de orden 1

# Como el p-value no es significativo, no se puede negar la hipotesis H0.
# Por lo que no es autocorrelacionado!