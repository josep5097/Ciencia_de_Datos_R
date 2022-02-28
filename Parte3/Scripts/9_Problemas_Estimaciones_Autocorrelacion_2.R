# Modelo de Regresion Lineal Multiple ====
# Autocorrelacion

# Librerias ====
library(forecast)
library(lmtest) #para contraste durbin watson.
library(nlme)
library(sandwich)
library(car)
library(ggplot2)
library(dynlm)
library(openxlsx)

# Data ==== 
Reg_Data <- read.xlsx("Data/exportaciones.xlsx")
attach(Reg_Data)

# Estimando el modelo ====
modelo <- lm(EXUSA ~ TCR + YPUSA, 
             data=Reg_Data)
summary(modelo)
# Parece que el modelo esta bastante adecuado para la toma de decisiones
# Toca revisar los resultados del modelo

# Identificacion del problema ====
## Graficando los residuos ====

plot(modelo$residual, type="b")
# Se observa que los residuos no son tan aleatorios, es decir no son 
# ruido blanco, puede tener problema de autocorrelacion.

### Rezagando un periodo ====
qplot(x=c(tail(modelo$residuals,-1),0),
      y=modelo$residuals)
# Parece que existe una autocorrelacion en los residuos

# Contrastes ====

# Probando con contrastes formalmente establecidos
# Aplicando DW
dwtest (modelo)

#Test DW de R proporciona el valor p
#HO: No Autocorrelacion
#H1: Autocorrelacion
#Si el valor p es significativo, se rechaza HO

# Como el valor p es significativo, se rechaza H0, con lo cual,
# Se establece que existe autocorrelaci?n.
# Con DW = 0.68193, se establece que es una autocorrelacion positiva.

# Aplicando el criterio 
# Breusch-Godfrey, para observar el orden existente
bgtest(modelo,order=1)

# El Test:
# H0: No existe autocorrelaci?n serial de orden 1.
# H1: Existe autocorrelaci?n de orden 1.
# Dado que p value es significativo, se rechaza H0, con lo cual se establce
# que existe autocorrelaci?n de al menos orden 1.

# Atenuando el problema ====
# Se ajusta el modelo.

# Minimos Cuadrados Generalizados
# Metodo invasivo.
modelfitted <- gls(model = EXUSA ~ TCR + YPUSA,
                   correlation = corAR1(),
                   method = "ML")
summary(modelfitted)

# Metodo NeweyWest - Metodod no invasivo
#Errores estandar libre de autocorelacion
coeftest(modelo, 
         vcov = NeweyWest(modelo))

summary(modelo)
# Se establece que los coeficientes son iguales, sin embargo,
# el error estandar se encontraba subestimado, por lo que la 
# prueba t no era certera en el modelo inicial.

# Regresion con rezago ====
# Se considera rezagos de la variable dependiente

# Primero Series temporales:
# Definamos como  serie temporal la matriz usada
tsReg_Data <- ts(Reg_Data,start=c(2004,1),
                 frequency = 12)

# Incorporemos la variable dependiente rezagada
# Dynamic linear models and time series regression
modelolag <- dynlm(EXUSA ~ TCR + YPUSA+L(EXUSA,1),
                   data = tsReg_Data)

summary(modelolag)
# Se observa que las variables dependientes son significativas para el modelo

# Comprobando si atenuacion funciono
# El estadistico DW, no se consideren var dep rezagadas!
# No Aplicar
dwtest (modelolag)

# Aplicando el estadistico BG, con un orden 1
bgtest(modelolag,order=1)
# Con el rezago de orden 1 sigue siendo un valor de correlacion minimo.
# Se debe de aplicar un rezago de orden 2 para un mejor resultado!