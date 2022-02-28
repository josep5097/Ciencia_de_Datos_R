# Librerias ====
library(forecast)
library(urca)
library(openxlsx)
library(highcharter)
library(dplyr)
# Data ====
Datos <- read.csv("Data/Exterior_Exportaciones_Totales_mensual_puntoycoma.csv",
                              stringsAsFactors=F,
                              header=T,sep = ";",dec = ",")
str(Datos)
attach(Datos)
Datos_Exportaciones <- filter(Datos, 
                              Variable == "Total Exportaciones FOB",
                              Year_Data >= 2000)
# Datos>
# Desde: 2000-1
# Hasta:2021-7
Datos_Exportaciones <- select(Datos_Exportaciones, Valor)

ts.plot(Datos_Exportaciones)

tsdata <- ts(as.vector(as.matrix(Datos_Exportaciones)),
             start=c(2000,1),
             frequency = 12)

ts.plot(tsdata)
hchart(tsdata)

plot(tsdata,
        main="Total de Exportaciones FOB en millones de dolares FOB",
        sub="Fuente: BCE",
        ylab="Total de Exportaciones En Millones",xlab="Periodo mensual",
        col="blue")
abline(v=2000); abline(v=2005);abline(v=2010); abline(v=2015); abline(v=2020)


# Contrastes de estacionariedad ====

# Augmented-Dickey-Fuller Unit Root Test
testpp <- ur.pp(tsdata,
                type=c("Z-tau"),
                model=c("trend"),
                lag=c("short"))

summary(testpp)
# H0: raiz unitaria
# Se observa que el valor Z-tau no es mayor que los valores criticos
# Lo cual nos indica que no se ofrece la evidencia estadistica
# para rechazar H0.
# Lo cual nos indica que se tiene problemas de raiz unitaria.

# Elliott, Rothenberg \& Stock Unit Root Test
testers <- ur.ers(tsdata, type=c("DF-GLS"),
                  model=c("trend"),
                  lag.max = 4)

summary(testers)
# Ho: Raiz unitaria
# No se tiene evidencia estadistica para rechazar H0.
# Segundo test que indica raiz unitaria.

# Uso de forecast ====
# Number of differences required for a stationary series
ndiffs(tsdata,
       test=c("kpss"))
# Nos indica que mediante el test kpss, 
# la serie se la debe de diferenciar 1 vez.

ndiffs(tsdata,
       test=c("adf"))
# Mediante adf, nos indica que se debe diferenciar
# una vez la serie.


# Seccion B ====
# Grafica ====
par(mfrow=c(1,2))
plot(tsdata)
plot(diff(tsdata,1))


acf(tsdata)
pacf(tsdata)
# Los datos obtenidos muestra que existe una tendencia en la 
# funcion de autocorrelacion simple.
# Lo cual impolica que se debe de comprobar con una media movil.
# La funcion de corelacion parcial, tal vez sea necesario de emplear
# 2 autorregresivos.

# Modelo ====
# Arima (p,d,q)
modelo1 <- Arima(tsdata,
                 order = c(1,1,1))
modelo1
# Los coeficientes son menores a 1.

# Significancia individual de los parametros.
# H0: coeficiente=0 
# H1: coeficiente =! 0
0.2502/0.1444
0.0821/0.1429
# Estos coeficientes no aportan al modelo

modelo1 <- Arima(tsdata,
                 order = c(1,0,1))
modelo1
0.9908/0.0082
0.291/0.0548
# Estos coeficientes si aportan al modelo.

# Capacidad predictiva del modelo
accuracy(modelo1)
# El modelo se obsrva que posee buenos margenes de precision.

# Residuos
# Evaluar los residuos
par(mfrow=c(1,1))
plot(modelo1$residuals)
abline(h=0)

# Test
Box.test(modelo1$residuals,
         type = c("Ljung-Box"))
# H0: Residuos independientes
# No se posee la evidencia estadistica para rechazar H0.

# Pronostico ====
f1 <-forecast(modelo1, 
              h=3, #3 pronosticos fuera de la muestra
              level=c(95,99)) 
par(mfrow=c(1,1))
plot(f1)
hchart(f1)

ts.plot(tsdata)
ts.plot(tsdata,
        f1$fitted,
        f1$mean,
        col=c("red","blue","green"))