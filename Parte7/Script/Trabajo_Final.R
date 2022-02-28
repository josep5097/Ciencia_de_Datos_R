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
Datos_Importaciones <- filter(Datos, 
                              Variable == "Total Importaciones FOB",
                              Year_Data >= 2000)

# Datos>
# Desde: 2000-1
# Hasta:2021-7
Datos_Importaciones <- select(Datos_Importaciones, Valor)

ts.plot(Datos_Importaciones)

tsdata <- ts(as.vector(as.matrix(Datos_Importaciones)),
             start=c(2000,1),
             frequency = 12)

ts.plot(tsdata)
hchart(tsdata)

plot(tsdata,
        main="Total de Importaciones FOB en millones de dolares FOB",
        sub="Fuente: BCE",
        ylab="Total de Importaciones En Millones",xlab="Periodo mensual",
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
# Graficas ====
par(mfrow=c(1,2))
plot(tsdata)
plot(diff(tsdata,1))

par(mfrow=c(1,2))
acf(tsdata)
pacf(tsdata)
# Los datos obtenidos muestra que existe una tendencia en la 
# funcion de autocorrelacion simple.
# Lo cual impolica que se debe de comprobar con una media movil.
# La funcion de corelacion parcial, tal vez sea necesario de emplear
# 2 autorregresivos.

# Modelo ====
# Modelo AutoArima
modelo <- auto.arima(tsdata)
modelo
# El modelo que nos devuelve se trata del siguiente:
# ARIMA(0,1,3)(0,0,2)[12]
# El cual se trata de un modelo SARIMA

# Empleando el modelo optimo.
modelo1 <- Arima(tsdata,
                 order = c(0,1,3),
                 seasonal=list(order=c(0,0,2)))
modelo1
# Los coeficientes son menores que 1 en valor absoluto.

# Prueba de significancia individual de los parametros
as.matrix(subset(modelo1$coef,
                 abs(modelo1$coef)>0))/as.matrix(diag(modelo1$var.coef))

# H0: coeficiente=0 
# H1: coeficiente =! 0

# Coeficientes >2, se rechaza H0, debido a que existe evidencia estadistica 
# para indicar que los coeficientes aportan al modelo.

# Capacidad predictiva del modelo ====
accuracy(modelo1)
# El modelo se obsrva que posee buenos margenes de precision.

# Residuos ====
## Evaluar los residuos
par(mfrow=c(1,1))
plot(modelo1$residuals)
abline(h=0)
# Se aprecia que no existe un patron de distribucion dentro de los residuos.

## Test ====
Box.test(modelo1$residuals,
         type = c("Ljung-Box"))
# H0: Residuos independientes
# No se posee la evidencia estadistica para rechazar H0.
# Con el resultado de esta prueba se confirma el punto anterior.

# Pronostico ====
f1 <-forecast(modelo1, 
              h=4, #4 pronosticos fuera de la muestra
              level=c(95,99)) 
par(mfrow=c(1,1))
plot(f1)
hchart(f1)

ts.plot(tsdata)

# Grafica original y proyectada
cols <- c("black","blue","red")
ts.plot(tsdata, 
        f1$fitted,
        f1$mean,
        col=cols)
legend("topleft", c("original",
                    "ajustado",
                    "proyectado"), 
       cex=1.2,
       fill=cols)


