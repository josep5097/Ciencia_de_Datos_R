# Librerias ====
library(forecast)
library(urca)
library(highcharter)

# Data ====
data <- read.table("Data/PIB.txt")

# Convirtiendo a serie de tiempo
tsdata <- ts(as.vector(as.matrix(data)),
             start=c(2000,1),
             frequency = 4)

ts.plot(tsdata)

# ARIMA(p,d,q)
# p: # de autoregresivos
# d: # de diferenciadores de la serie
# q: # de las medias moviles

# Contrastes de estacionariedad ====

# Augmented-Dickey-Fuller Unit Root Test
testpp <- ur.pp(tsdata,
                type=c("Z-tau"),
                model=c("trend"),
                lag=c("short"))

summary(testpp)
# H0: raiz unitaria
# No se tiene tiene evidencia estadistica para 
# rechazar H0.

# Elliott, Rothenberg \& Stock Unit Root Test
testers <- ur.ers(tsdata, type=c("DF-GLS"),
                  model=c("trend"),
                  lag.max = 4)

summary(testers)
# Ho: Raiz unitaria
# No se tiene evidencia estadistica para rechazar H0.

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

par(mfrow=c(1,2))
plot(tsdata)
plot(diff(tsdata,1))

# Funcion de autocorrelacion simple
acf(diff(tsdata,1),xlim=c(0.2,4))
# Se observa una barra -> tal vez sea necesario de emplear
# una media movil de orden 1

# Funcion de correlacion parcial
pacf(diff(tsdata,1))
# Se observa una barra -> tal vez sea necesario de emplear
# un autoregresivo de orden 1

# Modelo ====
# Arima de orden 1,1,1
# Arima (p,d,q)
modelo1 <- Arima(tsdata,
                 order = c(1,1,1))
modelo1
# Los coeficientes deben de ser <1 en valor absoluto.

# Significancia individual de los parametros.

# H0: coeficiente=0 (Se desea rechazar esta hipotesis)
# H1: coeficiente =! 0
# Si t calculado > 2, RECHACE Ho.

# Calculo de t
# ar1/s.e
0.8892/0.0881
# ma1/s.e
-0.3379/0.1964

# valor t de ma <2
# No se rechaza la H0.
# Este proceso no aporta a la explicacion del modelo

# Realizar un nuevo modelo
# Modelo ARI
modelo2 <- Arima(tsdata,
                 order=c(1,1,0))
modelo2
# Valor t
0.74/0.09
# Es mayor que 2, si aporta 

# Capacidad predictiva del modelo
accuracy(modelo2)
# RMS, MAE, MAPE
# MAPE> error de pronostico en terminos porcentuales
# MAE> error de pronostico en valores absolutos.

# Evaluar los residuos
par(mfrow=c(1,1))
plot(modelo2$residuals)
abline(h=0)
# No se observa un patron sistematico.
# Se lo considera ruido blanco.

# Contraste Box-Pierce and Ljung-Box Tests

# H0:residuos independientes
# si p-value es < 0.05, rechace HO

Box.test(modelo2$residuals,
         lag = 4,
         type = c("Ljung-Box"))
# No se posee suficiente evidencia estadistica para
# rechazar la H0.

par(mfrow=c(1,2))
acf(modelo2$residuals,xlim=c(0.2,4))
pacf(modelo2$residuals,xlim=c(0.2,4))
# Puede existir un porblema de autocorrelacion de orden 1

# Pronostico ====
f1 <-forecast(modelo2, 
              h=4, #4 trimestres hacia adelante
              level=c(95,99)) 
par(mfrow=c(1,1))
plot(f1)

f1

hchart(f1)

ts.plot(tsdata)
ts.plot(tsdata,
        f1$fitted,
        f1$mean,
        col=c("red","blue","green"))