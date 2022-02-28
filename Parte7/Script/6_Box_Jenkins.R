# Libreriaas ====
library(forecast)
library(urca)

# Data ====
data <- read.table("Data/PIB.txt")

ts.plot(data)

tsdata <- ts(as.vector(as.matrix(data)),
             start=c(2000,1),
             frequency = 4)

ts.plot(tsdata)

# Empleando auto.arima
modelo1 <- auto.arima(tsdata)
# El mejor modelo es un ARIMA(1,1,0)
accuracy(modelo1)

plot(modelo1)
# El modelo es estable, la raiz se encuentra dentro 
# del circulo unitario.

# Residuos
Box.test(modelo1$residuals,
         type = c("Ljung-Box"))
# H0: Residuos independientes

# No se tiene evidencia estadistica para rechazar H0

par(mfrow = c(1,2))
Acf(modelo1$residuals)
Pacf(modelo1$residuals)

# Proyeccion
f1 <- forecast(modelo1, h=3)
plot(f1)
par(mfrow = c(1,1))
hchart(f1)
