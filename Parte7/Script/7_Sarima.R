# Libreria ====
library(forecast)
library(urca)
library(highcharter)
library(openxlsx)

# Data ====
x <- read.xlsx("Data/base.xlsx")
ts.plot(x[,2])

# Transformando a serie de tiempo
# para ventas netas
ventas <- ts(x[,2],
          start=c(2006,1),
          frequency=12)

ts.plot(ventas)

# Test de Raiz Unitaria ====
monthplot(log(ventas))

#SARIMA(p,d,q)(P,D,Q)
nsdiffs(log(ventas), 
        test=c("ocsb"))
nsdiffs(log(ventas), 
        test=c("ch"))
# No requiere diferenciar la parte estacional de la serie

# Contrastes 
# Primero
kpss1<-ur.kpss(log(ventas),
               type=c("tau"),
               lags = "short")
summary(kpss1)
# No se rechaza H0, es estacional
# Segundo
ers1<-ur.ers(log(ventas),
             type=c("DF-GLS"),
             model=c("trend"))
summary(ers1)
# Se rechaza la raiz unitaria.

# Tercero
pp1<-ur.pp(log(ventas),
           type="Z-tau",
           model=c("trend"))
summary(pp1)
# Se rechaza la hipotesis de raiz unitaria.

# Cuarto
adf1<-ur.df(log(ventas),
            type=c("trend"),
            selectlags = c("BIC"))
summary(adf1)
# se rechaza H0.

# Correlogramas ====
par(mfrow=c(1,2))
Acf(log(ventas))
# Detecta el SMA

Pacf(log(ventas))
# Detecta el SAR

# Modelo ====
# Arima(p,d,q)(P,D,Q)
m1 <- Arima(log(ventas), 
            order = c(1,0,1),
            seasonal=list(order=c(1,0,1)))
m1

# Evaluacion del modelo ====
# Precision del modelo
accuracy(m1)

# Residuos
par(mfrow=c(1,1))

Box.test(m1$residuals,
         type=c("Ljung-Box"),
         lag = 6)
plot(m1$residuals)
#Ho:Residuos independientes
# Como el p-value no es menor que 0.05, no se rechaza H0

# Pronostico ====
f1.1 <- forecast(m1,h=12)
hchart(f1.1)

# Se esta trabajando con logs
ts.plot(ventas,
        exp(f1.1$fitted),
        (exp(f1.1$mean)), 
        col=c("blue","red","brown"))


# Grafica original y proyectada
str(f1.1)
cols <- c("blue","red","brown")
ts.plot(ventas, 
        exp(f1.1$fitted),
        exp(f1.1$mean),
        col=cols)
legend("topleft", c("original",
                    "ajustado",
                    "proyectado"), 
       cex=1.2,
       fill=cols)

# Modificando el modelo
m3 <- arima(log(ventas), 
            order = c(4,0,1),
            fixed=c(NA,0,0,0,NA,NA,0,NA),
            seasonal=list(order=c(1,0,1)))
m3



accuracy(m3)
accuracy(m1)
AIC(m3)
AIC(m1)
BIC(m1)
BIC(m3)