# Librerias ====
library(forecast)
library(urca)
library(openxlsx)
library(highcharter)

# Data ====
data <- read.xlsx("Data/base.xlsx")

exportaciones <- data[,4]

plot(exportaciones,
     type="l")

# Realizarlo a serie de tiempo
tsexportaciones <- ts(exportaciones,
                      start=c(2006,1),
                      frequency = 12)

plot(tsexportaciones)

# Pruebas de contrastes ====
## Augmented-Dickey-Fuller Unit Root Test ====
adftest <- ur.df(tsexportaciones,
                 type=c("trend"),
                 selectlags = c("BIC"))
summary(adftest)
# H0: Raiz Unitaria
# H1: no raiz unitaria

## Phillips \& Perron Unit Root Test ====
pptest <- ur.pp(tsexportaciones,
                type=c("Z-tau"),
                model=c("trend"),
                lags=c("short"))

summary(pptest)
# H0: Raiz Unitaria
# H1: no raiz unitaria
# La serie parece que no tiene raiz unitaria.
## Kwiatkowski et al. Unit Root Test ====
kpsstest <- ur.kpss(tsexportaciones,
                    type=c("tau"),
                    lags=c("short"))

summary(kpsstest)
# H0: Estacionariedad
# H1: No estacionariedad

## Elliott, Rothenberg \& Stock Unit Root Test ====
erstest <- ur.ers(tsexportaciones,
                  type=c("DF-GLS"),
                  model=c("trend"),
                  lag.max = 4)
summary(erstest)
#Ho: Raiz Unitaria
#H1: No raiz unitaria
# Se rachaza H0


# Modelo ====
plot(tsexportaciones)

par(mfrow=c(1,2))
Acf(tsexportaciones)
Pacf(tsexportaciones)
# La funcion de autocorrelacion simple, tiene es forma
# cuando una serie presenta una tendencia pronunciada.
# No requiere decir que se debe de incluir tantas medias
# moviles al modelo.
# Se deberias de generar 3 de regresion 
# AR1, AR2, AR4
# MA1

## Sin diferenciar ====
# ARIMA(p,d,q)
# p: maximo rezago =4
# d: 0, sin diferenciar
# q: 1, 1 media movil

modelo1<- Arima(tsexportaciones,
                order=c(4,0,1),
                # Como no esta diferenciado, la constante
                # se considera
                fixed=c(NA,0,0,NA,NA,NA))

modelo1

# Obtener los coeficientes
as.matrix(subset(modelo1$coef,abs(modelo1$coef)>0))
as.matrix(diag(modelo1$var.coef))

as.matrix(subset(modelo1$coef,
                 abs(modelo1$coef)>0))/as.matrix(diag(modelo1$var.coef))
# Se observa que tras aplicar la prueba de significancia individual
# Todos los coef son significativos.

# Evaluacion del modelo ====
accuracy(modelo1)
# Generar graficos de raiz unitaria
source("Script/Funciones/Compute_Arma_Inverse.R", local = T)
plot(modelo1)

# Residuos ====
Box.test(modelo1$residuals,
         type=c("Ljung-Box"))
# H0: Residuos independientes
# No se tiene evidencia estadistica para rechazar
# H0.

# Correlograma para los residuos
Acf(modelo1$residuals)
Pacf(modelo1$residuals)

# Pronosticos
hchart(forecast(modelo1,h=3,level=c(95)))

f1 <- forecast(modelo1,h=3,level=c(95))

par(mfrow=c(1,1))
ts.plot(tsexportaciones,f1$fitted,
        f1$mean,
        col=c("red","blue","green"))