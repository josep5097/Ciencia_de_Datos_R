# Modelo de regresion lineal ====

# Modelos log-lineal
# Se los conoce tambien como 
# Modelo tasa de crecimiento relativa compuesta

# y =b0+b1x1

# En este modelo se emplea:

#   ln(y)=b0+b1*TREND

# Trend: variable contadora o de tendencia

plot(seq(0,100,1))
#b1.... (exp(b1)-1)*100 = tasa de crecimiento relativa compuesta

plot(seq(0,100,1),type="l")

## Creacion del Modelo ====

### Libreria ====
library(forecast)

### Data ====
base <- read.csv("Data/REG_1.csv")

# Se trata de una serie temporal
# Convertir un data frame a una time series
# Se obtiene una matriz de serie temporal
TS_Base <- ts(base, start=c(2003,2),frequency = 12)

TS_Base

plot(TS_Base[,"GP"])

# Realizar el modelo
# Para tasa de crecimiento se emplea tslm
tasa <- tslm(log(GP) ~ trend, data=TS_Base)

summary(tasa)
# La pendiente es 0.017326
# Se la emplea para obtener la tasa de crecimiento relativa compuesta, 
# mediente el empleo de la ecuacion:
#           (exp(b1)-1)*100
# La tasa de crecimiento relativa compuesta
# para el periodo 2003m2 - 2007m10 es de
(exp(0.017326)-1)*100
# En promedio, mes a mes, el gasto publico se incremento mensualmente
# del 1.747%.