# Modelo de Regresión Lineal ====

# Librerias ====
library(forecast)
library(foreign)

# Data ====
base <- read.csv("Data/REG_1.csv")

# Construir un modelo de regresion simple para que explique la Oferta Monetaria,
# En este caso M1

# Usar recursivamente los datos de la base
attach(base)

tsbase <- ts(base,start = c(2003,1),frequency = 12)

# Graficando las series
plot(M1,type="l")
ts.plot(M1)
ts.plot(tsbase[,2])

ts.plot(tsbase[,3])

plot(GP,M1)

# Creando el Modelo de Regresion Simple
# Modelo de M1 (Oferta Monetaria) Vs GP (Gasto publico)
# M1 = B0 + B1*GP+ui
# Y = B0 + B1*X1+ui
modelo1 <- lm(M1 ~ GP,data=base)
summary(modelo1)

## Interpretacion ====

# B0 = Termino de intercepto, constante, basico.
# B1 = Termino de la pendiente

# B0 = 4264 millones de dolares es la oferta monetaria basica
# sin intervencion de ninguna variable

# B1 = incremento en una unidad monetaria de GP dara, como 
# resultado un incremento de 2.32 unidades monetarias en la 
# Oferta Monetaria m1


## Evaluacion ====
# Prueba de significancia individual
# Prueba de hipotesis individual de los parametros
# Prueba T

# ee : Error estandar
# tp = (B0^-B0)/ee(B0^)
# Como no se conoce el poblacional B0 = 0
# tp = B0^/ee(B0^)
# Generalizado:
#tp= bk&/ee(bk^)


#Ho: b0=0
#h1: b0=!0

# Regla 2 t: 
# Rechazo hipotesis nula si el valor de t es mayor que 2, 
# cuando n es mayor a 30 (regla 2t)

# Regla:
# Rechazo HO si el valor de probabilidad es menor que 0.05

# Se rechaza cuando no ayuda a explicar el fenomeno


plot(GP,M1,
     main="Regresion entre Oferta Monetaria y Gasto Publico",
     ylab = "Oferta Monetaria",
     xlab = "Gasto Publico",
     col="blue")
abline(modelo1,col="red")
