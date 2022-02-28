# Modelo de Regresion Multiple ====

# se realiza el modelo para obtener Y en funcion de 
# varias variables X.


# Librerias ====
library(forecast)

# Data ==== 
base <- read.csv("Data/REG_1.csv")

attach(base)


# Construir modelo =====

#             y=b0+b1x1+b2x2.

# Creacion del modelo:
# Se toma a la oferta monetaria en funcion del gasto publico y 
# la tasa de interes

modelo <- lm(M1 ~GP+TI,data=base)

summary(modelo)

## Interpretar los resultados ====

# Pasos:
# 1) Evaluar los signos esperados.
# La tasa de Interes es inversamente proporcional a la oferta monetaria --- Signo 
# El incremento del gasto publico conlleva un incremento de la oferta monetaria - Signo +

# 2) Interpretacion de los coeficientes
# la interpretacion al ser lineal, es una unidad a una unidad.
# El incremento en una unidad monetaria del gasto publico dara como resultado
# un incremento de 1.6697 unidades monetarias en la oferta monetaria.
# El incremento en 1% de la TI dara como resultado una disminucion de
# 1283.62 en la oferta monetaria.

# 3) Prueba de significancia individual

#b1=0
#b1=!0

# Reglas:
#* Regla 2 t: >2 en valor absoluto
#* El valor p <0.05
#* Las estrellas
# En ambos casos se rechaza la hipotesis nula


# Prueba de significancia conjunta
# Prueba F  - Un F estadistico
# Si p <0.05 rechazo H0.

#Ho: b0=b1=b2=0 - Todos los coef = 0
#H1: !=0 - Todos los coef dif de 0.


# Coef. determinacion. --- R cuadrado
# 0<=r^2<=1
# Mas cercano a 1, mejor ajuste.

# 81% de mi modelo esta explicado por las variables GP, TI,
# el resto se encuentra explicado por otras variables que no 
# son consideradas en la ecuacion, se concentran en los "residuos"


#El R^2 es una funcion lineal creciente.


#### Forecast o pronostico de la oferta monetaria ####

# Ecuacion del modelo establecida
# y=b0+b1x1+b2x2


# Opciones para establecer los pronosticos en las variables independientes:

#1) Opinion del experto (subjetivo, no es tecnico)
#2) Modelo ARIMA para proyectar GP, TI.
#   Con estas proyecciones se las incluyen en el modelo.
#3) Cuando  se conoce de antemano los valores futuros de las variables independtientes

# Se realiza la proyeccion del tiempo T+1
#yt+1=b0+b1x1t+1+b2x2t+1
# Se realiza la proyeccion del tiempo T+2
#yt+2=b0+b1x1t+2+b2x2t+2

names(base)
# Se incluyen: T+1 y T+2
# Proyeccion con dos valores fuera de la muestra
nueva <- data.frame(GP=c(2700,2900),
                    TI=c(4.2,4.7))


forecast(modelo,newdata = nueva)

# Cuando la TI baja, la oferta monetaria incrementa, pero con el caso 
# de que la TI incremente, la oferta monetaria disminuye.