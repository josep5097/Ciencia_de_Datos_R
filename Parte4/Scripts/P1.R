# Modelos MLP

#Bloque Librerias ====
# Librerías 
library(foreign)
library(forecast)
# Para construir tablas cruzadas
library(gmodels)
# Para contrastes de regresión lineal
library(lmtest)
# Para respuestas cualitativa
library(ResourceSelection)
# Para modelos de respuestas cualitativa
library(ROCR)

#Bloque datos ====
# Base de datos
DB_1 <- read.spss(file="Data/hepatitis.sav",
                  use.value.labels = FALSE,
                  to.data.frame = TRUE)
attach(DB_1)

#Bloque de Información ====
# Se busca obtener la relación de tener Hepatitis
# en función de los gramos de alcohol diarios y
# del consumo de drogas.

# Estimar el modelo de probabilidad ====
# MLP
# PAra MLP se estima con Mímino cuadrado ordinarios - LM
mlp <- lm(hepatitis ~ gramalcodiario+drogas,
          data = DB_1)

summary(mlp)
# Modelos microeconométrico mínimo se requieren 30 datos
# para aplicar la regla 2T
# Si el valor t > 2, se rechaza H0

# H0: B0 = 0
# H1: B0 =! 0

# El que consuma drogas tiene mayor incidencia que el consumir
# alcohol.

plot(mlp$fitted.values, 
     type = "l")
abline(h=0)
abline(h=1)
# Problema de heterocedasticidad
# Prob >1 y <0

# Proyecciones ====
fc1 <- forecast(object = mlp,
                newdata = data.frame(gramalcodiario=c(10),
                                     drogas=c(0)))
fc1
