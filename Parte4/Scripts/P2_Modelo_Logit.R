# Modelo Logit
# Ejercicio del libro de A. Colin Cameron and Pravin K.

# Librerias ====
library(openxlsx)
library(foreign)
library(forecast)
library(gmodels)
library(lmtest)
library(ResourceSelection)
library(ROCR)

# Base de Datos ====

DB_1 <- read.dta(file = "Data/randdata.dta")
attach(DB_1)
names(DB_1)

DB_2 <- DB_1[year==2,]
attach(DB_2)

DB_2 <- DB_2[complete.cases(educdec),]

summary(DB_2$educdec)

# Estimar el modelo Logit

# Estimar el gasto médico
# Si el gasto>0, entonces 1, caso contrario 0

med = glm(
          # Gasto en salud frente al número de desesos
          binexp ~ disea,
          # Modelo Logit
          family = binomial(logit),
          data = DB_2)

summary(med)

# Ratio de los coeficientes del modelo
exp(coef(med))
# Explicación:
# Es 1.6 veces más probable que una persona gaste en salud
# siempre que el número de desesos crezca

# Otros regresores ====
DB_3 <- DB_2[c("binexp",'female', 'xage','linc')]
med2 <- glm(binexp ~ .,
            family = binomial(logit),
            data = DB_3)
summary(med2)
# Las variables explican significativamente el modelo
exp(coef(med2))
# Es 1.57 más probable que gaste en salud siendo mujer,
# Es 1.01 más probable con el aumento de edad y
# es 1.22 veces más probable que gaste en salud cuando el 
# nivel de ingresos aumenta.

plogit <- predict(med2,
                  type = "response")
plogit
plot(plogit)
plot(plogit, type = "l")
abline(h=0)
abline(h=1)
