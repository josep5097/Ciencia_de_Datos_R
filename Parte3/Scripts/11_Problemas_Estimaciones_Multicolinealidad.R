# Modelos de Regresion Lineal Multiple
# Multicolinealidad

# Librerias ====
library(forecast)
library(lmtest) #para contraste durbin watson.
library(nlme)
library(sandwich)
library(car)
library(ggplot2)
library(dynlm)
library(GGally)

# Data ==== 
reg <- read.csv("Data/REG_1.csv")
attach(reg)

# Estimando el modelo ====
modelo <- lm(M1 ~ GP + TI + IPC, data=reg)

# Identificacion del problema ==== 

## Graficando los residuos ====

### Generando la matriz de correlacion =====
cor(reg [3:6],use="complete.obs")
# Cada elemento de la matriz de corr es un elemento Rij
# De los elementos de la matriz corr se escoje el que posea el mayor valor
# de correlacion
# GP vs IPC: 0.794
# Si el coef es mayor 0.91 se puede concluir categoricamente que existe un problema
# de multicolinealidad severa

# Realizar un grafico de corr materializado
pairs(reg[3:6])

## GGallery, grafico de correlacion
ggpairs(reg[3:6])

# Factor de inflacion de la varianza
vif(modelo)
# Si se tiene un factor VIF>10 se tiene un valor severo de multicolinealidad
# El modelo no tiene un caso severo

# Suponiendo que se tiene un caso de multicolinealidad 
# Atenuando el problema 

# 1) Si las variables son las correctas, se sugiere no hacer nada
# 2) Ampliar el tamano muestral
# 3) Aplicar log
# 4) Aplicar diferencias
# 5) An?lisis de Componentes principales
# 6) Es preferible emplear ratios

# Atenuando a traves de la primera diferencia
# Primeras diferencias
# Matriz caracterizada como una serie de tiempo
tsreg <- ts(reg[,2:6],
            start = c(2003,1),
            frequency = 12)

# Generando dataframe con una sola diferencia
# Se calcula la primera diferencia de la matriz y la transforma
# a un dataframe
difreg <- data.frame(diff(tsreg,1))
attach(difreg)

# Generando el modelo
modelo2 <- lm(M1 ~ GP + TI + IPC, 
              data=difreg)

summary(modelo2)
# Se observa que por corregir se empeora el modelo

vif(modelo2)
# El VIF es menor data que la primera diferencia lo atenua

# A traves del analisis de componentes principales
# Tecnica de la estadistica multivariante
# Con pocos factores se retenie gran parte de la variabilidad
# Se lo emplea como ultimo caso para aplicar

# Se selecciona las variables independientes
x <- data.frame(reg$GP,reg$TI,reg$INF)
summary(x)
cor(x)

# Estimar los componentes principales de las tres variables
# Se emplean los nuevos comp principales para estimar
macp1 <- princomp(x, 
                  # Guardar los scores
                  scores=TRUE, 
                  # Emplear la correlacion
                  cor=TRUE)
summary(macp1)
# Con k vars se tienen k componentes

# Obtener los scores
# Scores of the components
macp1$scores[1:10,]
score <- data.frame(macp1$scores)
score
attach(score)

# OBTENIDOS LOS SCORE, SE LOS PUEDE USAR
# PARA OTRAS CUESTIONES, POR EJEMPLO
# PARA HACER UNA REGRESION LIBRE DE COLINEALIDAD
# O PARA INCORPORARLAS EN ANALISIS CLUSTER 
# O PARA REALIZAR MODELOS PARA PRON?STICOS

reg2<-lm(M1 ~ score$Comp.1 + 
           score$Comp.2+ 
           score$Comp.3, data = reg)
summary(reg2)
vif(reg2)

# Con VIF = 1, no existe multicolinealidad