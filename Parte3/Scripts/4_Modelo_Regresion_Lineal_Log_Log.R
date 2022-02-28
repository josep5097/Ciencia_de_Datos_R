# Modelos de doble logaritmos ====
# Se los conoce como modelos de c?lculo de elasticidades.

#          Modelos log- log

# Antes se tenia:
# y= bo+b1x1

# Al trabajar con logaritmos:
# log(y)=bo+b1*log(x1)

# La interpretaci?n:
# El incremento en 1% de X dar? como resultado un 
# incremento de x% en y.


# Modelo ====

# Libreria ====
library(forecast)

# Data ====

#COBB.. funcion de produccion COBB-DOUGLAS

# Se busca linealizar ciertos par?metros que se encuentran elevados

#              Y=B0.L^B1.K^B2 
#    ESTE MODELO NO ES LINEAL EN LOS PARAMETROS

# Para poder estimarlo mediante MCO, debe de ser lineal
# Se emplean los logaritmos para poder linealizarlo

#       LOG(Y)= BO+B1 LOG(L)+ B2 LOG(K)

base <- read.csv("Data/COBB.csv")

attach(base)
names(base)

# Se tiene: PIBReal, L=Trabajo y K=Capital

# Se establece el modelo
modelo <- lm(log(PIBREAL) ~ log(L)+log(K), data=base)

summary(modelo)
## Interpretacion ====
# El incremento del 1% en el Trabajo dar? como resultado un 
# incremento en 1.4987% en el PIB Real, y el incremento de 1%
# en el factor Capital dar? como resultado un incremento del 0.4899%
# en el PIB Real.

# Se tiene que si:
# si b1+b2 >1 = rendimientos crecientes
# si b1+b2 <1 = rendimientos decrecientes
# si b1+b2 =1 = rendimientos constantes

# A partir de estas teorias microeconomicas se establecen rendimientos
# crecientes

# Prueba de significancia individual ====

# ho: b1=0
# h1: b1=!0

# si la t es mayor que 2 en valor absoluto, rechazo HO.
# si p value es menor que 0.05, rechazo HO.

# Se rechazan las hipotesis nulas!

# t value = b1-B1/ee b1
# t value = b1/ee b1
# t value = 1.4987/0.5397
1.4987/0.5397