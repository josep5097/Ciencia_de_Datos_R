# Librerias ====
library(forecast)
library(lmtest) #para contraste durbin watson.
library(nlme)
library(sandwich)
library(car)
library(ggplot2)
library(dynlm)
library(strucchange)
library(memisc)
library(moments) #jarque bera
library(nortest) #anderson darling

# Data 
reg <- read.csv("Data/REG_1.csv")
attach(reg)

# Estimando el modelo 
modelo <- lm(M1 ~ GP + TI + IPC, 
             data=reg)

# Suponiendo que el modelo se encuentra validado
# Se realiza las siguientes validaciones por los siguientes contrastes

# Contraste de Normalidad 
# Si los datos siguen una formacion normal, entonces los residuos 
# se consideran que siguen una distribucion normal

# Se prueban los siguientes 3 contrastes para normalidad
# El contraste toma como vector los residuos
jarque.test(as.vector(modelo$residuals))
ad.test(modelo$residuals)
shapiro.test(modelo$residuals)
# H0: Normalidad
# H1: No normalidad

# Como el valor p no es significativo, no se rechaza la h0, con lo 
# cual, los residuos siguen una distribuci?n normal
# Se lo observa en los tres test

# Parametros deben de ser estables ====
# El modelo o parametro que deben de ser coef estables, debido que al no 
# serlo no sirven para la toma de decisiones

# Contraste de estabilidad ====
# CUSUM:
# Cumulative sums of standardized residuals. Brown et al. (1975)
ols <- efp(modelo, 
           data = durab, 
           type = "OLS-CUSUM")
# MOSUM
# Medias moviles
olsms  <- efp(modelo, 
              data = durab, 
              type = "OLS-MOSUM")
plot(ols)
# Si el modelo no es estable, la linea negra sobresaldra las lineas
# rojas
# Las bandas rojas son los intervalos de confianza
# En los datos finales de la base de dato, se encuentra un quiebre 
# estructural, con lo cual, el modelo no posee estabilidad
# No se puede emplear este modelo
plot(olsms)
# Posee quiebres estructurales en el modelo

# Contraste para ver la fluctuacion ====
sctest(modelo, 
       type="OLS-CUSUM", 
       data=reg)
#HO: no cambio estructural
#H1: cambio estructural
# como el valor p es significativo, se rechaza categoricamente 
# la H0, por lo que existe cambio estructural

# El modelo debe de ser correctamente especificado ====
## Contraste de linealidad o de espeficacion ====
# Los modelos de RL, hacen ref que los par?metros son lineales
# por lo que se debe de verificar

# Contraste Ramsey Resets ====
resettest(modelo)
# Al no ser lineal, se puede teber un error de no especificacion

# H0: Modelo correctamente especificado
# H1: Modelo no es correctamente especificado

# Como p value es significativo, se rechaza H0.
# Se debe de revisar la estructura del modelo.