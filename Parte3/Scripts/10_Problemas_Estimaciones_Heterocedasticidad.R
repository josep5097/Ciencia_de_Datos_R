# Modelo de Regresion Lineal Multiple ====
# Heterocedasticidad

# Librerias ====

library(lmtest) 
library(nlme)
library(sandwich)
library(car)
library(ggplot2)
library(openxlsx)

# Data 
Reg_Data <- read.xlsx("Data/demanda.xlsx")
attach(Reg_Data)

# Estimando el modelo 
modelo <- lm(log(Q1) ~ log(YD) + log(P1) + log(P2)+ log(P3), 
             data=Reg_Data)

summary(modelo)
# Todos los betas muestran que son significativos exceptuando P3
# Observemos si es correcto

# Identificacion del problema ====
# Graficando los residuos contra los valores ajustados
qplot(x=modelo$fitted.values,
      y=(modelo$residuals))+
      geom_point()

# Se aprecia un patr?n entre los residuos y el valor ajustado
# Se aprecia una forma en U, abre hacia arriba
# Problema de heterocedasticidad

# Esto hay que repetirlo para todas las variables

qplot(x=log(Reg_Data$P1),
      y=(modelo$residuals))+
  geom_point()

qplot(x=log(Reg_Data$YD),
      y=(modelo$residuals))+
  geom_point()
# Sin embargo, es susceptible, por lo cual se debe de comprobar

# Probando Heterocedasticidad
# Heterocedasticidad ====

# Contraste Breusch-Pagan
bptest(modelo)

#si el valor p es sig, rechazo hipotesis nula
#HO: Homo
#H1: Hetero

# Como p es mayor, no se rechaza la nula. Parece que no existe problema 
# de heterocedasticidad

# Comprobar mediante otro test
# Non-Constant Error Variance
ncvTest(modelo)

# Como el valor p no es significativo, no se puede rechazar la h0,
# no presenta problema de heterocedasticidad

# Tercer contraste
#Glesjer
# Este contraste toma el valor absoluto de los residuos frente al log
# de la variable que se considera que presenta este problema
modelof <- lm(abs(modelo$residuals) ~ log(YD),
              data=Reg_Data)

summary(modelof)
# Este modelo indica que al emplear Log(YD), influye significativamente
# a los residuos
# Este modelo indica que si puede existir

# Goldfeld-Quandt
# Asumiendo que la data est? ordenada
gqtest(modelo, order.by = ~ log(YD), data = Reg_Data)
# Este contraste indica de igual forma que existe 

# Suponiendo que si existe

# Atenuando el problema 
# Mediante Minimos Cuadrados Ponderados (caso:cuando no se conoce la varianza)

modelofitted <- lm(log(Q1) ~ log(YD) + log(P1) + log(P2)+ log(P3), 
               data=Reg_Data,
               weights = 1/log(YD))

summary(modelofitted)

# Los betas tienen de igual forma un valor significativo

# Errores estandar libre de heterocedasticidad
# Recuperar el ee de las matrices var y cov libres de heterocedasticidad
# Se calculan los verdaderos valores del ee
modelohac <- coeftest(modelo, vcov = vcovHC(modelo))
modelohac
# Al eliminar el problema de la heterocedasticidad, muestra que incluso
# P3 tiene un valor significativo

# Combinando todas las opciones para el estimador HAC

#HCO:Basic Sandwich (Eicker, hUber,White)
#HC1-3:muestras relativamente peque?as
#HC4:muestras relativamente peque?as con presencia 
# de valores at?picos

#por defaul: HC3
# A traves de los 5 tipos de correxi?n de la matriz de Var y cov
# Como varian los ee del modelo
t(sapply(c("HC0", "HC1", "HC2", "HC3", "HC4"),
          function(x) sqrt(diag(vcovHC(modelo, type = x)))))

# Influencia 
# Intervalos
sum1 <- summary(modelo)

# Construir un intervalo de confianza 
ls1 <- sum1$coefficients[2]+1.96*sum1$coefficients[,2][2]
li1 <- sum1$coefficients[2]-1.96*sum1$coefficients[,2][2]

ls2 <- modelohac[2]+1.96*modelohac[,2][2]
li2 <- modelohac[2]-1.96*modelohac[,2][2]
data.frame(ls1,li1,ls2,li2)