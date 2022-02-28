# Modelos de Regresion multiple ====
# Modelos ANCOVA

# Librerias ==== 
library(lattice)
library(openxlsx)

# Data ====
Ancova_DB <- read.xlsx("Data/salarios.xlsx")
# w - Salarios
# Educ - nivel de educaci?n - Cuantitativa

# Estimando el modelo =====
modelo.Ancova_DB <-lm(w ~ female+educ, data = Ancova_DB)
summary(modelo.Ancova_DB)

# Analisis:
# El incremento en el nivel de educacion, dara como incremento 0.506 unidades
# de dolares en el salario, por otro lado, el genero femenino ganan 2.27 dolares en 
# promedio que los varones, en donde el promedio de los varones ganan 0.6228

# graficando la regresion 
xyplot(w ~ educ | female, data = Ancova_DB,
       panel = function(x, y, ...)
       {
         panel.xyplot(x, y, ...)
         panel.lmline(x, y, ...)
       }
)

# Estimando el modelo ancova multiplicativo ====

# Se puede agregar una variable multiplicativa que haga muchas veces de
# variable de control.
# Controlar los annos de educ por el genero -->  educ:female

# y = a1+a2+b1+b2

# alfa 1 -> Beta0
# alfa 2 (intercepto diferencial, en este caso female) -> las dos regresiones tienen el mismo intercepto?
# b1  --> educ
# b2(coeficiente de pendiente diferencial-alterador -> multiplicativo) -> las dos regresiones tienen la misma pendiente ?
# permite diferenciar entre los coefi cientes de las pendientes de las dos categorias,
# intercepto diferencial permite distinguir entre los interceptos de las dos categorias

modelo.Ancova_DB <-lm(w ~ female+educ+ educ:female, data = Ancova_DB)
summary(modelo.Ancova_DB)

# Analisis de las regresiones concurrentes ====

# Si el valor p, de la prueba con respecto al intercepto diferencia es significativo se
# rechaza de que ambas ecuaciones tienen el mismo intercepto.
# Al parecer ambas regresiones tienen el mismo intercepto dado que no se 
# puede rechazar.
# Con respecto a las variables del coef alterador, se analizan las pendientes
# Si es significativo, se rechazan.
# Como no es significativo ambas tienen las mismas pendientes.

# En el caso de ser diferentes parametros:

# Ecuacion para masculino

#     y=0.20+0.53*educ

# Ecuacion para femenino

#     y=0.20-1.19 + (0.53-0.086)*educ

#     y= -0.99*female +0.44*educ