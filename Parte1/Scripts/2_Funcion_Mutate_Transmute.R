# Mutate y Transmute ====

# Librerias ====
library(dplyr)

# Data ====
data <- read.csv("Data/Iowa_Liquor_Sales.csv",
                 stringsAsFactors=F,
                 header=T)

# Para determinar el tipo de variable que se tiene en la base de datos
str(data)

# Substr es equivalente a la funcion extrae
# Substr(data, indice_Init, long)
# as.numeric para cambiar al formato numerico


# Mutate ====
# mutate(data_origne, nombre_nueva_Var = valor, nombre_nueva_Var = valor,...)
# Mutate se trabaja sobre la misma base de datos
data <- mutate(data,
               costo.total=
                 as.numeric(substr(
                   data$State.Bottle.Cost,2,15))*
                 Bottles.Sold,
               ingreso.total=as.numeric(
                 substr(data$State.Bottle.Retail,2,15))*
                 Bottles.Sold,
               utilidad=ingreso.total-costo.total)

# Transmute ====
# Transmute, se copia la informacion sobre una nueva base de datos
data2 <- transmute(data,
               costo.total=
                 as.numeric(substr(
                   data$State.Bottle.Cost,2,15))*
                 Bottles.Sold,
               ingreso.total=as.numeric(
                 substr(data$State.Bottle.Retail,2,15))*
                 Bottles.Sold,
               utilidad=ingreso.total-costo.total)


# Uso de la funcion If_else() ====
# if_else()
# Es equivalente a la funcion si(condicion,verdadero,falso) de Excel
data <- mutate(data,
               resultado=if_else(utilidad>0,
                                 "utilidad",
                                 "perdida"))
table(data$resultado)

data3 <- transmute(data,
               resultado=if_else(utilidad>0,
                                 "utilidad",
                                 "perdida"))
table(data3$resultado)

names(data)