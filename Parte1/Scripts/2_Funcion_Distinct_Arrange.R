# FUNCIONES DISTINCT & ARRANGE ====

# Distinct: Para obtener valores unicos
# Arrange: Para ordenar en forma ascendente o descendente

# Libreria ====
library(dplyr)

# Data ====
data <- read.csv("Data/Iowa_Liquor_Sales.csv",
                 stringsAsFactors=F,
                 header=T)

# Para saber cuantas categorias distintas existen en ciudad
distinct(data,
         City)

# Trabajar con variables en un solo formato Mayuscula/Minuscula
sapply(data$City,tolower)
# Para aplicar los cambios dentro de una misma DB
# tolower, toupper

data <- mutate(data,
               City=sapply(data$City,tolower))

distinct(data,City)

# Funcion Arrange() =====
names(data)
# Ordenar por una variable, por default -- Ascendente
data <- arrange(data,
        Sale..Dollars.)
# Para hacerlo descendente
data <- arrange(data,
                desc(Sale..Dollars.))
# Para ordenar por dos variables
data <- arrange(data,
                Sale..Dollars.,
                Bottles.Sold)
# Muestra de los 4 pirmeros datos 
head(data,n=4L)