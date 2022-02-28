# Agrupando (group_by) Y resumiendo informacion (summarise) =====

# Librerias ====
library(dplyr)
library(openxlsx)

# Data ====
data <- read.csv("Data/Iowa_Liquor_Sales.csv",
                 stringsAsFactors=F,
                 header=T)

# Obtener la media de las ventas en dolares
mean(data$Sale..Dollars.)
# Al no tener el argumento correcto, se visualiza el tipo de dato
class(data$Sale..Dollars.)
# Se modifica los valores de la columna Sale..Dollars, mediante mutate
data <- mutate(data,
               Sale..Dollars.=(
                 as.numeric(
                   substr(data$Sale..Dollars.,2,15))))

# Agrupandola la variable de interes ====
# data.g1 <- group_by(data,City)
# Si queremos agrupar los datos nos saldr?n valores diferentes para las ciudades
# debido a la forma de escritura.

# Convirtiendo minusculas en mayusculas para obtener ciudades correctas
# Mediante sapply(Var, toupper)
data <- mutate(data,
       City=sapply(data$City,toupper))

# Agrupandola la variable de interes
# La visualizacion es la misma.
# Se aplica el groupby como un paso previo para aplicar summarise
data.g1 <- group_by(data,City)

# Resumiendo la informacion ====
# summarise(data, var1, var2, var3)
summarise(data.g1,media=mean(Sale..Dollars.,na.rm=T) )

summarise(data.g1,
          media=mean(Sale..Dollars.,na.rm=T),
          maximo=max(Sale..Dollars.,na.rm=T),
          minimo=min(Sale..Dollars.,na.rm=T),
          contar=n(),
          perdidos=sum(is.na(Sale..Dollars.)),
          mediana=quantile(Sale..Dollars.,prob=0.5),
          contar.menos6=sum(Bottles.Sold<=6),
          contar.mas6=sum(Bottles.Sold>6))

# Agruparlo en un data frame
resumen <- data.frame(summarise(data.g1,
          media=mean(Sale..Dollars.,na.rm=T),
          maximo=max(Sale..Dollars.,na.rm=T),
          minimo=min(Sale..Dollars.,na.rm=T),
          contar=n(),
          perdidos=sum(is.na(Sale..Dollars.)),
          mediana=quantile(Sale..Dollars.,prob=0.5),
          contar.menos6=sum(Bottles.Sold<=6),
          contar.mas6=sum(Bottles.Sold>6)))


# Resumiendo la informacion de una variable
# sin estar escribendo el nombre de la misma, en cada linea.
# funciones --> (funs(funcion1, funcion2, ...), Var)
resumen2 <- data.frame(summarise_each(data.g1,
               funs(
                 media=mean(.,na.rm = T),
                 contar=n(),
                 max=max(.,na.rm=T),
                 min=min(.,na.rm=T)),
               Sale..Dollars.))