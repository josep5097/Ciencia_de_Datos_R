# Funcion Select() ====

# Librerias ====
library(dplyr)

# Data ====
Data_IOWA <- read.csv("Data/Iowa_Liquor_Sales.csv",
         stringsAsFactors=F)

# Names es un funcion que devuelve el nombre de las columnas de mi base de datos
names(Data_IOWA)

# Seleccionando variables a partir de su nombre
# select(data, Var1, Var2, Var3, ..)
nueva.base <- select(Data_IOWA,Invoice.Item.Number,
                     Vendor.Name,Category)


# Seleccionando variables a partir del numero de la columna
nueva.base1 <- select(Data_IOWA,c(2:5))

# Seleccionamos las variables segun su nombre, hasta el nombre de la ultima variable que se requiera
nueva.base2 <- select(Data_IOWA,Date:Address)

# Seleccionar variables definiendo el orden en la tabla
nueva.base3 <- select(Data_IOWA,Date, everything())

#seleccionar varias variables definiendo el orden en la tabla
nueva.base4 <- select(Data_IOWA,Date,Store.Name, everything())

## BORRANDO VARIABLES
nueva.base4 <- select(Data_IOWA,-Date)

#Borrando varias variables
nueva.base <- select(Data_IOWA,-Date,-Address, -City)

#Borrando variables seg?n el n?mero de la columna
nueva.base <- select(Data_IOWA,-c(1,3,5))

#Borrando entre columnas las variables
nueva.base <- select(Data_IOWA,-c(3:5))

names(Data_IOWA)
nueva.base <- select(Data_IOWA,-c(Address:Vendor.Name))