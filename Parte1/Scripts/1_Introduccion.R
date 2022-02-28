# Introduccion R ====

# Installar librerias ====
# Quitar # para instalarlas:
#install.packages("dplyr")
#install.packages("openxlsx")
#install.packages("lubridate")

# librerias ====
library(dplyr)
library(openxlsx)
library(lubridate)


# Listas ====
x <- c(1,3,7)
y <- c(2,7,3)

# Dataframe ====
data <- data.frame(x,y)

# Cargar datos ====
# Data <- read.csv()
# Data <- read.xlsx()

# Devuelve la ruta del archivo a abrir
#file.choose()

# Se leen los datos de Excel.
data1 <-read.xlsx("Data/DEP_2016_BP.xlsx",
                  detectDates = T) 

# Manipulacion de Matrices ====
# Nombrematrix[fila,columna]

## Obtener elementos de la matriz ====
# Elemento i,j
data[2,1]
data[5,2]

## Todas las filas de una columna ====
data[,2]
data[,1]

## Todas las columnas de una fila ====
data[1,]
data[2,]
## Rangos ====
data[1:3,1:2]
data[1:5,2]
data[1:4,1]

data[,2]

# Para saber los nombres de las variables en mi base de datos
# Se emplea la funcion names(Tabla)
names(data)
names(data1)

nueva.base <- data.frame(data1[,5])
# Se le crea un nombre por default

names(nueva.base)

# Para cambiar el nombre de una columna
names(nueva.base)[1] <- "REGION"

# Para realizar una tabla de fecuencia absoluta de cierta variable de interes
# NombreMatriz$Variable
table(data1$REGION)
table(data1$ENTIDAD)

# Para tener una idea de lo que contiene la base tomando los primeros valores
# head(nombreBD) --- Por defecto devuelven los 6 primeros elementos
# Se cambia la cantidad de datos a devolver con n = 
head(data1,n=2L)

# Filtrar ====
# Filtrar por region
nueva.base <- data1[data1$REGION=="COSTA",]
table(nueva.base$REGION)
table(data1$REGION)

# Filtrar por numero de clientes
nueva.base <- data1[data1$NUMERO.DE.CLIENTES<=5,]
min(nueva.base$NUMERO.DE.CLIENTES)
max(nueva.base$NUMERO.DE.CLIENTES)

# Usando operadores logicos
#Ejemplo1 - And
nueva.base <- data1[data1$NUMERO.DE.CLIENTES>=5 &
                      data1$NUMERO.DE.CLIENTES<=10,]

min(nueva.base$NUMERO.DE.CLIENTES)
max(nueva.base$NUMERO.DE.CLIENTES)

# Ejemplo2 - OR
nueva.base <- data1[data1$REGION=="COSTA"|
                      data1$REGION=="SIERRA",]
table(nueva.base$REGION)


# Elementos distintos de algo
nueva.base <- data1[data1$REGION!="COSTA",]
table(nueva.base$REGION)

# Combinando
nueva.base <- data1[data1$REGION=="COSTA" & 
                      data1$NUMERO.DE.CLIENTES>=5,]
table(nueva.base$REGION)
min(nueva.base$NUMERO.DE.CLIENTES)
max(nueva.base$NUMERO.DE.CLIENTES)
