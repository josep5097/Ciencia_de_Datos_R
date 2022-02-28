# Empleando Filter() ====

# Librerias ====
library(dplyr)

# Data ====
data <- read.csv("Data/Iowa_Liquor_Sales.csv",
                 stringsAsFactors=F,
                 header=T)

# Manipulando data ====
# Reconocer los nombres de los encabezados
names(data)
# Filtros en donde sola la ciudad Davenport aparezca
nueva.base <- filter(data,
                     City=="DAVENPORT")
# Nos muestra el conteo r?pido de las coincidencias
table(nueva.base$City)
table(data$City)

# Operadores logicos
# & es el operador logico Y (and)
# | es el operador logico o (or)

nueva.base <- filter(data,
                     City=="DAVENPORT" |City=="CEDAR RAPIDS")

table(nueva.base$City)
names(data)

# Condicion combinada
nueva.base <- filter(data,
                     City=="DAVENPORT" &
                       Bottles.Sold >= 6)
# Funcion max y min
min(nueva.base$Bottles.Sold)
max(nueva.base$Bottles.Sold)

# Filtrado por libreria o funciones internas de R
nueva.base <- filter(data,
                     City=="DAVENPORT" &
                       Bottles.Sold >= 6)

zz <- data[data$City=="DAVENPORT" &
             data$Bottles.Sold >= 6,]

# Ejemplo 1
nueva.data <- filter(data,
                     City=="Davenport",
                     Bottles.Sold >= 6,
                     Volume.Sold..Gallons.>=3)
names(data)

# Ejemplo 1
nueva.base <- filter(data,
                     City=="Davenport" | City=="CEDAR RAPIDS",
                     Bottles.Sold >= 6 ,Bottles.Sold <= 15)

min(nueva.base$Bottles.Sold)
max(nueva.base$Bottles.Sold)

# Operador distinto de !=
nueva.base <- filter(data,
                     City!="DAVENPORT")
table(nueva.base$City)

nueva.base <- filter(data,
                     City!="DAVENPORT",
                     Bottles.Sold!=6)

# fragmentando data con slice
nueva.base <- slice(data, 350:420)