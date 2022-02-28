# Join de datos ====

# Librerias ====
library(dplyr)
library(openxlsx)

# Variable de union no puede estar repetida.
data1 <- read.csv("Data/Iowa_Liquor_Sales_A.csv", 
                  stringsAsFactors = F)

data2 <- read.csv("Data/Iowa_Liquor_Sales_B.csv", 
                  stringsAsFactors = F)

names(data1)

# Union funciones internas
data3 <- left_join(data1,data2,
                   by="Invoice.Item.Number")
data3 <- right_join(data1,data2,
                   by="Invoice.Item.Number")
data3 <- inner_join(data1,data2,
                   by="Invoice.Item.Number")
data3 <- full_join(data1,data2,
                   by="Invoice.Item.Number")

# Combinando columnas ====
data1 <- read.xlsx("Data/DEP_2016_BP_A.xlsx", 
                   detectDates = TRUE)
data2 <- read.xlsx("Data/DEP_2016_BP_B.xlsx", 
                   detectDates = TRUE)

# bind_cols
z <- bind_cols(data1,data2)


# Combinando filas ====
# Se combinan las filas de los historicos
data1 <- read.xlsx("Data/DEP_2016_BP_C.xlsx", 
                   detectDates = TRUE)
data2 <- read.xlsx("Data/DEP_2016_BP_D.xlsx", 
                   detectDates = TRUE)

# Deben de ser las mismas variables tanto en 1 como es 2
names(data1)
names(data2)
# Deben de tener el mismo formato, como oficinas no posee el mismo tipo de dato
# se debe de modificarla en primera instancia.

data1$OFICINAS <- as.numeric(data1$OFICINAS) 
#data1 <- select(data1,-OFICINA)
data.nueva <- bind_rows(data1,data2)