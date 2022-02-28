# Dispersion y Correlacion

# Librerias ====
library(gmodels)
library(dplyr)
library(openxlsx)
library(corrplot)

# Data ====
Base_Data <- read.csv("Data/REG_1.csv")
Base_Data <- Base_Data[2:6]

# Gastos publicos frente a la Oferta monetaria
plot(GP~M1, data = Base_Data)

plot(Base_Data)

# Grafico de Pares
pairs(~TI+M1+GP+IPC,data = Base_Data)

matrix <- cor(Base_Data)
corrplot(matrix,method = "pie") #pie, number, 
