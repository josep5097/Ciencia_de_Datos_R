# TALLER 1 ====
# Filtrar solo Bancos Grandes, Region Sierra y 
# que los depesitos sean mayores o iguales a 1000
# Calcular la media, mediana y desviacion estandar.


# Librerias ====
library(dplyr)
library(openxlsx)
library(lubridate)

# Data ====
Data_BP <-read.xlsx("Data/DEP_2016_BP.xlsx",
                  detectDates = T)

# Revisar los datos de la tabla
names(Data_BP)

# Filtrado de la informacion
Data_BP_filtrada <- Data_BP[Data_BP$REGION=="SIERRA" & 
                              Data_BP$TIPO.DE.ENTIDAD =="BANCOS GRANDES" &
                              Data_BP$TIPO.DE.DEPOSITO =="Depósitos de ahorro" &
                              Data_BP$SALDO >= 1000,]

# Con la data filtrada se realiza el analisis de media, mediana y desviacion estandar
# Media
mean(Data_BP_filtrada$SALDO[12])
# Mediana
median(Data_BP_filtrada$SALDO[12])
# Desviacion Estandar
sd(Data_BP_filtrada$SALDO[12])
