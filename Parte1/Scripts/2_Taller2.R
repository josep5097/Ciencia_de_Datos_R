# Taller 2 ====

# Librerias ====
library(dplyr)
library(openxlsx)
library(lubridate)

# Data ====
Data_Bancos <- read.xlsx("Data/DEP_2016_BP.xlsx",
                        detectDates = T)

# Revisar los tipos de datos
str(Data_Bancos)

# Revisar los elementos
names(Data_Bancos)

# Obtener el mes del dato
month(Data_Bancos$FECHA)

Resumen_Bancos <-Data_Bancos %>%  # Data
  mutate(Month_DB=month(Data_Bancos$FECHA))%>%   # Extrae el mes de la columna date
  filter(Month_DB>=1 & Month_DB<=7)%>%
  group_by(ENTIDAD) %>%
  summarise_each(funs(
    Promedio=mean(.),
    Maximo=max(.),
    Minimo=min(.)),NUMERO.DE.CLIENTES)%>%
  arrange(desc(Minimo))
View(Resumen_Bancos)