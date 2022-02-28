# Histogramas y Tablas de Frecuencias ====

# Librerias ====
library(openxlsx)
library(fdth)
library(dplyr)
library(lubridate)

# Data ====
Ventas_Data <- read.csv("Data/Iowa_Liquor_Sales.csv",
                        stringsAsFactors = F)


Ventas_Data <- Ventas_Data %>%
  mutate(
    Sale..Dollars.=(
      as.numeric(substr(Sale..Dollars.,2,15))),
    State.Bottle.Cost = (
      as.numeric(substr(State.Bottle.Cost,2,15))),
    City = sapply(City, toupper),
    Date = as.Date(Date,
                   format = "%m/%d/%Y"),
    data_year = year(Date),
    mes = month(Date)
  )

Ventas_Data <- Ventas_Data %>%
  mutate(
    Costos = Bottles.Sold*State.Bottle.Cost,
    Utilidad = Sale..Dollars.- Costos,
    Margen = Utilidad/Sale..Dollars.
  )

Resumen1 <- Ventas_Data %>%
  group_by(data_year, mes)%>%
  summarise_each(funs(
    Media = mean(.,na.rm = TRUE),
    Minimo = min(.,na.rm = TRUE),
    Maximo = max(.,na.rm = TRUE),
    Total_Datos = n()), Sale..Dollars.) %>%
  arrange(data_year, mes, Media)
View(Resumen1)

# Uso de la funcion hist() con la data completa
hist(Ventas_Data$Costos,breaks = "Sturges")

# Realizar la tabla de frecuencia
Freq_Table <- fdt(Resumen1$Media,
                  breaks = "Sturges")

Freq_Table

# Histograma de la tabla de frecuencia
hist(Resumen1$Media, breaks = "Sturges")

# Tomando los intervalos de la tabla de Frecuencia
hist(Resumen1$Media,
     seq(from = Freq_Table$breaks[1],
         to = Freq_Table$breaks[2],
         by = Freq_Table$breaks[3]),
     main = "Histograma de las Ventas Medias",
     xlab = "Ventas Medias",
     ylab = "Frecuencias",
     col = c("Blue"))
mtext("Ventas de Licor")
