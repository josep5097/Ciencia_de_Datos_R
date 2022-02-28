# Librerias ====
library(dplyr)
library(lubridate)

# Data ====
Ventas_Data <- read.csv("Data/Iowa_Liquor_Sales.csv",
                        stringsAsFactors = F)

str(Ventas_Data)

Ventas_Data <- Ventas_Data %>%
                mutate(
                  Sale..Dollars.=(
                    as.numeric(substr(Sale..Dollars.,2,15))),
                  State.Bottle.Cost = (
                    as.numeric(substr(State.Bottle.Cost,2,15))),
                  City = sapply(City, toupper),
                  Date = as.Date(Date,
                                 format = "%m/%d/%Y"),
                  anio = year(Date),
                  mes = month(Date)
                )

Ventas_Data <- Ventas_Data %>%
                mutate(
                  Costos = Bottles.Sold*State.Bottle.Cost,
                  Utilidad = Sale..Dollars.- Costos,
                  Margen = Utilidad/Sale..Dollars.
                )

# Pregunta 1 ====
# Promedio de tiendas que mas han vendido en el 2017 
# y otros descriptivos

Resumen1 <- Ventas_Data %>%
            filter(anio==2017) %>%
            group_by(Store.Name)%>%
            summarise_each(funs(
              Media = mean(.,na.rm = TRUE),
              Minimo = min(.,na.rm = TRUE),
              Maximo = max(.,na.rm = TRUE),
              Total_Datos = n()), Sale..Dollars.) %>%
            arrange(desc(Media))
View(Resumen1)

Resumen2 <- Ventas_Data %>%
            filter(anio == 2017, 
                   Store.Name == "Sam's Club 6514 / Waterloo")
View(Resumen2)


# Pregunta 2 ====
# Utilidad por ciudad mayor a 45
# Evaluar si el indicar se cumple agrupando por ciudad y luego
# un comparativo por anio

Resume3 <- Ventas_Data %>%
            group_by(City) %>%
            summarise_each(funs(
              Media = mean(.,na.rm = TRUE),
              Minimo = min(.,na.rm = TRUE),
              Maximo = max(.,na.rm = TRUE)), Utilidad) %>%
            arrange(Media)
View(Resume3)

# No cumple el requisito

Resume3 <- Ventas_Data %>%
  group_by(City, anio) %>%
  summarise_each(funs(
    Media = mean(.,na.rm = TRUE),
    Minimo = min(.,na.rm = TRUE),
    Maximo = max(.,na.rm = TRUE)), Utilidad) %>%
  arrange(City, anio, Media)
View(Resume3)
