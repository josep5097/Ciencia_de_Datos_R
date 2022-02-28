# Pruebas T para una muestra ====

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
    anio = year(Date),
    mes = month(Date)
  )

Ventas_Data <- Ventas_Data %>%
  mutate(
    Costos = Bottles.Sold*State.Bottle.Cost,
    Utilidad = Sale..Dollars.- Costos,
    Margen = Utilidad/Sale..Dollars.
  )

Resumen1 <- Ventas_Data %>%
  group_by(anio, mes, City)%>%
  summarise(
    Suma = sum(Sale..Dollars.)) %>%
  arrange(City,anio, mes) %>%
  filter(anio==2016)

View(Resumen1)

Davenport_Inf <- Resumen1 %>%
            filter(City == "DAVENPORT")

summary(Davenport_Inf)
mean(Davenport_Inf$Suma)

# Si yo no conozco la media -> Prueba p test
# H0: u=62000
# H1: u=!62000
# Twoside -> data dos colas
p1 <- t.test(na.omit(Davenport_Inf$Suma), 
                     alternative = "two.side", 
                     mu = 62000)
p1
# Regla de decisi?n
#   Si p<0.05 se rechaza H0.
# Con p>0.05 -> en promedio el valor contrastado es 62000 segun prueba t


# Prueba 2
p2 <- t.test(na.omit(Davenport_Inf$Suma),
                     alternative = "greater",
                     mu = 68000)
p2
# H0: u=68000
# H1: u>=68000

# Muestras Independientes ====

# Diferencia de medias muestreadas, conociendo que no son pareadas
# Primero obtener las varianzas de las muestras
CedarRapids_Info <- Resumen1 %>%
  filter(City == "CEDAR RAPIDS")
var.test(x=CedarRapids_Info$Suma,
         y=Davenport_Inf$Suma)
# H0: Var iguales
# H1: Var diferentes

# Con p>0.05 -> Var iguales

# Realizar prueba de hip de dif de medias
t.test(x=CedarRapids_Info$Suma,
       y=Davenport_Inf$Suma,
       var.equal = T,
       paired = F)
# H0: ua=ub
# h1: ua=!ub

# Con p>0.05 no se rechaza la H0, la medias de las ventas son similares en ambas ciudades
