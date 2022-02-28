# Graficando la informacion ====

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
  group_by(anio, mes)%>%
  summarise(
    Suma = sum(Sale..Dollars.)) %>%
  arrange(anio, mes)
View(Resumen1)

# Graficando ====

# Puntos
plot(Resumen1$Suma)
# Grafico de lineas
plot(Resumen1$Suma,type = "l", main = "Evolutivo de Ventas",
     xlab = "Meses",
     ylab = "Nivel de Ventas",
     col = "blue"
     )

Ts_Data = ts(Resumen1[,3],
             start = c(2015,1), 
             frequency = 12)
ts.plot(Ts_Data)

# Grafico de barras
barplot(Resumen1$Suma)

x <- seq(as.Date("2015/1/1"),as.Date("2017/2/1"),
         format="%b%y",
         by="month")
fecha <- format(x,"%Y-/%m")
bp <- barplot(Resumen1$Suma, names.arg = fecha,
              las = 2,
              ylim = c(0,450000),
              main = "Grafico de barras",
              col = "lightblue")

y <- round(Resumen1$Suma,0)
text(bp,y,y,srt=90,cex=1,pos=4,col="black")
mtext(side = 3, line = 1, adj = 0,
      "Fuente:BCE")