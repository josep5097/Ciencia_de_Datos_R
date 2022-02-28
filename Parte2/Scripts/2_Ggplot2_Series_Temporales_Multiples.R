# Graficos de Series Temporales Multiples ====

# Librerias
library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)
library(tidyquant)  
library(xml2)

# Data ====
Datos <- read.xlsx("Data/importacionesperu.xlsx",
                   sheet = "Mensuales",
                   detectDates = T)

str(Datos)

# Modificar los datos como fecha 
Datos$periodo <-  seq(as.Date("2014/08/1"), 
                      as.Date("2019/05/1"), 
                      by = "month")

# Redondear a 2 decimales los valores desde la columna 2-6
Datos[,2:6] <- round(Datos[,2:6],2)

melt1 <- melt(Datos,id.vars = "periodo")

# ggplot(data, estetica, geometrica, complementos)
ggplot(data=melt1,
       aes(x=periodo,y=value))+
       geom_line()+
       facet_grid(.~ variable,scales = "free") 
        # .~ Varaibles -> Todos los valores contra la variable denominada variable

# Divide la ventana en diferentes rectangulos
ggplot(data=melt1,
       aes(x=periodo,y=value))+
       geom_line()+
       facet_wrap(.~ variable,scales = "free")

# Podemos cambiar la orientacion del grafico a filas
ggplot(data=melt1,
       aes(x=periodo,y=value))+
       geom_line()+
       facet_grid(variable ~. ,scales = "free")
      # Variables contra todos los valores

ggplot(data=melt1,
       aes(x=periodo,y=value))+
       geom_line()+
       facet_wrap(variable ~. ,scales = "free")


# Con wrap tambien puede controlar el numero de columnas

ggplot(data=melt1,aes(x=periodo,y=value))+
  geom_line()+facet_wrap(variable ~. ,scales = "free",ncol = 2,)

# Se pueden agregar complementos
# Agregar lineas de tendencia -Geom_smooth, con el m?todo de los 
# monimos cuadrados o modelos lineales lm
ggplot(data=melt1,
       aes(x=periodo,y=value))+
       geom_line()+
       facet_wrap(.~variable  ,scales = "free")+
       geom_smooth(se=FALSE, method="lm", colour="black")  

# Se puede colocar una formula.
# Se puede observar la media movil
# Con modelo de regresion - Constante de una recta de regresion 
# Un modelo formula=y~1
ggplot(data=melt1,
       aes(x=periodo,y=value))+
  geom_line()+
  facet_wrap(.~variable  ,scales = "free")+
  geom_smooth(se=FALSE, method="lm", formula=y~1, colour="black")

# Probemos con una media movil de 3 meses
ggplot(data=melt1,
       aes(x=periodo,y=value))+
       geom_line()+
       facet_wrap(.~variable ,scales = "free")+
       # Promedio
       geom_smooth(aes(color="Promedio"),
                   se=FALSE,
                   method="lm", 
                   formula=y~1,
                   show.legend = TRUE)+
       # Media movil de tamano 12
       geom_ma(aes(color="MA(12)"),
               ma_fun = SMA,
               n = 12, 
               size = 1,
               show.legend = TRUE)+
       # Modificando de forma manual las leyendas
       scale_colour_manual(name = 'Leyendas', 
                           values = c("red" ,
                                      "green"),
                           labels = c('Promedio',
                                 'Media M?vil 12'))+
       # Colocar una tendencia en las series 
       # Red de regresiones
       geom_smooth(method="lm")+
       # Reescalado del eje x
       scale_x_date(date_labels = "%Y %b",breaks="6 months")+
       # Ajustes del eje en la leyenda
       theme(legend.position = "bottom",
             axis.text.x = element_text(angle = 90,
                                        hjust=1,
                                        size=7)
             )

# Sin Manejo de colores 
ggplot(data = melt1,aes(x=periodo,y=value))+
  geom_line()+facet_wrap(variable ~.,scales = "free",ncol=3)+
  geom_smooth(aes(color="Promedio"),se=FALSE,
              method = "lm",formula=y~1,show.legend = TRUE)+
  geom_ma(aes(color="MA(12)"),ma_fun = SMA,
          n=12,size=1,
          show.legend = TRUE)+
  geom_smooth(method = "lm")+
  scale_x_date(date_labels = "%Y %b",breaks = "3 months")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=90,
                                   hjust=1,
                                   size=7))