# Graficas de barra ====

# Librerias ====
library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)

# Data ====
Datos <- read.xlsx("Data/importacionesperu.xlsx",
                   sheet = "Mensuales",
                   detectDates = T)

str(Datos)

# Agrupando los Datos para poder analizarlos
# Modificacion del periodo
Datos$periodo <-  seq(as.Date("2014/08/1"), 
                      as.Date("2019/05/1"), 
                      by = "month")
# Modificacion de la base de datos
Datos <- Datos %>%
  mutate(Year_data=year(Datos$periodo),
         mes=month(Datos$periodo)) %>%
  select(Year_data,mes, everything())  

# Generar datos agrupados
Datosagrupados1 <- Datos %>%
  filter(mes<=tail(Datos$mes,n=1))%>%
  select(-periodo,-mes,-Total)%>%
  mutate(Year_data=as.numeric(Year_data))%>%
  group_by(Year_data)%>%
  summarise_each(list(sum))

# Redondear elementos
Datosagrupados1 <- round(Datosagrupados1,2)

# Para barras stacked
# (x, y, fill)
# si hay varias variables, es probable que se deba 
# usar la funcion melt
# de la libreria reshape2
melt1 <- melt(Datosagrupados1,id.vars = "Year_data")


ggplot(melt1, 
       aes(x = Year_data, y = value, fill = variable)) + 
  geom_bar(stat = "identity")+
  geom_text(aes(y=value, label=value),
            position = position_stack(vjust = 0.5),
            size=4)+
  labs(title="Evolucion de las importaciones del Peru",
       subtitle = "En millones de pesos, acumulados de Enero a (fecha de corte)",
       caption = "Fuente: BCP\n Elaboracion:Jose Miguel Pereira",
       x="Periodo acumulado",y="Millones de soles")
last_plot()
ggsave("importacionesstack.png", width = 8, height = 8)  


# Basta colocar un coor_flip para voltear las barras

ggplot(melt1, aes(x = Year_data, y = value, fill = variable)) + 
  geom_bar(stat = "identity")+coord_flip()+
  geom_text(aes(y=value, label=value),
            position = position_stack(vjust = 0.5),
            size=4)+
  labs(title="Evoluci?n de las importaciones del Per?",
       subtitle = "En millones de pesos, acumulados de Enero a (fecha de corte)",
       caption = "Fuente: BCP\n Elaboracion:Jose Miguel Pereira",
       x="Periodo acumulado",y="Millones de soles")+
  scale_fill_brewer(palette = "Spectral")+
  theme_minimal()
last_plot()
ggsave("importacionesstackflip.png", width = 8, height = 8) 
dev.off()