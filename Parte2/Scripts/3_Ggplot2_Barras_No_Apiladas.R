# Graficos de Barras ====

# Librerias ====
library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)

# Data ====
Datos <- read.xlsx("Data/BASE DE DATOS.xlsx",
                   sheet = "Hoja1",
                   detectDates = T)

# Ejemplo no apropiado para barras - mejor con series de tiempo
ggplot(data=Datos)+
  aes(x=PERIODO)+
  aes(y=EXPORTACIONES)+
  geom_bar(position = "dodge",stat="identity")
  # Dogde - Barras una a lado de las otras 
  # Identity - Poner los valores propios del dataframe

# Barras se opera mejor cuando la data se encuentra trabajada.
# Agrupando un poco los datos para poder analizarlos
Datos <- Datos %>%
         mutate(data_year=year(Datos$PERIODO),
                mes=month(Datos$PERIODO)) %>%
         select(data_year,mes, everything())  

# Realizando filtrado de la informacion
datosagrupados <- Datos %>%
                  filter(mes<=tail(Datos$mes,n=1))%>%
                  select(-PERIODO)%>%
                  mutate(data_year=as.numeric(data_year))%>%
                  group_by(data_year)%>%
                  summarise_each(list(sum)) # Resumir la suma de los agrupados

datosagrupados <- round(datosagrupados,2)
data_year <- seq(2010,2016)

# Graficos ====
ggplot(data=datosagrupados)+
       aes(x=data_year)+
       aes(y=EXPORTACIONES)+
       geom_bar(position = "dodge",stat="identity")+
       # Colocar valores encima de las barras
       geom_text(aes(label=EXPORTACIONES), 
                     vjust=-1, # Con 0 -> Encima de la barra 
                     color="black", 
                     size=4)+
       labs(title="Evolucion de las exportaciones del Ecuador",
           subtitle = "En miles de millones",
           caption = "Fuente: BCE\n Elaboracion:autor")+
       scale_x_continuous(name = waiver(),n.breaks = 7)+
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 90,
                                         hjust=1,
                                         size=10))
       last_plot()
ggsave("exportacionesbarra.png", width = 8, height = 5)

# Barras horizontal
ggplot(data=datosagrupados)+
  aes(x=data_year)+
  aes(y=EXPORTACIONES)+
  geom_bar(position = "dodge",stat="identity")+
  # Invertir las coordenadas para obtener las barras horizontales
  coord_flip()+
  geom_text(aes(label=EXPORTACIONES), vjust=0, color="black", size=4,
            position = position_dodge(width = 1),
            angle=270)+
  scale_x_continuous(name = waiver(),n.breaks = 7)+
  labs(title="Evolucion de las exportaciones del Ecuador",
       subtitle = "En miles de millones",
       caption = "Fuente: BCE\n Elaboracion:autor")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(hjust=1,
                                   size=10))
last_plot()
ggsave("exportacionesbarraflip.png", width = 8, height = 8)
dev.off()
