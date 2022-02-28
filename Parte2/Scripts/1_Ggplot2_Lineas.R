# Graficas de Lineas ====

# Librerias ====
library(openxlsx)
library(ggplot2)
library(reshape2)
library(scales)

# Cargar la informacion ====
Datos <- read.xlsx("Data/BASE DE DATOS.xlsx",
                   sheet = "Hoja1",
                   detectDates = T)

# Estructura GGPLOT ====

# El flujo es el siguiente:
# data,esteticas,geometrias,complementos

# Data
# Esteticas (Dirigir seg?n el plano x,y)
# Geometrias (x, y, alpha, color, group, linetype, size)
# Complementos (titulos, subtitulos, escalas, temas, lineas de intercepcion,
# colores, estadisticas, regresiones, etc)

# Realizando Grafica ====
# Con los +, se van agregando las capas.
ggplot(data=Datos)+ # Data
                  # Estetica - Apuntando al eje x
                  aes(x=PERIODO)+   
                  # Estetica - Apuntando al eje y
                  aes(y=EXPORTACIONES)+
                  # Geometria de lineas para series temporales
                  geom_line(color="blue", 
                            alpha=0.5, 
                            size=1)+
                  # Unir lineas y puntos 
                  geom_point(color="red",
                             alpha=0.7,
                             size=1.5)+  
                  # alpha - Transparencia - 1 mas marcado
                  # Agregar una linea horizontal
                  geom_hline(yintercept = mean(Datos$EXPORTACIONES), 
                             col = "grey",
                             size=1,
                             alpha=0.5)+
                  # Valor Minimo
                  geom_hline(yintercept = min(Datos$EXPORTACIONES), 
                             col = "grey",
                             size=1,
                             alpha=0.5)+
                  # Valor Maximo
                  geom_hline(yintercept = max(Datos$EXPORTACIONES), 
                             col = "grey",
                             size=1,
                             alpha=0.5)+
                  # Agregar una l?nea Vertical Para comparar fechas
                  # Primera Fecha
                  geom_vline(xintercept = as.numeric(
                              as.Date(tail(Datos$PERIODO,n=1))), 
                             linetype=1)+
                  # Segunda Fecha
                  geom_vline(xintercept = as.numeric(
                              as.Date(tail(Datos$PERIODO,n=13)[1])), 
                             linetype=1)+
                  # Tercera Fecha
                  geom_vline(xintercept = as.numeric(
                              as.Date(tail(Datos$PERIODO,n=25)[1])), 
                             linetype=1)+
                  # Agregar Comentarios
                  # Media
                  annotate(geom = "text",
                           x = as.Date(tail(Datos$PERIODO,n=1)[1]),
                           y=mean(Datos$EXPORTACIONES),
                           label= "Media",
                           angle=0,
                           size=4)+
                  # Minimo
                  annotate(geom = "text",
                           x = as.Date(tail(Datos$PERIODO,n=1)[1]),
                           y=min(Datos$EXPORTACIONES)-20000,
                           label="M?nima",angle=0,size=4)+
                  # Maximo
                  annotate(geom = "text",
                           x = as.Date(tail(Datos$PERIODO,n=1)[1]),
                           y=max(Datos$EXPORTACIONES)+30000,
                           label="M?nima",angle=0,size=4)+
                  # Textos - Titulos
                  labs(title="Evolucion de las exportaciones del Ecuador",
                       subtitle = "En miles de millones",
                       caption = "Fuente: BCE\n Elaboracion: Jose Miguel Pereira")+
                  # Configuracion del tema
                  theme(text = element_text(size=14),
                        legend.position = "bottom",
                        axis.text.x = element_text(angle = 90, hjust=1,size=9))+ #un poco de cambios en fechas
                  # Configuracion de la escala del texto
                  scale_x_date(date_labels = "%Y %b",breaks=Datos$PERIODO)
                  # scale_x_date(date_labels = "%Y %b",breaks=scales::pretty_breaks(n=6))

last_plot()
ggsave("exportaciones.png", width = 10, height = 6)
dev.off()