# Taller ====
# Librerias ====
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)
library(plotly)
library(gganimate)
library(animation)
library(tidyquant)  
library(xml2)
library(dygraphs)

# Data ====
Datos_Sector_Real <- read.csv("Data/Inflacion_anual_nacional.csv",
                              stringsAsFactors=F,
                              header=T,sep = ";",dec = ",")

# Revisar los tipos de datos que se tienen
str(Datos_Sector_Real)


# Seccion A ====

## Grafics de Boxplot ====

# Realizar un boxplot usando ggplot2, de una de las variables descargadas.
# Convertirlo a GIF con una transici?n que ACUMULE las cajas y haga una 
# pausa prolongada al final del gr?fico con el fin de poder visualizarla
# de forma completa.

Datos_Sector_Real_nuevo <- Datos_Sector_Real %>% 
                                                    mutate(Datos_Sector_Real,
                                                           Fecha = as.Date(
                                                             substr(Datos_Sector_Real$Mes,1,10)),
                                                           Year_Data = year(Fecha),
                                                           Mes = month(Fecha))

Datos_Sector_Real_nuevo <- Datos_Sector_Real_nuevo %>% 
                                            mutate(Datos_Sector_Real_nuevo,
                                                   Region = gsub(" ", "", Datos_Sector_Real_nuevo$`Region`),
                                                   Inflacion_Anual = Datos_Sector_Real_nuevo$`Inflacion_Anual`
                                                   )

Datos_Sector_Real_nuevo <- Datos_Sector_Real_nuevo %>%
                                            select(Year_Data,Mes,Region,Inflacion_Anual)

str(Datos_Sector_Real_nuevo)

# Realizar el boxplot

Datos_Boxplot <- ggplot(Datos_Sector_Real_nuevo,
                              aes(Region,Inflacion_Anual, fill=Region))+
                              geom_boxplot()+
                              geom_jitter(width=0.1,alpha=0.2)+
                              facet_wrap(~Year_Data,scales = "free")+
                              theme(
                                    legend.position = "bottom",
                                    axis.text.x = element_text(size = 7,angle=90,hjust = 1))+
                              # Textos - T?tulos
                              labs(title="Inflacion anual por provincias del Ecuador",
                                 subtitle = "En porcentaje",
                                 caption = "Fuente: BCE\n Elaboracion: Jose Miguel Pereira",
                                 ylab("Inflacion Anual %"))

# boxplot con ggplotly

ggplotly(Datos_Boxplot)

# Realizando el GIF

Datos_Boxplot <- 
                  Datos_Boxplot+
                  transition_manual(Region,
                                    cumulative = T)

Datos_Boxplot

# Guardar el GIF mediante animate.

animate(Datos_Boxplot,
        end_pause = 10,
        width=1100, 
        height=600,
        fps = 3,)
anim_save(file="Boxplot_Graf.gif",animation = last_animation())


# Grafico de Barras ====


# Realizar un grafico de barras No apiladas para 2 variables que tengan sentido 
# exportaciones e importaciones, cr?ditos y dep?sitos, etc.
# Convertirlo a GIF con una transici?n que ACUMULE las cajas y haga una 
# pausa prolongada al final del gr?fico con el fin de poder visualizarla
# de forma completa.
# Colocar los valores encima de cada barra.

Datos_Imp_Exp <- read.csv("Data/Exterior_Exportaciones_Totales_mensual_puntoycoma.csv",
                              stringsAsFactors=F,
                              header=T,sep = ";",dec = ",")

# Revisar los tipos de datos que se tienen

str(Datos_Imp_Exp)

Datos_Imp_Exp <- Datos_Imp_Exp %>% 
                               mutate(Datos_Imp_Exp,
                                      Fecha = as.Date(paste(as.character(Datos_Imp_Exp$Year_Data),
                                                            as.character(Datos_Imp_Exp$Mes.Numero),
                                                            "1",
                                                            sep = "-")),
                                      Year_Data = year(Fecha)
                                      )

Datos_Imp_Exp <- Datos_Imp_Exp %>%
                                select(Year_Data,Variable,Valor)%>%
                                group_by(Year_Data)

Datos_Importaciones <- Datos_Imp_Exp %>%
                                filter(Variable == "Total Importaciones FOB")%>%
                                select(-Variable) %>%
                                group_by(Year_Data) %>%
                                summarise(
                                  Sum = round(sum(Valor), 0)) # Resumir la suma de los agrupados

Datos_Exportaciones <- Datos_Imp_Exp %>%
                                filter(Variable == "Total Exportaciones FOB")%>%
                                select(-Variable) %>%
                                group_by(Year_Data) %>%
                                summarise(
                                  Sum = round(sum(Valor), 0)) # Resumir la suma de los agrupados

Datos_Exp_Imp_Unidos <- right_join(Datos_Importaciones,Datos_Exportaciones,
                                   by="Year_Data")

colnames(Datos_Exp_Imp_Unidos) <- c("Year_Data", "Importaciones en Ecuador", "Exportaciones en Ecuador")

# Combinacion de los datos

Melt_Exp_Imp <- melt(Datos_Exp_Imp_Unidos,id.vars = "Year_Data")


# Graficos de Barras de Importaciones y Exportaciones ====
# Realizar el grafico de barras
Datos_Barras_Animados <- ggplot(data=Melt_Exp_Imp)+
                         aes(x = Year_Data, y = value, fill = variable)+
                         geom_bar(position = "stack",stat="identity")+
                         geom_text(aes(y=value, label=value),
                                   position = position_stack(vjust = 0.5),
                                   size=4)+
                         # Colocar valores encima de las barras
                         labs(title="Hist?rico de Importaciones y Exportaciones del Ecuador",
                              subtitle = "En millones USD FOB",
                              caption = "Fuente: BCE\n Elaboracion: Jose Miguel Pereira Ponton")+
                         theme(legend.position = "bottom",
                               axis.text.x = element_text(angle = 90,
                                                          hjust=1,
                                                          size=10))


# Realizando el GIF
Datos_Barras_Animados <- 
                          Datos_Barras_Animados+
                          transition_manual(Year_Data,
                                            cumulative = T)

Datos_Barras_Animados

# Guardar el GIF mediante animate.
animate(Datos_Barras_Animados,
        end_pause = 10,
        width=1600, 
        height=750,
        fps = 10,)

anim_save(file="Barras_Graf_Stack.gif",animation = last_animation())


# Grafico de Barras Solo Importaciones ====
# Realizar el grafico de barras
Datos_Barras_Animados_Imp <- ggplot(data=Datos_Importaciones)+
                                              aes(x=Year_Data)+
                                              aes(y=Sum)+
                                              geom_bar(position = "dodge",stat="identity")+
                                              geom_text(aes(y=Sum, label=Sum),
                                                        position = position_stack(vjust = 0.5),
                                                        size=4)+
                                              # Colocar valores encima de las barras
                                              labs(title="Historico de Importaciones del Ecuador",
                                               subtitle = "En millones USD FOB",
                                                   caption = "Fuente: BCE\n Elaboracion: Jose Miguel Pereira Ponton")+
                                              theme(legend.position = "bottom",
                                                    axis.text.x = element_text(angle = 90,
                                                                               hjust=1,
                                                                               size=10))
Datos_Barras_Animados_Imp
last_plot()

ggsave("Importaciones_barra.png", width = 8, height = 5)

Datos_Barras_Animados_Imp <- 
                            Datos_Barras_Animados_Imp+
                            transition_manual(Year_Data,
                                              cumulative = T)

Datos_Barras_Animados_Imp

# Guardar el GIF mediante animate

animate(Datos_Barras_Animados_Imp,
        end_pause = 10,
        width=1600, 
        height=750,
        fps = 10,
        nframes = 150)
anim_save(file="Barras_Graf_Imp.gif",animation = last_animation())


# Grafico de Barras Solo Exportaciones ====

# Realizar el grafico de barras

Datos_Barras_Animados_Export <- ggplot(data=Datos_Exportaciones)+
                                aes(x=Year_Data)+
                                aes(y=Sum)+
                                geom_bar(position = "dodge",stat="identity")+
                                geom_text(aes(y=Sum, label=Sum),
                                          position = position_stack(vjust = 0.5),
                                          size=4)+
                                # Colocar valores encima de las barras
                                labs(title="Hist?rico de Exportaciones del Ecuador",
                                     subtitle = "En millones USD FOB",
                                     caption = "Fuente: BCE\n Elaboracion: Jose Miguel Pereira Ponton")+
                                theme(legend.position = "bottom",
                                      axis.text.x = element_text(angle = 90,
                                                                 hjust=1,
                                                                 size=10))
last_plot()
ggsave("exportacionesbarra.png", width = 8, height = 5)

# Realizar el GIF

Datos_Barras_Animados_Export <- 
                                Datos_Barras_Animados_Export+
                                transition_manual(Year_Data,
                                                  cumulative = T)

Datos_Barras_Animados_Export

# Guardar el GIF mediante animate.

animate(Datos_Barras_Animados_Export,
        end_pause = 10,
        width=1600, 
        height=750,
        fps = 10,
        nframes = 150)
anim_save(file="Barras_Graf_Imp.gif",animation = last_animation())


# Grafico de Lineas ====


# Escoger 2 variables y realizar un gr?fico de l?neas en dos cuadros, usando facet
# debe de incorporar la l?nea de tendencia de las dos variables.
# Convertirlo a GIF con una transici?n que ACUMULE las cajas y haga una 
# pausa prolongada al final del gr?fico con el fin de poder visualizarla
# de forma completa.
# Colocar los valores encima de cada punto.
Datos_Balanza <- read.csv("Data/Exterior_Exportaciones_Totales_mensual_puntoycoma.csv",
                          stringsAsFactors=F,
                          header=T,sep = ";",dec = ",")

# Revisar los tipos de datos que se tienen

str(Datos_Balanza)

Datos_Balanza <- Datos_Balanza %>% 
                              mutate(Datos_Balanza,
                                     Fecha = as.Date(paste(as.character(Datos_Balanza$Year_Data),as.character(Datos_Balanza$Mes.Numero),"1",sep = "-")),
                                     Year_Data = year(Fecha),
                                     Mes = month(Fecha)
                              )

Datos_Balanza <- Datos_Balanza %>%
                              select(Year_Data,Mes,Variable,Valor)%>%
                              group_by(Year_Data)

Datos_Balanza_Comercial_NP <- Datos_Balanza %>%
                              filter(Variable == "Balanza Comercial no petrolera",
                                     Year_Data >= 2019)%>%
                              select(-Variable) %>%
                              group_by(Year_Data) #%>%
                              #summarise(
                                #Sum = round(sum(Valor), 0)) # Resumir la suma de los agrupados

Datos_Balanza_Comercial_P <-  Datos_Balanza %>%
                              filter(Variable == "Balanza Comercial Petrolera",
                                     Year_Data >= 2019)%>%
                              select(-Variable) %>%
                              group_by(Year_Data) #%>%
                              #summarise(
                                #Sum = round(sum(Valor), 0)) # Resumir la suma de los agrupados

Datos_Balanza_Melt <- right_join(Datos_Balanza_Comercial_P,Datos_Balanza_Comercial_NP,
                                 by=c("Year_Data","Mes")
                                )

colnames(Datos_Balanza_Melt) <- c("Year_Data", "Mes", "Balanza Comercial Petrolera","Balanza Comercial No Petrolera")

# Combinaci?n de los datos

Balanza_Melt <- melt(Datos_Balanza_Melt,id.vars = c("Year_Data","Mes"))


Datos_Series_Animado <- ggplot(data=Balanza_Melt)+  
                          aes(x=Mes,y=value)+
                          geom_line()+
                          facet_wrap(variable~Year_Data ,scales = "free")+
                          # Colocar una tendencia en las series 
                          # Red de regresiones
                          geom_smooth(method="lm", formula = 'y ~ x')+
                          # Ajustes del eje en la leyenda
                          theme(legend.position = "bottom",
                                axis.text.x = element_text(angle = 90,
                                                           hjust=1,
                                                           size=7)
                          )+
                          # Textos - T?tulos
                          labs(title="Analisis de la Balanza Comercial Petrolera y No Petrolera del 2019-2021",
                               subtitle = "Total de Balanza Comercial",
                               caption = "Fuente: BCE\n Elaboracion: Jose Miguel Pereira Ponton")+
                          # Configuraci?n del tema
                          theme(text = element_text(size=14),
                                legend.position = "bottom",
                                axis.text.x = element_text(angle = 90, hjust=1,size=9))+ #un poco de cambios en fechas
                          # Configuraci?n de la escala del texto
                          scale_x_continuous(name = "Meses del Year_Data", n.breaks = 13)+
                          ylab("Balanza Comercial")
                          # scale_x_date(date_labels = "%Y %b",breaks=scales::pretty_breaks(n=6))

Datos_Series_Animado
ggsave("exportaciones.png", width = 10, height = 6)

# Realizar el GIF

Datos_Series_Animado <- 
                        Datos_Series_Animado+
                        transition_manual(Year_Data,
                                          cumulative = T)

Datos_Series_Animado


# Guardar el GIF mediante animate.

animate(Datos_Series_Animado,
        end_pause = 10,
        width=1600, 
        height=750,
        fps = 10,
        nframes = 150)
anim_save(file="Series_Tiempo.gif",animation = last_animation())

# Secci?n B ====
# Del gr?fico de l?neas, escoge una variable y construye un gr?fico de series
# de tiempo usando gygraph, que contenga:
#     *Rango seleccionador
#     *Evento importante
#     *Regi?n sombreada del evento importante
#     *Que los labels sigan el cursor del mouse a medida que se pasa el curso por 
#         cada punto 

Datos_Balanza <- read.csv("Data/Exterior_Exportaciones_Totales_mensual_puntoycoma.csv",
                          stringsAsFactors=F,
                          header=T,sep = ";",dec = ",")

# Revisar los tipos de datos que se tienen

str(Datos_Balanza)

Datos_Balanza <- Datos_Balanza %>% 
  mutate(Datos_Balanza,
         Fecha = as.Date(paste(as.character(Datos_Balanza$Year_Data),as.character(Datos_Balanza$Mes.Numero),"1",sep = "-")),
         Year_Data = year(Fecha),
         Mes = month(Fecha)
  )

Datos_Balanza <- Datos_Balanza %>%
                 select(Year_Data,Mes,Variable,Valor)

Data_TS_previa <-
              Datos_Balanza %>%
              filter(Variable == "Balanza Comercial no petrolera",
                     Year_Data >= 2019)%>%
              select(Valor)

TS_Data <- ts(Data_TS_previa,start = c(2019,1),frequency = 12)
TS_Data
dygraph(TS_Data)

TS_Animado <- dygraph(TS_Data,
                           # Titulo
                           main="Balanza Comercial No Petrolera",
                           # Nombre de los ejes
                           xlab ="Year_Data",
                           ylab = "Balanza") %>%
                           # Aplicando opciones extras
                           dyOptions(fillGraph = T,
                                     fillAlpha = 0.04,
                                     drawPoints = T,
                                     pointSize = 3,
                                     pointShape = "triangle",
                                     gridLineColor = "blue")%>%
                           # Se procede con el realce o con los relieves
                           dyHighlight(highlightCircleSize = 8,
                                       highlightSeriesBackgroundAlpha = 1,hideOnMouseOut = T,
                                       highlightSeriesOpts = list(strokeWidth = 3))%>%
                           # Se procede con el Rango Seleccionador
                           dyRangeSelector()%>%
                           # Generando las anotaciones
                           dyAnnotation("2019-01-01",text = "VM",tooltip = "Valor M?nimo")%>%
                           # Generando los sombreados - Usando los colores RGB
                           dyShading(from = "2020-01-01", to = "2020-12-01",color = "#99d8c9")%>%
                           dyShading(from = "2019-01-01", to = "2020-01-01",color = "#e7e1ef")%>%
                           # Generando eventos - (fecha,texto,ubicaci?n) --- Linea indicando el evento
                           dyEvent("2020-01-01","INICIO del Super?vit Comercial No Petrolero",labelLoc = "top")%>%
                           dyEvent("2020-12-01","FINAL del Super?vit Comercial No Petrolero",labelLoc = "top")%>% 
                           # Se mueve la etiqueta con el mouse
                           dyLegend(show="follow") 
TS_Animado
