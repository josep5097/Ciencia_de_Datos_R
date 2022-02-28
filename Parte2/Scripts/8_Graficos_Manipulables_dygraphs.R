# Graficos con dygraphs ====

# Librerias ====
library(openxlsx)
library(lubridate)
library(dplyr)
library(dygraphs)

# Data ====
Datos <- read.xlsx("Data/importacionesperu.xlsx",
                   sheet = "Mensuales",
                   detectDates = T)


Datos <- Datos%>%
         select(BienesConsumo,MateriaPrima,BienesCapital)  

# La data debe de ser de tipo series de tiempo
# Se emplea la funcion TS-Time Series
tsDatos <- ts(Datos,start = c(2014,8),frequency = 12)

# Se grafica mediante el uso de la funcion dygraph
dygraph(tsDatos)

# Dygraphs tiene algunas opciones:
#   opciones
#   realces o relieves
#   rango selector
#   anotaciones
#   lineas y sombreados para eventos
# Adem?s la posibilidad de exportar el gr?fico como un archivo .html.

# Se lo acompa?a con las opciones que proporciona con dyOption
# Se puede emplear el operador pipe

dygraph(tsDatos,
        # Titulo
        main="Evolucion de las Exportacioes del Peru",
        # Nombre de los ejes
        xlab ="Periodo",
        ylab = "Millones de soles") %>%
        # Aplicando opciones extras
        dyOptions(fillGraph = T,
                  fillAlpha = 0.04,
                  drawPoints = T,
                  pointSize = 3,
                  pointShape = "triangle",
                  gridLineColor = "blue")

# Revision de los relieves o realces
dygraph(tsDatos,
        # Titulo
        main="Evolucion de las Exportacioes del Peru",
        # Nombre de los ejes
        xlab ="Periodo",
        ylab = "Millones de soles") %>%
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
                    highlightSeriesOpts = list(strokeWidth = 3))


# Revision del rango seleccionador

dygraph(tsDatos,
        # Titulo
        main="Evolucion de las Exportacioes del Peru",
        # Nombre de los ejes
        xlab ="Periodo",
        ylab = "Millones de soles") %>%
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
        # dyRangeSelector()
        # Genera el rango seleccionado entre dos fechas
        dyRangeSelector(dateWindow = c("2017-01-01","2018-01-01"))


# Generar anotaciones y regiones de sombreado
graficodinamico <- dygraph(tsDatos,
                           # Titulo
                           main="Evolucion de las Exportacioes del Peru",
                           # Nombre de los ejes
                           xlab ="Periodo",
                           ylab = "Millones de soles") %>%
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
                          dyAnnotation("2016-01-01",text = "IE",tooltip = "Inicio Recesivo")%>%
                            # Generando los sombreados - Usando los colores RGB
                          dyShading(from = "2016-02-01", to = "2016-12-01",color = "#99d8c9")%>%
                          dyShading(from = "2018-02-01", to = "2018-12-01",color = "#e7e1ef")%>%
                            # Generando eventos - (fecha,texto,ubicacion) --- Linea indicando el evento
                          dyEvent("2016-01-01","inicio recesivo",labelLoc = "top")

# Hacer que los datos sigan segon el cursor del mouse
graficodinamico <- dygraph(tsDatos,
                           # Titulo
                           main="Evolucion de las Exportacioes del Peru",
                           # Nombre de los ejes
                           xlab ="Periodo",
                           ylab = "Millones de soles") %>%
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
                           dyAnnotation("2016-01-01",text = "IE",tooltip = "Inicio Recesivo")%>%
                           # Generando los sombreados - Usando los colores RGB
                           dyShading(from = "2016-02-01", to = "2016-12-01",color = "#99d8c9")%>%
                           dyShading(from = "2018-02-01", to = "2018-12-01",color = "#e7e1ef")%>%
                           # Generando eventos - (fecha,texto,ubicacion) --- Linea indicando el evento
                           dyEvent("2016-01-01","inicio recesivo",labelLoc = "top")%>%
                           # Se mueve la etiqueta con el mouse
                           dyLegend(show="follow")

##### Guardar en un archivo HTML
# Uso de la librer?a htmlwidgets
library(htmlwidgets)
saveWidget(graficodinamico,
           file = "Graficodinamico.html",
           selfcontained = T,
           title = "Grafico_Dinamico" )
dev.off()
