# Uso de Boxplot ====

# Librerias ====
library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)

# Datos ====
Datos <- read.xlsx("Data/importacionesperu.xlsx",
                   sheet = "Mensuales",detectDates = T)

# Modificacion de la fecha
Datos$periodo <-  seq(as.Date("2014/08/1"), 
                      as.Date("2019/05/1"), 
                      by = "month")

# Modificaci?n de los Datos
Datosagrupados <- Datos %>%
  mutate(Year_data=year(periodo),
         mes=month(periodo))%>%
  select(Year_data,mes,everything())%>%
  select(-Total,-periodo)%>%
  #filter(mes<=tail(mes,n=1))%>%
  group_by(Year_data)

# Al tener varias variables se realiza el Melt con los id Year_data, mes
Melt_Datos <- melt(Datosagrupados,
                   id.vars = c("Year_data","mes"))

# Primer Caso ====
ggplot(Melt_Datos,aes(x=Year_data,y=value,fill=variable))+
       geom_boxplot()+
       facet_wrap(~variable,scales = "free")+
       theme(legend.position = "bottom")
# Se obtiene una gr?fica que no aporta mucha informacion.

# Cambiando el facet para analizar por Year_data, donde las x son las variables.
# Agregemos jitter generar una aleatoriedad en los Datos
# y se los pueda visualizar como dispersi?n.
ggplot(Melt_Datos,aes(x=variable,y=value,fill=variable))+
       geom_boxplot()+
       geom_jitter(width=0.1,alpha=0.2)+
       facet_wrap(~Year_data,scales = "free")+
       theme(legend.position = "bottom",
       axis.text.x = element_text(angle=90,hjust = 1))

# Veamos cuando las x son Year_datas y el facet es por variable.
# es decir, generemos el boxplot por Year_data, separando por variable 
# en el encabezado del grafico
# Cambiar en x de normal a factor
ggplot(Melt_Datos,aes(x=factor(Year_data),y=value))+
  geom_boxplot(aes(fill=variable))+
               facet_wrap(~variable,scales = "free")+
               geom_jitter(width=0.1,alpha=0.2,aes(color=variable))+
               theme(legend.position = "none", axis.text.x = element_text(angle = 90, hjust = 1))


# Usando Libreria Highcharter ====
# Similar a plotly y dygraphs
# Producen graficos tipo HTML

# Libreria ====
library(highcharter)       

# Producir un boxplot manipulable
# Anterior
hcboxplot(x = Melt_Datos$value, 
          var = Melt_Datos$Year_data, 
          var2 = Melt_Datos$variable,
          outliers = TRUE) %>% 
          # Se indica que el tipo de gr?fico es en Columna
          hc_chart(type = "column") 

# Generando Graficos dinamicos ==== 
# Agregando un highchart
graficodinamicohc <- highchart() %>% 
                     # Se establece el eje
                     hc_xAxis(categories = Datos$periodo) %>% 
                     # Se a?aden series de tiempo mediante hc_add_series()
                     hc_add_series(name = "BienesConsumo", 
                                   data = Datos$BienesConsumo) %>% 
                     hc_add_series(name = "MateriaPrima", 
                                   data = Datos$MateriaPrima) %>% 
                     hc_add_series(name = "BienesCapital", 
                                   data = Datos$BienesCapital)%>%
                     hc_add_series(name = "Total", 
                                   data = Datos$Tota,
                                   type="area")%>% 
                     # Agregando subtitulos
                     hc_subtitle(text = "Evolucion de las importaciones de Peru",
                                 align = "left",
                                 style = list(color = "#2b908f", fontWeight = "bold")) %>% 
                     # Agregando los cr?ditos
                     hc_credits(enabled = TRUE,
                                text = "www.josepereira.com",
                                href = "www.josepereira.com") %>% 
                     # Indicar posici?n de la leyenda
                     hc_legend(align = "left") %>%
                     # Usando un tooltip
                     hc_tooltip(crosshairs = TRUE, 
                                backgroundColor = "#FCFFC5",
                                shared = TRUE, 
                                borderWidth = 5) %>% 
                     # Activar opciones para exportar la data
                     hc_exporting(enabled = TRUE)

# Se puede implementar como barra 
graficodinamicohc %>%
                      hc_chart(type = "column",
                      options3d = list(enabled = TRUE, beta = 15, alpha = 15))

# Agrupemos la informacion
Datosagrupados2 <- Datos %>%
                   mutate(Year_data=year(periodo),
                          mes=month(periodo))%>%
                   select(Year_data,mes,everything())%>%
                   select(-Total,-periodo)%>%
                   filter(mes<=tail(mes,n=1))%>%
                   group_by(Year_data)%>%
                   summarise_each(list(sum))

# Realizando un grafico acumulativo
highchart() %>% 
                hc_xAxis(categories = Datosagrupados2$Year_data) %>% 
                hc_add_series(name = "BienesConsumo", 
                              data = Datosagrupados2$BienesConsumo,
                              type="column") %>% 
                hc_add_series(name = "MateriaPrima", 
                              data = Datosagrupados2$MateriaPrima,
                              type="column")%>%
                hc_add_series(name = "BienesCapital", 
                              data = Datosagrupados2$BienesCapital,
                              type="column")%>%
                hc_subtitle(text = "Evolucion de las importaciones de Peru",
                            align = "left",
                            style = list(color = "#2b908f", fontWeight = "bold")) %>% 
                hc_credits(enabled = TRUE,
                           text = "www.josepereira.com",
                           href = "www.josepereira.com") %>% 
                hc_legend(align = "left") %>%
                hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                           shared = TRUE, borderWidth = 5) %>% 
                hc_exporting(enabled = TRUE)