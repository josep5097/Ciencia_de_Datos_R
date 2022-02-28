# Graficando mediante plotly ====

# Librerias ====
library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)
library(plotly)

# Data ====
Datos <- read.xlsx("Data/importacionesperu.xlsx",
                   sheet = "Mensuales",
                   detectDates = T)

Datos$periodo <-  seq(as.Date("2014/08/1"), 
                      as.Date("2019/05/1"), 
                      by = "month")



Datosagrupados <- Datos %>%
  mutate(Year_data=year(periodo),
         Mes=month(periodo))%>%
  select(Year_data,Mes,everything())%>%
  select(-Total,-periodo)%>%
  filter(Mes<=tail(Mes,n=1))%>%
  group_by(Year_data)%>%
  summarise_each(list(sum))%>%
  select(-Mes)  

Melt_Datos <- melt(Datosagrupados,id.vars = "Year_data")


agrupados2 <- ggplot(Melt_Datos,
                     aes(x=Year_data,y=value,group=variable))+
              geom_area(aes(color=variable,fill=variable),alpha=0.2)+
              theme(legend.position = "bottom")+
              theme_minimal()

# Usando el graficador ggplotly
ggplotly(agrupados2)