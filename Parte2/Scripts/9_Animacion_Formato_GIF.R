# Animacion: Graficos en formato GIF =====

# Librerias ====
library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)
library(gganimate)
library(animation)

# Data ====
Datos <- read.xlsx("Data/importacionesperu.xlsx",
                   sheet = "Mensuales",
                   detectDates = T)

Datos$periodo <-  seq(as.Date("2014/08/1"), 
                      as.Date("2019/05/1"), 
                      by = "month")


Datosagrupados <- Datos %>%
                  mutate(Year_Data=year(periodo),
                         mes=month(periodo))%>%
                  select(Year_Data,mes,everything())%>%
                  select(-Total,-periodo)%>%
                  #filter(mes<=tail(mes,n=1))%>%
                  group_by(Year_Data)

Melt_Agrupados <- melt(Datosagrupados,id.vars = c("Year_Data","mes"))

# Los animados se basan en ggplot
Grafico_Animado <-
                  # Datos en ggplot
                  ggplot(Melt_Agrupados,
                          aes(x=factor(Year_Data),
                              y=value))+
                   # Geometr?a
                   geom_boxplot(aes(fill=variable))+
                   # Agregando las facetas
                   facet_wrap(~variable,scales = "free")+
                   # jitter para evaluar los posibles valores at?picos
                   geom_jitter(width=0.1,alpha=0.2,aes(color=variable))+
                   # Agregando los temas
                   theme(legend.position = "none",
                         axis.text.x = element_text(angle = 90, hjust = 1))

# Ejemplos =====
Grafico_Animado <- 
                   Grafico_Animado+
                   transition_manual(Year_Data,
                                     cumulative = T)
                  # cumulative, es para evitar que las cajas se vayan borrando
                  # Se mantengan durante las siguientes secuencias
Grafico_Animado


# Configuraciones adicionales
# por ejemplo, que muestre el frame (periodo como t?tulo)
Grafico_Animado <- 
                    Grafico_Animado+
                    transition_manual(Year_Data,cumulative = T)+
                    labs(title = "Year_Data: {(current_frame)}")
                    # Va agregando el Year_Data en el frame!
  
Grafico_Animado


# Guardar el GIF mediante animate.
animate(Grafico_Animado,
        end_pause = 4,
        width=1100, 
        height=600,
        fps = 5)
anim_save(file="animacion1.gif",animation = last_animation())

# Frames ==== 

Grafico_Animado <- Grafico_Animado+transition_states(Year_Data,
                   transition_length = 2,
                   state_length = 1)+
                   enter_fade() + 
                   exit_shrink()+
                   labs(title = "Year_Data: {closest_state}")
  

# Grafico animado para una serie de tiempo

Melt_Agrupados2 <- melt(Datos,id.vars = c("periodo"))

Grafico_Animado2 <- ggplot(data=Melt_Agrupados2,aes(x=periodo,y=value))+
  geom_line()+facet_wrap(.~variable  ,scales = "free")

Grafico_Animado2 <- Grafico_Animado2+
                    transition_reveal(along = periodo)

Grafico_Animado2 <- Grafico_Animado2+
                    geom_point()+ 
                    transition_reveal(along = periodo)+
  labs(title = "Mes de encuesta: {frame_along}")


Grafico_Animado2