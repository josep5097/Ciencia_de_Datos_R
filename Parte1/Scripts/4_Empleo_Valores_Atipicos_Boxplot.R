# Deteccion de valores Atipico mediante boxplot ====

# Librerias ====
library(ggplot2)
library(dplyr)
library(lubridate)
library(outliers)
library(lattice)
library(reshape2)

# Data ====
Informacion <- read.csv("Data/empleo.csv")

# Observar la informacion
names(Informacion)

# Bloxplot inicial
boxplot(Informacion$Employment..in.thousands., data=Informacion)

# Manipulacion
empleo <- Informacion %>%
          mutate(Month.Ending=as.Date(Month.Ending,
                                      format = "%m/%d/%Y"),
                 Mes = month(Month.Ending)) %>%
          filter(Year>=2000)%>%
          arrange(Year, Mes)

boxplot(empleo$Employment..in.thousands.,data=empleo)

boxplot(empleo$Employment..in.thousands. ~Year,data=empleo, 
        las=2)

empleo2 <- Informacion %>%
  mutate(Month.Ending=as.Date(Month.Ending,
                              format = "%m/%d/%Y"),
         Mes = month(Month.Ending)) %>%
  filter(Year>=2000, Category=="Service-Providing")%>%
  arrange(Year, Mes)

boxplot(empleo2$Employment..in.thousands. ~Year,
        data=empleo2, 
        las=2,
        main = "Service-Providing")

## Uso de la libreria Lattice
selempleo <- empleo2 %>%
              select(c(Year,Category,Employment..in.thousands.))
# Melt es de reshape2
empleoreshape <- melt(selempleo,
                      id = c("Year","Category"))

bwplot(value~factor(Year)|variable,
       data = empleoreshape,
       main = "Boxplot con Lattice")

# Realizar por subsector
selempleo2 <- empleo2 %>%
  select(c(Year,Category,
           Supersector.or.Industry,
           Employment..in.thousands.))
# Melt es de reshape2
empleoreshape2 <- melt(selempleo2,
                      id = c("Year",
                             "Category",
                             "Supersector.or.Industry"))

bwplot(value~factor(Year)|Supersector.or.Industry,
       data = empleoreshape2,
       main = "Boxplot con Lattice",
       scale = list(x=list(rot = 90)))

# Concentrandonos en Informacion

xx <- empleoreshape2 %>%
      filter(Supersector.or.Industry == "Information")

bwplot(value~factor(Year)|Supersector.or.Industry,
       data = xx,
       main = "Boxplot con Lattice",
       scale = list(x=list(rot = 90)))

# Concentrandonos en Retail

xy <- empleoreshape2 %>%
  filter(Supersector.or.Industry == "Retail Trade")

bwplot(value~factor(Year)|Supersector.or.Industry,
       data = xy,
       main = "Boxplot con Lattice",
       scale = list(x=list(rot = 90)))

# Extrayendo poder a partir de la Data

bwplot(value~factor(Year)|Supersector.or.Industry,
       data = empleoreshape2,
       main = "Boxplot con Lattice",
       scale = list(x=list(rot = 90),
                    y=list(relation = "sliced")))

# Tratamiento de Valores Atipicos

selempleo3 <- empleo2 %>%
  select(c(Year,Category,
           Supersector.or.Industry,
           Employment..in.thousands.))

empleoreshape3 <- melt(selempleo3,
                       id = c("Year",
                              "Category",
                              "Supersector.or.Industry"))

# Contraste Chi Cuadrado
# Ho: No existe un valor atipico
# H1: Existe un Valor atipico

max(empleoreshape3$value)
median(empleoreshape3$value)

# Libreria outliers ====
# Si el valor de p<0.05 se rechaza la H0
chisq.out.test(empleoreshape3$value)
# Con p<0.05 se rechaza H0, con lo cual existe al menos 1 valor

# Contraste Grubbs
# Se analiza hasta 2 valores atipicos
# H0: No existe hasta 2 valores atipicos
# H1: Existe al menos 2 valores atipicos

grubbs.test(empleoreshape3$value)
# Con p=1, no se rechaza la H0, con lo cual no existen al menos 2 valores.

# Funci?n que muestran los valores atipicos
outlier(empleoreshape3$value)

# Reemplazando los valores atipicos
zz <- empleoreshape3[empleoreshape3$Supersector.or.Industry == "Retail Trade",]
Zz2 <- data.frame(rm.outlier(zz$value,fill = TRUE, median = FALSE))

aa <- cbind(zz$value, Zz2)
ts.plot(aa,
        col = c("red","blue"))

empleoreshape3 <- melt(selempleo3,
                       id = c("Year",
                              "Category",
                              "Supersector.or.Industry"
                              ))

empleoreshape3[empleoreshape2$Supersector.or.Industry=="Retail Trade", "Value"] <- (rm.outlier(zz$value, fill = T, median = T))

XX3 <- empleoreshape2 %>%
      filter(Supersector.or.Industry=="Retail Trade")
bwplot(value ~ factor(Year)|Supersector.or.Industry,
       data=XX3,
       main="Boxplot",
       scales=list(x=list(rot=90)))

XX4 <- empleoreshape3 %>%
  filter(Supersector.or.Industry=="Retail Trade")
bwplot(value ~ factor(Year)|Supersector.or.Industry,
       data=XX4,
       main="Boxplot",
       scales=list(x=list(rot=90)))
