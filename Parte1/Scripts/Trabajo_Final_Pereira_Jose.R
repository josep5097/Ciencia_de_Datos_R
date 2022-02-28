# Trabajo Final  ====

# Librerias  ====
library(fdth)
library(dplyr)
library(lubridate)
library(lattice)
library(reshape2)

# Seccion A ====

# Cargar la base en GDP_Data
GDP_Data <- read.csv("Data/gdpindustrias.csv",
                        stringsAsFactors = F)

# Manipulacion de la informacion ====

# Obtencion fecha y data_year
GDP_Data <- GDP_Data %>%
  mutate(
    Date.Quarter.Ended = as.Date(Date.Quarter.Ended,
                   format = "%m/%d/%Y"),
    data_year = year(Date.Quarter.Ended)
  )

# Obtencion de los historicos para Information
Information_Data <- GDP_Data %>%
                    filter(Industry == "Information")

Resume_Info <- Information_Data %>%
  group_by(data_year, Industry)%>%
  summarise(
    Sum = sum(GDP..millions.of.current.dollars.))

# Obtencion de los historicos para Construction
Construction_Data <-GDP_Data %>%
                    filter(Industry == "Construction")

Resume_Const <- Construction_Data %>%
  group_by(data_year, Industry)%>%
  summarise(
    Sum = sum(GDP..millions.of.current.dollars.))

# Realizar una prueba de medias para contrastar si:
# PIB(Information) similar al PIB(Construccion).

# H0: medias similares
# H1: medias diferentes

t.test(x = Resume_Const$Sum,
       y = Resume_Info$Sum,
       alternative = "two.sided",
       paired = F)

# Con p<0.05 -> Se rechaza H0.
# Existe una diferencia de medias entre el PIB de information y el PIB de Construction

# Realizar el boxplot con las 5 industrias con mayor PIB 
# Y la evolucion por year.
Information_Data_All <- GDP_Data %>%
  group_by(Industry)%>%
  summarise(
    Sum = sum(GDP..millions.of.current.dollars.))%>%
  arrange(desc(Sum))

boxplot(Information_Data_All$Sum ~ Industry, 
        data = Information_Data_All,
        las=2,
        main = "PIB por year",
        xlab = "year",
        ylab = "PIB")

# Extraccio de los datos de las 5 Industrias con mayor PIB
GDP_Data_5 <- GDP_Data %>%
              filter(Industry == as.character(Information_Data_All[1,1])|
                     Industry == as.character(Information_Data_All[2,1])|
                     Industry == as.character(Information_Data_All[3,1])|
                     Industry == as.character(Information_Data_All[4,1])|
                     Industry == as.character(Information_Data_All[5,1]))
# Ordenar la Data
Inf_Lattice <- GDP_Data_5 %>%
  select(c(data_year,
           Industry.Type,
           Industry,
           GDP..millions.of.current.dollars.))

# Melt para poder emplear el boxplot de Lattice
Inf_Reshape <- melt(Inf_Lattice,
                       id = c("data_year",
                              "Industry.Type",
                              "Industry"))

# Boxplot a partir de Lattice
bwplot(value~factor(data_year)|Industry,
       data = Inf_Reshape,
       main = "Top 5 de Industrias con mayor PIB",
       scale = list(x=list(rot = 90),
                    y=list(relation = "sliced")))

# Seccio B ====

# Seleccionar industria Information hasta 2016
# En la variable Information_Data se tiene toda la informacion hasta el 2017
Information_Data_Nueva <- Information_Data %>%
                          filter(data_year<=2016)

# Dividir en dos periodos: 2005-2010 y 2011-2016
Periodo1 <- Information_Data_Nueva %>%
            filter(data_year >= 2005, data_year <= 2010)
Periodo2 <- Information_Data_Nueva %>%
            filter(data_year >= 2011, data_year <= 2016)

# U del primero > U del segundo
t.test(x = Periodo1$GDP..millions.of.current.dollars.,
       y = Periodo2$GDP..millions.of.current.dollars.,
       alternative = "greater",
       paired = T)

# H0: uPeriodo1 = uPeriodo2
# H1: uPeriodo1 > uPeriodo2
# Con p < 0.05 se rechaza H0 y se considera que la la media del primer periodo
# es mayor que la media del segundo periodo.

# Considerando la industria Construccio
# Filtro para tener la data menor al year 2016
Construction_Data_Nueva <- Construction_Data %>%
                            filter(data_year<=2016)

# Divir en dos periodos
Periodo3 <- Construction_Data_Nueva %>%
            filter(data_year >= 2005, data_year <= 2010)
Periodo4 <- Construction_Data_Nueva %>%
            filter(data_year >= 2011, data_year <= 2016)

# U del primero > U del segundo
t.test(x = Periodo1$GDP..millions.of.current.dollars.,
       y = Periodo2$GDP..millions.of.current.dollars.,
       alternative = "greater",
       paired = T)

# H0: uPeriodo3 = uPeriodo4
# H1: uPeriodo3 > uPeriodo4
# Con p < 0.05 se rechaza H0 y se considera que la la media del primer periodo,
# donde primer periodo es Periodo3