# Operador Pipe ====

# Librerias ====
library(dplyr)
library(lubridate)

# Data ====
data <- read.csv("Data/Iowa_Liquor_Sales.csv",
                 stringsAsFactors=F,
                 header=T)

# Transformemos de caracter a numerico la variable ventas en dolares
data <- mutate(data,
               Sale..Dollars.=(
                 as.numeric(
                   substr(data$Sale..Dollars.,2,15))))

# Convirtiendo minusculas en mayusculas
data <- mutate(data,
               City=sapply(data$City,toupper))

# Visualizar el conteo
table(data$City)

# Operador pipe %>% sirve para continuar una linea sin ejecutarlar
# compactando el proceso de codificacion

# Forma 1 ====
# Se puede hacer un summarise as agrupando 
# seleccionando la base agrupada
g1 <- group_by(data,City)
summarise(g1,
          media=mean(Sale..Dollars.))

# Forma 2 ====
# Nos permite limitar los recursos empleados de la computadora
data %>%
  group_by(City) %>%
  summarise(media=mean(Sale..Dollars.))

# Compactando las funciones
# Se emplean summarise_each((funs()),var)
data %>%
  group_by(City,Category.Name) %>%
  summarise_each(funs(
    media=mean(.),
    minimo=min(.),
    maximo=max(.)),Sale..Dollars.)

data <- mutate(data,
               Date=as.Date(data$Date,
                            format = "%m/%d/%Y"))
class(data$Date)


# Compactando funciones extensas ====
# Agrupar por Date_year
data %>%  # Data
  mutate(Date=as.Date(data$Date,    
                      format = "%m/%d/%Y"),     # Funcion para modificar anio
         Date_year=year(data$Date))%>%   # Extrae el anio de la columna date
  # El objeto anio, no se refleja en la DB, No existe..
  group_by(City,Date_year) %>%    # Funcion para agrupar por ciudad, anio
  summarise_each(funs(       # Summarise_each(funs()) Empiezan las funciones individuales
    # Agregar los estad?sticos a emplear
    media=mean(.),
    minimo=min(.),
    maximo=max(.)),Sale..Dollars.)

# Agrupar por Date_year mayor a 2016
data %>%
  mutate(Date=as.Date(data$Date,
                      format = "%m/%d/%Y"),
         Date_year=year(data$Date))%>%
  # Se agregan filtros
  filter(Date_year>=2016)%>% # Solo con datos mayores o iguales al 2016
  group_by(City,Date_year) %>%
  summarise_each(funs(
    media=mean(.),
    minimo=min(.),
    maximo=max(.)),Sale..Dollars.)

# Ordenar segun la media
data %>%
  mutate(Date=as.Date(data$Date,
                      format = "%m/%d/%Y"),
         Date_year=year(data$Date))%>%
  filter(Date_year>=2016)%>%
  group_by(City,Date_year) %>%
  summarise_each(funs(
    media=mean(.),
    minimo=min(.),
    maximo=max(.)),Sale..Dollars.)%>%
    arrange(desc(media)) # Ordenado descendentemente segun la media


# Numero de botellas vendidas por mes en el anio 2016
data %>%
  mutate(Date=as.Date(data$Date,
                      format = "%m/%d/%Y"),
         Date_year=year(data$Date),
         mes=month(data$Date))%>%
  filter(Date_year==2016)%>%
  group_by(mes) %>%
  summarise_each(funs(
    media=mean(.),
    minimo=min(.),
    maximo=max(.)),Bottles.Sold)%>%
  arrange(mes)


# Numero de botellas vendidas por mes en el anio 2016, por ciudad
resumen <- data %>%
  mutate(Date=as.Date(data$Date,
                      format = "%m/%d/%Y"),
         Date_year=year(data$Date),
         mes=month(data$Date))%>%
  filter(Date_year==2016)%>%
  group_by(City,mes) %>%
  summarise_each(funs(
    media=mean(.),
    minimo=min(.),
    maximo=max(.)),Bottles.Sold)%>%
  arrange(mes)
View(resumen)
rm(resumen)

# Compactando usando select ====
resumen <- data %>%
  select(City,Bottles.Sold,Sale..Dollars.)%>%
  mutate(Date=as.Date(data$Date,
                      format = "%m/%d/%Y"),
         Date_year=year(data$Date),
         mes=month(data$Date))%>%
  filter(Date_year==2016)%>%
  group_by(City,mes) %>%
  summarise_each(funs(
    media=mean(.),
    minimo=min(.),
    maximo=max(.)),Bottles.Sold)%>%
  arrange(mes)
View(resumen)
rm(resumen)
