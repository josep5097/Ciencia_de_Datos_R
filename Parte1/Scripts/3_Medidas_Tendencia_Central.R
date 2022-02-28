# Medidas de Tendencia Central 1 ====

# Librerias ====
library(lubridate)
library(dplyr)

data_ventas <- read.csv("Data/Iowa_Liquor_Sales.csv",
                        stringsAsFactors = FALSE)
str(data_ventas)

# Manipulacion de fecha ====
data_ventas$Fecha <- as.Date(data_ventas$Date,
                             format = "%m/%d/%Y")

data_ventas$data_year <- year(data_ventas$Fecha)
head(data_ventas)

mes <- as.numeric(format(data_ventas$Fecha,"%m"))
data_ventas$mes <- mes

# Manipulacion del Frame ====
# Eliminar la columna Date
data_ventas <- data_ventas[c(-2)]

data_ventas2 <- data_ventas[c(1,24,25,26)]
data_ventas3 <- data_ventas[c(2:23)]

data_ventas <- cbind(data_ventas2,data_ventas3)

# Cambiar nombres de variables
names(data_ventas)
names(data_ventas)[1] <- "FACTURA"
names(data_ventas)[26] <- "VOLUMEN_VENDIDO_GALONES"

# Aplicando filtros ====
# SUBSET
newdata <- subset(data_ventas, data_year == 2016,
                  select = c(FACTURA:VOLUMEN_VENDIDO_GALONES))


names(data_ventas)[24] <- "VENTAS_DOLARES"
names(newdata)[24] <- "VENTAS_DOLARES"

data_ventas$VENTAS_DOLARES <- as.numeric(substr(data_ventas$VENTAS_DOLARES,2,15))
newdata$VENTAS_DOLARES <- as.numeric(substr(newdata$VENTAS_DOLARES,2,15))
str(newdata)

# Preguntas: ====
# Ventas por ciudad
# Convertir en mayuscula
newdata$City <- sapply(newdata$City, toupper)

# Split the data into subsets
base_ciudad <- aggregate(VENTAS_DOLARES ~ City,
                         data = newdata,
                         FUN = sum)
base_ciudad

# Venta promedio por ciudad
base_ciudad <- aggregate(VENTAS_DOLARES ~ City,
                         data = newdata,
                         FUN = mean)
base_ciudad

# Promedio de ventas por tienda
names(newdata)[10] <- "Nombre_tienda"
base_sucursal <- aggregate(VENTAS_DOLARES ~ Nombre_tienda,
                         data = newdata,
                         FUN = mean)
base_sucursal
base_Sucursal_Ordenado_mean <- base_sucursal[order(-base_sucursal$VENTAS_DOLARES),]



base_ciudad_media <- aggregate(VENTAS_DOLARES ~ City,
                         data = newdata,
                         FUN = mean)

base_ciudad_mediana <- aggregate(VENTAS_DOLARES ~ City,
                         data = newdata,
                         FUN = median)
base_ciudad_max <- aggregate(VENTAS_DOLARES ~ City,
                         data = newdata,
                         FUN = max)
base_ciudad_min <- aggregate(VENTAS_DOLARES ~ City,
                         data = newdata,
                         FUN = min)

descriptivos <- c(base_ciudad_max,
                  base_ciudad_min[2],
                  base_ciudad_media[2],
                  base_ciudad_mediana[2]
                  )

descriptivos <- as.data.frame(descriptivos)
names(descriptivos)[2] <- "Maximo"
names(descriptivos)[3] <- "Minimo"
names(descriptivos)[4] <- "Media"
names(descriptivos)[5] <- "Mediana"

View(descriptivos)

# Revision con la media acotada
# Median normal en la data
base_ciudad_media_Normal <- aggregate(VENTAS_DOLARES ~ data_year,
                                data = newdata,
                                FUN = mean)
# Media acotada
base_ciudad_media_Acotada <- aggregate(VENTAS_DOLARES ~ data_year,
                               data = newdata,
                               mean, trim = 0.10) #Trim quita el % descrito en las colas

# Uso de la libreria dplyr #
Resumen <- newdata %>%
  group_by(City,data_year) %>%
  summarise_each(funs(
    Media = mean(.),
    Maximo = max(.),
    Minimo = min(.)), 
    VENTAS_DOLARES) %>%
  arrange(desc(Media))




# Medidas de posicion y dispersion

# Quantiles 
edades <- c(22,32,55,44,13,33,36,48)
# Si 0.5 en Quantile, entonces mediana
quantile(edades, probs=c(0.5))


# Cuartiles 
# Se puede obtener los cuartiles
quantile(edades, probs=c(0.25,0.5,0.75,1))

# Deciles 
# Se puede obtener los Deciles
quantile(edades, probs=seq(0.1,1.0,0.1))

# Centiles 
# Se puede obtener los Centiles, Datos mas especificos
quantile(edades, probs=c(0.23,0.55,0.77))

newdata %>%
  mutate(data_year=year(Fecha))%>%
  group_by(City,data_year) %>%
  summarise_each(funs(
    Minimo = min(., na.rm = TRUE), # Que no considere valores faltantes 
    P_25 = quantile(., probs = 0.20),
    P_40 = quantile(., probs = 0.40),
    Mediana = quantile(., probs = 0.50),
    Media = mean(., na.rm = TRUE),
    Datos_Totales = n(),
    Valores_perdidos = sum(is.na(.)),
    Varianza=var(., na.rm = TRUE),
    Desviacion_Estandar = sd(., na.rm = TRUE),
    ),VENTAS_DOLARES) %>%
  View(title = "Resumen" )