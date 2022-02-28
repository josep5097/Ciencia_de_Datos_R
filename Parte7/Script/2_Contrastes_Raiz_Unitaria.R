# Librerias ====
library(forecast)
library(urca)

# Data ====
data <- read.table("Data/consumo.txt")

ts.plot(data)

# Convirtiendo a serie de tiempo
tsdata <- ts(data/1000,
             start=c(2004,8),
             end=c(2014,8),
             frequency = 12)
# Ploteo de serie de tiempo
ts.plot(tsdata)

# Contrastes de Estacionariedad ====
## Augmented-Dickey-Fuller Unit Root Test ====

### Regla:
### Si el valor calculado es mayor que el valor
### critico se rechaza H0.

### H0: Raiz unitaria
### H1: No raiz unitaria
adftest <- ur.df(tsdata,
                 type=c("trend"),
                 selectlags = c("BIC"))
summary(adftest)
# Primer valor del test estadistico > que una valor tau3
# Como no se da el caso, no se tiene evidencia estadistica
# para rechazar la H0.

# Raiz unitaria = no estacionariedad
# Es necesario transformar la serie.


## Phillips & Perron Unit Root Test ====
pptest <- ur.pp(tsdata,
                type=c("Z-tau"),
                model=c("trend"),
                lags=c("short"))

summary(pptest)
### H0: Raiz unitaria
### H1: No raiz unitaria
# Si Z-tau > que un valor critico, se rechaza la H0.
# Como no es mayor, no se rechaza la H0.

## Contraste KPSS ====
kpsstest <- ur.kpss(tsdata,
                    type=c("tau"),
                    lag=c("short"))

summary(kpsstest)
### H0: Estacionariedad
### H1: No Estacionariedad
# Si el valor test-estadistico > que un valor critico
# Se rechaza la H0.
# Al 90% se puede rechazar la H0.
# Al 95% se puede rechazar la H0.
# Al 95% existe evidencia estadistica para rechazar la 
# H0, se establece que no existe estacionariedad.

## Elliott, Rothenberg & Stock Unit Root Test ====
erstest <- ur.ers(tsdata,
                  type=c("DF-GLS"),
                  model=c("trend"),
                  lag.max = 4)
summary(erstest)

### H0: Raiz unitaria
### H1: No raiz unitaria

# Como el valor del test estadistico no existe evidencia 
# estadistica para rechazar la H0.