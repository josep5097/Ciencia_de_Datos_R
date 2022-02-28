# Librerias ====
library(forecast)
library(urca)
library(openxlsx)
# Data ====
data <- read.xlsx("Data/base.xlsx")
exportaciones <- data[,4]
plot(exportaciones,type="l")

# Pasando a Serie de Tiempo
tsexportaciones <- ts(exportaciones,
                      start=c(2006,1),
                      frequency = 12)

plot(tsexportaciones)

# Contrastes ====
adftest <- ur.df(tsexportaciones,
                 type=c("trend"),
                 selectlags = c("BIC"))
summary(adftest)

#Ho: Raiz Unitaria
#H1: no raiz unitaria

pptest <- ur.pp(tsexportaciones,
                type=c("Z-tau"),
                model=c("trend"),
                lags=c("short"))

summary(pptest)

#Ho: raiz unitaria
#H1: no raiz unitaria



kpsstest <- ur.kpss(tsexportaciones,
                    type=c("tau"),
                    lags=c("short"))

summary(kpsstest)

#Ho: estacionariedad
#h1: no estacionariedad

erstest <- ur.ers(tsexportaciones,
                  type=c("DF-GLS"),
                  model=c("trend"),
                  lag.max = 4)
summary(erstest)

#Ho: Raiz Unitaria
#H1: No raiz unitaria

zatest <- ur.za(tsexportaciones,
                model=c("both"))
summary(zatest)

#Ho: raiz unitaria
#H1: no raiz unitaria






