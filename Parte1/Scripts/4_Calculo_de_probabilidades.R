# Calculo de probabilidades ====
# Conjunta, condicional y marginal

# Librerias ====
library(gmodels)
library(dplyr)
library(openxlsx)

Informacion <- read.xlsx("Data/DEP_2016_BP.xlsx",
                         detectDates = T)

table(Informacion$TIPO.DE.DEPOSITO)

# Tabla de Resumen
Resumen <- Informacion %>%
            filter(FECHA == "2016-01-31",
                   (
                   TIPO.DE.DEPOSITO == "De 1 a 30 días"|
                   TIPO.DE.DEPOSITO == "De 31 a 90 días"|
                   TIPO.DE.DEPOSITO == "De 91 a 180 días"|
                   TIPO.DE.DEPOSITO == "De 181 a 360 días"
                   )
            )

table(Resumen$TIPO.DE.DEPOSITO)

# Construir la tabla cruzada o de contingencia
ct <- CrossTable(Resumen$TIPO.DE.DEPOSITO,
                 Resumen$REGION, 
                 prop.t = F,
                 prop.r = T,
                 chisq = F,
                 UseNA="no"
                 )

# Probabilidad Marginal: Posibles/Totales
# Probabilidad Conjunta: Explica dos fen?menos a la vez
# Probabilidad Condicional: Probabilidad de que suceda 
#                           un evento dado que otro ya sucedido.
#                           Hay prob cond de col y filas.