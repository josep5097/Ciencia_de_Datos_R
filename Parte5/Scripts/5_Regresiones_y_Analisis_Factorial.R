# Regresiones y Analisis Factorial ====

# Librerias ====
library(car)
library(openxlsx)
library(GGally)

# Data ====
DB <- read.xlsx("Data/REG_ACP.xlsx")
attach(DB)

# Estimar modelo ====
reg <- lm(INEX ~ CONS+GPER+GEX,
          data = DB)

summary(reg)
# Problema de multicolinealidad donde se tiene 
# P(t) no significativos y un R^2 alto

# Correlacion ====
cor(DB[3:5])
# Afecta la estimacion de los betas

# Grafico pairs
pairs(DB[3:5])
ggpairs(DB[3:5])

# Fator VIF
vif(reg)
# SI VIF >10 --> Multicolinealidad
# Categoricamente se confirma la multicolinealidad
# Atenuar el problema si es posible

# Atenuacion ====
# 1) Nada
# 2) Log
# 3) Dif
# 4) Ratios
# 5) ACP
# 6) Ampliando el tamano muestral

# ACP ====
## Data a analizar ====
DB1 <- cbind(DB[3:5])
## ACP ====
macp1 <- princomp(DB1,
                  scores = T,
                  cor = T)
macp1
loadings(macp1)

## Almacenamiento de los scores ====
# Estos elementos son ortogonales
# con lo cual se elimina la multicolinealidad

scoresModel <- data.frame(macp1$scores)

reg2 <- lm(INEX ~ scoresModel$Comp.1+
                  scoresModel$Comp.2+
                  scoresModel$Comp.3,
           data = DB)

summary(reg2)

vif(reg2)