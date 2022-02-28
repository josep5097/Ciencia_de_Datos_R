# PCA ====

# Librerias ====
library(openxlsx)
library(corrplot)
library(factoextra)
library(psych)
library(pls) # Ajustar el modelo lineal con los valores del PCA
# Data ====
data <- read.xlsx("Data/variables_macro.xlsx")

# Matriz de Correlacion ====
corD <- cor(data)
## Grafico de Correlacion ====
corrplot(corD,
         type = "lower",
         method = "pie",
         tl.cex = 0.7,
         tl.srt = 50 )
## KMO ====
KMO(corD)
# Con un valor de MSA = 0.78 > 0.60, es valido realizarlo

# Modelo ACP ====
macp1 <- princomp(data,
                  cor = T,
                  scores = T)
summary(macp1)
# Con 5 o 6 componentes principales se puede compactar el modelo

## Screeplot ====
screeplot(macp1,type = "l")
abline(h=1)
# Hasta el comp6

## Analisis de las cargas ====
loadings(macp1)

## Biplot ====
biplot(macp1)
# No se tiene mucho aporte

# Almacenar en un dataframe
scoresM <- data.frame(macp1$scores)

p1 <- fviz_eig(macp1,
               choice = "eigenvalue",
               addlabels = T,
               hjust = -0.3)
p1

nuevaData <- scoresM[,1:6]

