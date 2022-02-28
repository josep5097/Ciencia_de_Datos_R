# Analisis de componentes principales ====
# Reducir un numero de variables que logren explicar un porcentaje
# de varianza para un modelamiento.

## Consideraciones ====
# Base de datos completa, sin datos faltantes
# Variables x, se encuentren relacionadas entre las variables

# Librerias ====
library(openxlsx)
library(corrplot)
library(psych) # KMO
library(factoextra)

# Data ====
DB_0 <- read.xlsx("Data/vab_industrias.xlsx")
DB_0 <- DB_0[,2:19]

# Analisis de Correlacion ====
Mat_cor <- cor(DB_0)
## Grafica de correlacion ====
corrplot(Mat_cor, 
         type = "lower",
         method ="pie",
         tl.cex = 0.8,
         bg = "gray100",
         tl.srt = 90,
         tl.offset = 0.5,
         title = "Correlación entre industrias"
)

# Calcular el KMO ====
# KMO al menos >0.60
KMO(r = DB_0)
# Overall MSA = 0.89. Se puede reducir el valor de
# variables 

# Analisis de Componentes Principales ====
macp1 <- princomp(DB_0, cor = T, scores = T)
summary(macp1)
# Comp.1 posee el 87% de la varianza de la base de datos
# Comp.2 posee el 6% de la varianza

## Cargas factoriales del ACP ====
# No se tiene una interpretacion directa.
loadings(x = macp1)

## Cuantas componentes quedarse ====
# Donde SD > 1
# Con dos componentes se va a obtener un 93% de la informacion

## Mediante Grafico ====
# Se grafica la varianza de las componentes
screeplot(macp1,type = "l")
abline(h = 1)

# Grafica mediante la libreria factoextra ====
p1 <- fviz_eig(X = macp1,
               choice = "variance", 
               hjust = -0.3,
               addlabels = T
               )
p1
p2 <- fviz_eig(X = macp1,
               choice = "eigenvalue", 
               hjust = -0.3,
               addlabels = T
               ,
               ggtheme = theme_minimal())+
      ylim(0,20)
      
p2
head(macp1$scores)
scoreM <- data.frame(macp1$scores)
scoreM