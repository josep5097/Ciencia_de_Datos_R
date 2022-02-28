# Reduccion de Dimensiones ====
# Analisis Factorial ====

# Librerias ====
library(openxlsx)
library(corrplot)
library(factoextra)
library(GPArotation)
library(psych)

# Data ====
DB <- read.xlsx("Data/variables_macro2.xlsx")

# Antes de hacer un AF, se debe de realizar 
# un ACP, para poder conocer cuantos componentes
# se debe de retener

# ACP ====
macp1 <- princomp(DB,
                  cor = T,
                  scores = T)
summary(macp1)

# Scree Plot ====
p1 <- fviz_eig(macp1,
               choice = "eigenvalue",
               addlabels = T,
               hjust = -0.3)
p1

# AF ====
# Se realiza un Analisis Factorial con 6 componentes

maf1 <- factanal(DB, 
                 factor=6,
                 rotation="none")
maf1
maf1$loadings

# Si la base de Datos se encuentra correlacionada
# Se debe de realizar una rotacion 

# Con rotacion oblimin
maf2 <- fa(r=DB,
           nfactors = 6,
           rotate = "oblimin")


maf2
loadings(maf2)
maf2$scores[,1]

# Plot del Score
plot(maf2$scores[,1],
     type="l")
abline(h=0)

# plot de barras de los pesos de la componente MR1
barplot(maf2$weights[,"MR1"],
        las=2,
        cex.names = 0.6)
