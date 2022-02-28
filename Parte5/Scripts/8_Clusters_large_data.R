# Algoritmo K-medoids ====
# No toma de manera aleatoria el centroide (Medoide)
# A diferencia del k-means

# Se utiliza mejor en muestras grandes, aunque tambien 
# se lo hace bien con pequenas
# minimiza la suma de disimilaridades (entre pares de puntos) en vez
# de una suma de distancias euclidianas 
# Medoide es el punto ubicado mas hacia el centro en cada grupo


# Librerias ====
library(openxlsx)
library(factoextra)
library(cluster)
library(gridExtra)

# Data ====
data <- read.xlsx("Data/Parte2/bancos.xlsx")
nombres <- data$BANCOS

base <- as.data.frame(scale(data[,-1]))
row.names(base) <- nombres


# Mediod ====
# Partitioning Around Medoids 
medoides <- pam(base, 2)

# Grafico del cluster ====
gm <- fviz_cluster(medoides,data=base)
gm

# Comparacion con k-means
kmedias <- kmeans(base,2)

par(mfrow=c(1,2))
gk <- fviz_cluster(kmedias,data=base)
gk

# Grafico comun
g1 <- suppressMessages(grid.arrange(gm, gk, 
                                    ncol = 2))

# Clara ====
# Clustering Large Applications
modeloClara <- clara(base, k=2, samples = 5)
modeloClara

gclara <- fviz_cluster(modeloClara,data=base)

# Grafico
g2 <- suppressMessages(grid.arrange(gclara,gm, gk, 
                                    ncol = 3))