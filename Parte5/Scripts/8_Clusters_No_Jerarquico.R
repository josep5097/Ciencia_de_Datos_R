# Cluster No Jerarquico
# Allgoritmo de k-mena

# Librerias ====
library(openxlsx)
library(factoextra)
library(NbClust)

# Data ====
data <- read.xlsx("Data/Parte2/bancos.xlsx")
nombres <- data$BANCOS

# Normalizacion
base <- as.data.frame(scale(data[,-1]))
row.names(base) <- nombres

# Algoritmo kmeans
cnj <- kmeans(base,4)
cnj

# Media de los clusters
aggregate(base,
          by=list(cnj$cluster),
          FUN=mean)

cnj$centers

# Visualizando el cluster
fviz_cluster(cnj,
             data=base)

# Visualizacion 2
require(cluster)
clusplot(base,
         cnj$cluster, 
         color=TRUE, shade=TRUE, 
         labels=2, lines=2)

# Numero de Clusters optimos
# Mediante la libreria NbClust
clustersoptimos <- NbClust(base,
                           distance = "euclidean",
                           min.nc = 2,
                           max.nc = 12,
                           method="ward.D",
                           index = "all")

# Cluster optimo = 2
fviz_nbclust(clustersoptimos)

# Realizando el algoritmo mediante la cantidad optima
cnj <- kmeans(base,2)

# Plot silhouette
silueta <- silhouette(cnj$cluster,
                      dist(base,
                           method="euclidean")
                      )

fviz_silhouette(silueta)
# Con valores negativos se sugiere comprobar con un
# numero mayor de cluster, revisar valores atipicos,
# Revisar que las variables sean las correctas,
# Cambiar el enfoque.