# Clasificacion y Segmentacion mediante Clusters ====
# Cluster Jerarquico ====
# Es un analisis de independencia.

# Librerias ====
library(openxlsx)
library(cluster)
library(devtools)
library("factoextra")

# Data ====
data <- read.xlsx("Data/Parte2/bancos.xlsx")
# Se trata de ratios financieros

# Adquirir el nombre de los bancos
nombres <- data$BANCOS

# Se escala para mantener en una sola escala todas las variables
# Normalizarla
base <- as.data.frame(scale(data[,-1]))
# Asignarle el nombre de los bancos a las filas
row.names(base) <- nombres


# Para los clusters ====
# Se especifica la distancia:
#   Euclidina, maximun, manhattan, canverra, binaria
# y el metodo de aglomeracion:
# M.ward.D, ward.D^2,completo, average, single

# Formar el cluster ====
# Con el metodo Ward.D
cluster <- hclust(dist(base, 
                       method = "euclidean"),
                       method = "ward.D")

plot(cluster,
     hang = -0.01,
     cex=0.8)


# Con el metodo Average
cluster2 <- hclust(dist(base, 
                        method = "euclidean"),
                        method = "average")


plot(cluster2,
     hang = -0.01,
     cex=0.8)

# Nota:
# Los elementos importantes siguen resaltando sin importar
# el metodo seleccionado.

#compactar en 2 graficos
par(mfrow=c(1,2))
plot(cluster,hang = -0.01,cex=0.8);plot(cluster2,hang = -0.01,cex=0.8)

# La distancia entre los elementos:
distancia <- dist(base,method = "euclidean")
distancia

# El grupo al que pertenecen los elementos
cluster$merge

# Cortes ====
# Realizar 4 cortes al dendograma
cutree(cluster, k=4)

par(mfrow=c(1,1))
plot(cluster,
     hang = -0.01,
     cex=0.8)

# Dibujar un rectangulo rojo a los clusters del dendograma 
rect.hclust(cluster, k=4, border="red")

# Obtener los grupos en dataframe
grupos <- as.data.frame(cutree(cluster,k=4))

# Empleando la libreria cluster ====
ncluster <- diana(base,
                  metric = "euclidean")
ncluster
par(mfrow=c(1,2))
plot(ncluster)
# Divisive Coefficient >0.6


par(mfrow=c(1,1))
plot(cluster)

ncluster$dc

# Cluster jerarquico computado y con corte en 4 clusters
res <- hcut(base, 
            k = 4, 
            stand = TRUE, 
            hc_metric = "euclidean",
            hc_method = "ward.D")

# Visualizar
fviz_dend(res, 
          rect = TRUE, 
          cex = 0.5,
          k_colors = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"))