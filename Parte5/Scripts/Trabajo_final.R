# Trabajo Final Entregable

# Librerias ====
library(openxlsx)
library(cluster)
library(devtools)
library("factoextra")
library(dplyr)
library(GGally)
library(sjmisc)
library(NbClust)

# Data ====
DB <- read.xlsx("Data/Parte2/BOL_BP_MAY_ 2017.xlsx",sheet = "INDICADORES")

DB1 <- DB[4:98,2:33]
str(DB1)
attach(DB1)

DB2 <- filter(DB1,
              INDICADORES.FINANCIEROS == "ACTIVOS PRODUCTIVOS / TOTAL ACTIVOS"|
              INDICADORES.FINANCIEROS == "MOROSIDAD DE LA CARTERA TOTAL"|
              INDICADORES.FINANCIEROS == "GASTOS DE OPERACION  / MARGEN FINANCIERO"|
              INDICADORES.FINANCIEROS == "RESULTADOS DEL EJERCICIO / ACTIVO PROMEDIO"|
              INDICADORES.FINANCIEROS == "FONDOS DISPONIBLES / TOTAL DEPOSITOS A CORTO PLAZO")
nombreColumnas <- filter(DB1,
                         INDICADORES.FINANCIEROS == "NOMBRE DEL INDICADOR")
nombreBancos <- select(nombreColumnas,
                         -X32,-INDICADORES.FINANCIEROS)
nombreBancos <- rotate_df(nombreBancos)

DB3 <- select(DB2,-X32)
attach(DB3)
head(DB3)
DB3[,2:31] <- sapply(DB3[,2:31], as.numeric)
str(DB3)

R1 <- rotate_df(DB3[1,])
R2 <- rotate_df(DB3[2,])
R3 <- rotate_df(DB3[3,])
R4 <- rotate_df(DB3[4,])
R5 <- rotate_df(DB3[5,])

DB4 <- cbind(R1,R2,R3,R4,R5)
DB4names <- DB4[1,]
DB4 <- DB4[-1,]
colnames(DB4) <- DB4names


DB4[,] <- sapply(DB4[,], as.numeric)
str(DB4)

rownames(DB4) <- nombreBancos[,]

DB4[2:31,] <- sapply(DB4[2:31,], as.numeric)
str(DB4)

DB4 <- DB4[-31,]

# Se escala para mantener en una sola escala todas las variables
# Normalizarla
base <- as.data.frame(scale(DB4[,]))

# Para los clusters ====
## Con el metodo Ward.D y distancia Euclidiana ====
par(mfrow=c(1,1))
cluster1 <- hclust(dist(base, 
                       method = "euclidean"),
                  method = "ward.D")

plot(cluster1,
     hang = -0.01,
     cex=0.8)
# Dibujar un rectangulo rojo a los clusters del dendograma 
rect.hclust(cluster1, k=4, border="red")
rect.hclust(cluster1, k=6, border="blue")

## Con el metodo Average y distancia Euclidiana ====
par(mfrow=c(1,1))

cluster2 <- hclust(dist(base, 
                       method = "euclidean"),
                  method = "average")

plot(cluster2,
     hang = -0.01,
     cex=0.8)

# Dibujar un rectangulo rojo a los clusters del dendograma 
rect.hclust(cluster2, k=4, border="red")
rect.hclust(cluster2, k=6, border="blue")

## Con el metodo Ward.D y distancia Manhattan ====

par(mfrow=c(1,1))
cluster3 <- hclust(dist(base, 
                        method = "manhattan"),
                   method = "ward.D")

plot(cluster3,
     hang = -0.01,
     cex=0.8)

# Dibujar un rectangulo rojo a los clusters del dendograma 
rect.hclust(cluster3, k=4, border="red")
rect.hclust(cluster3, k=6, border="blue")

## Con el metodo Average y distancia Manhattan ====
par(mfrow=c(1,1))

cluster4 <- hclust(dist(base, 
                        method = "manhattan"),
                   method = "average")

plot(cluster4,
     hang = -0.01,
     cex=0.8)

# Dibujar un rectangulo rojo a los clusters del dendograma 
rect.hclust(cluster2, k=4, border="red")
rect.hclust(cluster2, k=6, border="blue")


#compactar en 2 graficos - Distancia Euclidiana
par(mfrow=c(1,2))
plot(cluster1,hang = -0.01,cex=0.8);plot(cluster2,hang = -0.01,cex=0.8)

#compactar en 2 graficos - Distancia Manhattan
par(mfrow=c(1,2))
plot(cluster3,hang = -0.01,cex=0.8);plot(cluster4,hang = -0.01,cex=0.8)


# Obtener los grupos en dataframe
grupos <- as.data.frame(cutree(cluster,k=4))

# Empleando la libreria cluster ====
ncluster <- diana(base,
                  metric = "euclidean")
ncluster
par(mfrow=c(1,2))
plot(ncluster)
# Divisive Coefficient >0.6







# Cluster No jerarquico =====

# Algoritmo kmeans
cnj <- kmeans(base,4)
cnj

# Media de los clusters
aggregate(base,
          by=list(cnj$cluster),
          FUN=mean)

# Visualizando el cluster
fviz_cluster(cnj,
             data=base)

# Visualizacion 2
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
