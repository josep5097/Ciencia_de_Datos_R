# Clasificando ====

# Librerias ====
library(rpart)
library(rpart.plot)
library(caret)
library(foreign)
library(party)
library(partykit)
library(ROCR)
library(e1071)

# Data ====
datos <- read.spss(file ="Data/tree_credit.sav",
                   use.value.labels=FALSE, 
                   to.data.frame=TRUE)

attach(datos)
head(datos)
# Uso de semilla ====
set.seed(1234)
nombres <- c("buenpagador", 
             "edad",
             "ingresos",
             "tarjetas",
             "educacion",
             "creditovehiculo")

colnames(datos) <- nombres
colnames(datos)
attach(datos)

# Particion de los datos ====
# 70 - Entrenamiento
# 30 - Validacion
# Uso de la libreria caret para particionar

## Data Splitting functions ====
entrenamiento <- createDataPartition(datos$buenpagador,
                                     p=0.70,
                                     list=F)

## Recursive Partitioning and Regression Trees - rpart ====
m.entrenamiento <- rpart(buenpagador ~ 
                          edad + 
                          ordered(ingresos) +  
                          factor(tarjetas) +
                          factor(educacion)+ 
                          factor(creditovehiculo),
                          method="class",
                          data=datos[entrenamiento,])

m.entrenamiento

# Informacion Resumen====
# Displays CP table for Fitted Rpart Object - rpart
printcp(m.entrenamiento)
# xerror: error de validacion
# xstd: error de desviacion estandar

# Plot a Complexity Parameter Table for an Rpart Fit
plotcp(m.entrenamiento)  
# A menor CP - El arbol es mas frondoso, mayor coste

# Resumen
summary(m.entrenamiento)


# Graficando el arbol ====

plot(m.entrenamiento, 
     uniform=TRUE, 
     main="")

text(m.entrenamiento, 
     use.n=TRUE, 
     all=TRUE,
     cex=.8,
     pretty = 0)


## Guardando la imagen ====
png(file="mygraphic.png",
    width=840,
    height=750)

plot(m.entrenamiento, 
     uniform=TRUE, 
     main="")

text(m.entrenamiento, 
     use.n=TRUE, 
     all=TRUE,
     cex=.8,
     pretty = 0)

# Cerrar el graficador 
dev.off()


# Guardar Segundo Grafico ====
png(file="mygraphic2.png",
    width=840,
    height=550)

# Funcion para colocar el numero de casos en cada nodo
cuentacasos <- function(x, 
                        labs, 
                        digits, 
                        varlen)
                {paste(labs,
                       "\n\nn =", 
                       x$frame$n)}

rpart.plot(m.entrenamiento,
           main="Arbol de Clasficacion",
           shadow.col = "lightgreen",
           box.palette="Grays",
           cex = 0.8,
           tweak = 1,
           extra = 104,
           type = 2,
           under = F,
           fallen.leaves = T, 
           nn=T, 
           node.fun=cuentacasos)

dev.off()



# Podando el Arbol ====

# Se toma el valor del CP con min Xerror

# Prunes a tree (using leaves' labels)
poda <- prune(m.entrenamiento, 
              cp = 0.01373626)

# Plot an rpart model
prp(poda, 
    faclen = 0, 
    cex = 0.8, 
    extra = 1)

# Guardando la imagen 
png(file="mygraphic3.png",
    width=840,height=550)

rpart.plot(poda,
           main="Arbol de Clasficacion podado",
           shadow.col = "lightgreen",
           box.palette="Grays",
           cex = 0.75,
           tweak = 1,
           extra = 104,
           type = 2,
           under = F,
           fallen.leaves = T, 
           nn=T, 
           node.fun=cuentacasos)

dev.off()

# Evaluar el modelo ====
## Matriz de clasificacion para entrenamiento ====
ajustados <- predict(poda, 
                     datos[entrenamiento,], 
                     type="class")

# Se deben de encontrar en factor ambos datos.
confusionMatrix(ajustados,
                as.factor(datos$buenpagador[entrenamiento]))

table(datos[entrenamiento,]$buenpagador,
      ajustados,
      dnn = c("Actual","Predicho"))


# Con la data de validacion

ajustados <- predict(poda, 
                     datos[-entrenamiento,], 
                     type="class")

confusionMatrix(ajustados,
                as.factor(datos$buenpagador[-entrenamiento]))

# Curva ROC ====

# Se trabaja bajo el arbol podado
ajustados <- predict(poda, 
                     datos[-entrenamiento,], 
                     type="prob")

# Modelo de prediccion
pred <-prediction(ajustados[,2],
                  datos$buenpagador[-entrenamiento])
# Performance 
perf <- performance(pred,
                    measure = "tpr",
                    x.measure = "fpr")
# Plot de la curva ROC
plot(perf,
     colorize=T, 
     lty=3)

abline(0,1,col="black")

# Curvatura de sensitividad vs especificidad
plot(performance(pred, 
                 measure="sens", 
                 x.measure="spec"), 
     colorize=TRUE)

# Se requiere que la curvatura se debe de encontrar
# lo mas pegada a los margenes superior y de la derecha
# la caida sea suave.


# Curvatura frente a los datos de entrenamiento
ajustados2 <- predict(poda, 
                      datos[entrenamiento,], 
                      type="prob")

pred2 <-prediction(ajustados2[,2],
                   datos$buenpagador[entrenamiento])

perf2 <- performance(pred2,
                     measure = "tpr",
                     x.measure = "fpr")

plot(perf2,colorize=T, lty=3)
abline(0,1,col="black")

## Gragicando ambas curvas ====
par(mfrow=c(1,2))

plot(perf,colorize=T, lty=3)
abline(0,1,col="black")

plot(perf2,colorize=T, lty=3)
abline(0,1,col="black")


# Area bajo la curva fuera de la muestra
aucl <- performance(pred, 
                    measure = "auc")

aucl <- aucl@y.values[[1]]
aucl

# Area bajo la curva dentro de la muestra
aucl2 <- performance(pred2, 
                    measure = "auc")

aucl2 <- aucl2@y.values[[1]]
aucl2


# Prediccion para entrenamiento y validacion ====

# Reconocer que tanta relevancia tienen las variables en 
# el modelo.
poda$variable.importance

# Generando un valor fuera de la muestra
newdata = data.frame(edad=40,
                     ingresos=1, 
                     tarjetas=2,
                     educacion=2,
                     creditovehiculo=1)

# Predecir dentro de la muestra
predict(poda, 
        newdata = newdata) 
# La probabilidad de que no pague el nuevo cliente es de
# 91%.

# Generando el modelo con los datos completos
modelofinal <- rpart(buenpagador ~ 
                       edad + 
                       ordered(ingresos) +  
                       factor(tarjetas) +
                       factor(educacion)+ 
                       factor(creditovehiculo),
                    method="class",
                    data=datos)


predict(modelofinal, newdata)



# Obtener los elementos de un nodo en particular ====

# Pasemos de rpart a partykit 
# Para obtener el conjunto de datos de un nodo en particular

# Formato partykit
party.entrenamiento <- as.party(poda)
party.entrenamiento

# Ploteo del modelo con partykit
plot(party.entrenamiento)

# Obteniendo informacion de un nodo en especifico
nodoespecifico <- data_party(party.entrenamiento, 
                             id = 9)

head(nodoespecifico)