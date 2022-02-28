# SVM ====

# Librerias ====
library(foreign)
library(dplyr)
library(caret)
library(ROCR)
library(e1071) # Para SVM
library(reshape2)
library(pROC)

# Data ====
datos <- read.spss("Data/ENV_2017.sav",
                   use.value.labels = F,
                   to.data.frame = T)

# Depuracion
table(datos$prov_nac)
str(datos$prov_nac)


datos$prov_nac <- as.numeric(as.character(datos$prov_nac))
datos$con_pren <- as.numeric(datos$con_pren)
datos$edad_mad <- as.numeric(datos$edad_mad)


nuevadata <- datos %>% filter(prov_nac==17)%>%
                        select(peso,
                               talla,
                               sem_gest,
                               sexo,
                               edad_mad,
                               sabe_leer,
                               con_pren)%>%
                        filter(
                          peso!=99,
                          talla!=99,
                          sem_gest!=99,
                          con_pren!=99,
                          sabe_leer!=9)%>%
                        mutate(peso=if_else(peso>2500,1,0),
                               sexo=if_else(sexo==1,0,1),
                               sabe_leer=if_else(sabe_leer==1,1,0),
                               con_pren=if_else(con_pren>=7,1,0),
                               edad2=edad_mad^2)


str(nuevadata$peso)

nuevadata$peso <- factor(nuevadata$peso)
str(nuevadata$peso)

nuevadata <- nuevadata %>%
              mutate(peso=recode_factor(
                peso,
                `0`="no.adecuado",
                `1`="adecuado"))

table(nuevadata$peso)


# Proceso ====
# Semilla ====
set.seed(1234)

#crear una muestra de entrenamiento
entrenamiento <- createDataPartition(nuevadata$peso,
                                     p=0.05,
                                     list=F)

# Modelo SVM ====
modelo <- svm(peso ~
                talla+
                sem_gest+
                sexo+
                edad_mad+
                edad2+
                sabe_leer,
              data=nuevadata[entrenamiento,],
              kernel="linear",
              cost=10,
              scale=T,
              probability=TRUE)

# Vectores de soporte
modelo$index

# Termino independiente
modelo$rho

# Coeficientes empleados para multiplicar cada observacion
# Obteniendo un vector perpendicular
modelo$coefs

# Evaluar el modelo ====
# Prediccion con la muesta entrenamiento
ajustados <- predict(modelo,
                     nuevadata[entrenamiento,],
                     type="prob")

# Punto de corte por defecto es 0.5

# Matriz de confusion
ct <- table(nuevadata[entrenamiento,]$peso,
            ajustados,
            dnn=c("Actual","Predicho"))

diag(prop.table(ct,1))
sum(diag(prop.table(ct)))

# Matriz de confusion y estadisticas con libreria ROCR 
confusionMatrix(nuevadata$peso[entrenamiento],
                ajustados,
                dnn=c("Actual", "Predicho"),
                levels(ajustados)[2])

# Ploteo del SVM
plot(modelo,
     data=nuevadata[entrenamiento,],
     talla ~ sem_gest)


# Optimizacion de los hiperparametros
# Mediante la validacion cruzada
# tune de libreria e1071
modelo.tuneado <-  tune(svm,
                        peso ~.,
                        data=nuevadata[entrenamiento,],
                        ranges = list(cost=c(0.001,0.01,0.1,1,5,10,50)),
                        kernel="linear",
                        scale=T,
                        probability=T)


summary(modelo.tuneado)
# El costo final sera de 1

# Analizarlo con grafico
# Se observara el desempeño del modelo
ggplot(data=modelo.tuneado$performances,
       aes(x=cost,y=error))+
       geom_line()+
       geom_point()+
       labs(title="Error de validacion vs hipeparametro C")+
       theme_bw()+
       theme(plot.title = element_text(hjust = 0.5))


mejor.modelo <- modelo.tuneado$best.model
summary(mejor.modelo)

#vectores de soporte
head(mejor.modelo$index,100)

# Ploteo del mejor modelo
plot(mejor.modelo,
     data=nuevadata[entrenamiento,],
     talla~sem_gest)
# Se observa que no es tan lineal el modelo

# Validacion del mejor modelo ====
ajustados.mejor.modelo <- predict(mejor.modelo,
                                  nuevadata[entrenamiento,],
                                  type="prob",
                                  probability = T)

# Verificar como se capturan las probabilidades 
str(ajustados.mejor.modelo)
head(attr(ajustados.mejor.modelo,"probabilities"),5)


# Matriz de confusion - Clasificacion
levels(ajustados.mejor.modelo)
# La primera categoria: Adecuado / No adecuado

# Se trabaja a partir del atributo
table(attr(ajustados.mejor.modelo,"probabilities")[,1]>0.5,
      nuevadata$peso[entrenamiento])

# Matriz de confusion
confusionMatrix(ajustados.mejor.modelo,
                nuevadata$peso[entrenamiento],
                positive = levels(nuevadata$peso)[2])
# La clase objetivo es adecuado

# Curvas ROC ====
pred <- prediction(attr(ajustados.mejor.modelo,
                        "probabilities")[,2],
                   nuevadata$peso[entrenamiento])

perf <- performance(pred,
                    "tpr",
                    "fpr")
plot(perf,
     colorize=T,
     lty=3)
abline(0,1,col="black")

## Area bajo la curva ====
aucmodelo1 <- performance(pred,measure = "auc")
aucmodelo1 <- aucmodelo1@y.values[[1]]
aucmodelo1

## Sensitividad y especificidad ====
plot(performance(pred,
                 measure = "sens",
                 x.measure = "spec",
                 colorize=T))

# Punto de corte optimo ====
perf1 <- performance(pred, "sens","spec") 
sen <- slot(perf1,"y.values"[[1]])
esp <- slot(perf1,"x.values"[[1]])
alf <- slot(perf1,"alpha.values"[[1]])

mat <- data.frame(alf,sen,esp)

names(mat)[1] <- "alf"
names(mat)[2] <- "sen"
names(mat)[3] <- "esp"

# Melt para graficar con ggplot
m <- melt(mat,id=c("alf"))

p1 <- ggplot(m,
             aes(alf,value,group=variable,
                 colour=variable))+
             geom_line(size=1.2)+
             labs(title="Punto de corte optimo para SVM",
                  x="cut - off",
                  y="")

p1

# Determinar el cut-off que maximiza el accuracy
# Con el performance se mide el acc
max.accuracy <- performance(pred,
                            measure = "acc")
plot(max.accuracy)

# Determinar los valores internos 
# Determinar el indice maximo
indice <- which.max(slot(max.accuracy,
                         "y.values")[[1]])
# Determinar el Accuracy Maximo y el Maximo Cut-off
acc <- slot(max.accuracy,
            "y.values")[[1]][indice]
cutoff <- slot(max.accuracy,
               "x.values")[[1]][indice]

print(c(accuracy=acc,
        cutoff=cutoff))
# Se tiene que el maximo accuracy es de 93%
# mientras que el cut-off es 0.4587

## Mediante la libreria pROC ====

prediccionescutoff <- attr(ajustados.mejor.modelo,
                           "probabilities")[,1]

curvaroc <- plot.roc(nuevadata$peso[entrenamiento],
                     as.vector(prediccionescutoff),
                     precent=TRUE,
                     ci=TRUE,
                     print.auc=TRUE,
                     threholds="best",
                     print.thres="best")

# Prediciendo con SVM ====
newdata <- head(nuevadata,5)
str(newdata)

# Predecir dentro de la muestra
predict(mejor.modelo,newdata)
pronostico1 <- predict(mejor.modelo,newdata)

# Obtener las probabilidades
p.probabilidades <- predict(mejor.modelo,
                            newdata,
                            probability = TRUE)
p.probabilidades


# Pronostico fuera de la muestra ====
names(newdata)

newdata2 <- data.frame(talla=45,
                       sem_gest=38,
                       sexo=1,
                       edad_mad=30,
                       sabe_leer=1,
                       con_pren=1,
                       edad2=900)

pronostico2 <- predict(mejor.modelo,
                       newdata2, 
                       probability = T)
pronostico2

predict(mejor.modelo,
        newdata2)

# Punto de Corte Sugerido ====
umbral <- as.numeric(cutoff)

table(attr(ajustados.mejor.modelo,
           "probabilities")[,1]>umbral,
      nuevadata$peso[entrenamiento])

## Probabilidades devueltas
head(attr(ajustados.mejor.modelo,
          "probabilities"))

# Probabilidad objetivo ====
prediccionescutoff <- attr(ajustados.mejor.modelo,
                           "probabilities")[,1]

str(prediccionescutoff)


prediccionescutoff <- as.numeric(prediccionescutoff)

# Corte predecido
predCut <- factor(ifelse(prediccionescutoff>umbral,1,0))

# Matriz de corte
matrizpuntocorte <- data.frame(real=nuevadata$peso[entrenamiento],
                               predicho=predCut)
# Estableciendo los mismos parametros
matrizpuntocorte <- matrizpuntocorte %>% 
                    mutate(predicho=recode_factor(predicho,
                                                  `0`="no.adecuado",
                                                  `1`="adecuado"))
# Matriz de Confusion
confusionMatrix(matrizpuntocorte$predicho,
                matrizpuntocorte$real,
                positive = "adecuado")