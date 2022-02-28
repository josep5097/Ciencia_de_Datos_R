# Remuestreo ====

# Librerias ====
library(foreign)
library(dplyr)
library(caret)
library(ROCR)
library(e1071) # Para SVM
library(reshape2)
library(pROC)
library(ROSE)

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

table(nuevadata$peso[entrenamiento])


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

# Data de entrenamiento
train_data <- nuevadata[entrenamiento, ]

# Oversample de la libreria ROSE ====
#OVERSAMPLE apunta a completar la informacion respecto
# de la clase que mas peso tiene versus la que menos tiene
table(nuevadata$peso[entrenamiento])
# Como la de mayor informacion es adecuados:
# 2078 * 2 = 4156

# Over-sampling, under-sampling, 
# combination of over- and under-sampling.
overs <- ovun.sample(peso ~.,
                     data = train_data,
                     method = "over",
                     N =4156 )$data
table(overs$peso)
# El oversample -> Clase menor se la lleva a la misma 
# cantidad de la clase mayor.

# Undersample
# El undersample lleva la clase que mas datos tiene
# a la clase que menos tiene
unders  <- ovun.sample(peso ~.,
                       data = train_data,
                       method = "under",
                       N =624 )$data
# Como el que menos datos tiene es no.adecuado
# 312*2=624

table(unders$peso)

# Rose ====
# Metodo sintetico para completar la informacion, mediante
# una estimacion condicional.
# Mediante densidad de kernels
# Se supone que es el metodo mas robusto.

roses  <- ROSE(peso ~.,
               data = train_data,
               seed = 1)$data

table(roses$peso)

# Comprobar el modelo para los desbalances ====
# Modelos con las tres tecnicas de remuestreo
# Modelo con overs
modelo.over <- tune(svm, 
                    peso ~ .,
                    data=overs,
                    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 50)),
                    kernel = "linear",
                    scale=T,
                    probability = TRUE)

summary(modelo.over)

mejor.modelo.over <- modelo.over$best.model

mejor.modelo.over

# Modelo con unders
modelo.under <- tune(svm, peso ~ .,
                     data=unders,
                     ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 50)),
                     kernel = "linear",
                     scale=T,
                     probability = TRUE)

summary(modelo.under)
mejor.modelo.under <- modelo.under$best.model
mejor.modelo.under

# Modelo SVM con roses
modelo.rose <- tune(svm, 
                    peso ~ .,
                    data=roses,
                    ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 50)),
                    kernel = "linear",
                    scale=T,
                    probability = TRUE)

summary(modelo.rose)
mejor.modelo.rose <- modelo.rose$best.model
mejor.modelo.rose

# Evaluacion de los modelos con data remuestreada ====
# Predicciones
ajustadosover <- predict(mejor.modelo.over,
                         overs, type="prob",probability=TRUE)

ajustadosunder <- predict(mejor.modelo.under,
                          unders, type="prob",probability=TRUE)

ajustadosrose <- predict(mejor.modelo.rose,
                         roses, type="prob",probability=TRUE)


#Con Caret
confusionMatrix(overs$peso,
                ajustadosover,
                dnn = c("Actuales", "Predichos"),
                levels(ajustadosover)[1])

confusionMatrix(unders$peso,
                ajustadosunder,
                dnn = c("Actuales", "Predichos"),
                levels(ajustadosunder)[1])

confusionMatrix(roses$peso,
                ajustadosrose,
                dnn = c("Actuales", "Predichos"),
                levels(ajustadosrose)[1])

confusionMatrix(ajustados.mejor.modelo,
                nuevadata$peso[entrenamiento],
                positive = levels(nuevadata$peso)[2])
# Mejor accuracy con el over


# Curvas ROC para los 4 modelos
predover <- prediction(attr(ajustadosover,
                            "probabilities")[,2],
                       overs$peso)


predrose <- prediction(attr(ajustadosrose,
                            "probabilities")[,2],
                       roses$peso)


roc.curve(overs$peso, attr(ajustadosover,
                           "probabilities")[,2],
          col="blue")

roc.curve(unders$peso, attr(ajustadosunder,
                            "probabilities")[,2], 
          col="green",
          add.roc = T)

roc.curve(roses$peso, attr(ajustadosrose,
                           "probabilities")[,2], 
          col="red",
          add.roc = T)

#Modelo original

roc.curve(nuevadata$peso[entrenamiento], attr(ajustados.mejor.modelo,
                                              "probabilities")[,2], 
          col="black",
          add.roc = T)

# Mejor modelo se obtiene con un over.