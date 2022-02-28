# Metodo Bugging ====

# Librerias ====
library(foreign)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)
library(foreign)
library(partykit)
library(ROCR)
library(e1071)
library(randomForest)
library(caret)
#install.packages("doMC", repos="http://R-Forge.R-project.org")
library(doMC)
library(ranger)

# RAMDOM FOREST es una tecnica de ML que utiliza en un lugar de un arbol
# un bosque completo de arboles.
# La idea es crear varios modelos similares pero independientes. 

# 4 criterios para construir un RAMDOM FOREST:

# 1.- mtry= numero de predictores obtenidos de forma aleatoria
# 2.- min.node.size= tamano minimo que debe tener un nodo en cada arbol para ser dividio
# 3.- splitrule=  criterio de division. Generalmente se usa GINI
# 4.- numero de arboles creados

# Data ====
datos <- read.spss("Data/ENV_2017.sav",
                   use.value.labels=TRUE, 
                   to.data.frame=TRUE)

# Manipulacion
datos$peso <-  as.numeric(datos$peso)


nuevadata <- datos %>% filter(prov_res=="Guayas")%>%
                      select(peso,
                             talla,
                             sem_gest,
                             sexo,
                             edad_mad,
                             sabe_leer,
                             con_pren,
                             est_civil)%>% 
                      filter(peso!=99,
                             talla!=99,
                             sem_gest!=99,
                             con_pren!=99,
                             est_civil!='Sin información')%>%
                      mutate(con_pren = as.numeric(con_pren),
                             edad_mad = as.numeric(edad_mad),
                             peso=if_else(peso > 2500, 1, 0),
                             sexo=if_else(sexo == "Hombre", 0, 1),
                             sabe_leer=if_else(sabe_leer=="Si", 1, 0),
                             con_pren=if_else(con_pren >= 7, 1, 0),
                             #est_civil=if_else(est_civil =="Casada",1,0),
                             edad2=edad_mad*edad_mad
                      )
str(nuevadata)
table(nuevadata$sabe_leer)
# Realizar una semilla aleatoria
set.seed(1234)

nuevadata$peso <-  factor(nuevadata$peso)


entrenamiento <- createDataPartition(nuevadata$peso,
                                     p=0.01,
                                     list=F)


# Estimando el modelo Bagging
modelo_bagging <- randomForest(peso ~ 
                                 talla+
                                 sem_gest + 
                                 sexo+
                                 edad_mad+
                                 edad2+
                                 sabe_leer+
                                 est_civil,
                               data=nuevadata[entrenamiento,], 
                               mtry = 7, 
                               ntree = 500,
                               importance = TRUE,
                               norm.votes = FALSE,
                               keep.forest=TRUE)

modelo_bagging
# OOB: Out of bounds - Debe de ser pequeno

randomForest::importance(modelo_bagging)
# Se observa que mayor importancia es la variable de Talla.

varImpPlot((modelo_bagging))

# Pronostico con bagging ====
newdata <- head(nuevadata, n=10)
predict(modelo.bagging, 
        newdata = newdata,
        predict.all = T)



# Optimizando Hiperparametros ====

# Numeros de Cores
numCores <- detectCores()
numCores
# doMC opera de forma paralela con los nucleos para 
# procesamiento rapido 

registerDoMC(cores = 4)

# Genrando el modelo de randomForest
particiones  <- 10
repeticiones <- 5

# Hiperparametros
hiperparametros <- expand.grid(mtry = c(2, 3, 4),
                               min.node.size = c(2, 3, 4, 5, 10, 15, 20, 30),
                               splitrule = "gini")

# Generando una semilla
set.seed(1234)
seeds <- vector(mode = "list", 
                length = (particiones * repeticiones) + 1)

for (i in 1:(particiones * repeticiones)) {
                  seeds[[i]] <- sample.int(1000, 
                                           nrow(hiperparametros)
                                           )
}

seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# Modelo de entrenamiento ====
# Usando el paquete Caret
control_train <- trainControl(method = "repeatedcv", 
                              number = particiones,
                              repeats = repeticiones, 
                              seeds = seeds,
                              returnResamp = "final", 
                              verboseIter = FALSE,
                              allowParallel = TRUE)

## Calculando el modelo ====

modelo_rf <- train(peso ~ 
                     talla+
                     sem_gest+
                     factor(sexo)+
                     edad_mad+edad2+
                     factor(sabe_leer)+
                     factor(est_civil),
                   data=nuevadata[entrenamiento,],
                   method = "ranger",
                   tuneGrid = hiperparametros,
                   metric = "Accuracy",
                   # Modelo de control
                   trControl = control_train,
                   # Numero de arboles ajustados
                   num.trees = 500)
modelo_rf
# Valor final
# mtry = 4, min.node.size = 10, splitrule = gini
modelo_rf$finalModel

ggplot(modelo_rf, 
       highlight = TRUE) +
  scale_x_continuous(breaks = 1:30) +
  labs(title = "Accuracy del Modelo Random Forest") +
  guides(color = guide_legend(title = "mtry"),
         shape = guide_legend(title = "mtry")) +
  theme_bw()



# Pasar de validacion cruzada a RandomForest ====

# Importancia
set.seed(1234)
modelo_randomforest <- randomForest(peso ~ 
                                      talla+
                                      sem_gest + 
                                      sexo+
                                      edad_mad+
                                      edad2+
                                      sabe_leer+
                                      est_civil,
                                    data=nuevadata[entrenamiento,], 
                                    mtry = 4, 
                                    ntree = 500,
                                    importance = TRUE, 
                                    nodesize = 10,
                                    norm.votes = TRUE,
                                    keep.forest=TRUE)
modelo_randomforest
# OBB: 4.09%

# Variable de importancia
varImpPlot(modelo_randomforest)

# Prediccion con el modelo RF
newdata <- head(nuevadata,n=10)
str(newdata)

# Predecir dentro de la muestra
fix(newdata)
predict(modelo_randomforest, 
        newdata ,
        predict.all = TRUE) 

