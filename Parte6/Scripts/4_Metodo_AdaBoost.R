# Metodo AdaBoost ====

# Librerias ====
library(foreign)
library(dplyr)
library(caret)
library(ROCR)
library(e1071)
library(fastAdaboost)

# Data ====
datos <- read.spss("Data/ENV_2017.sav",
                   use.value.labels=TRUE, 
                   to.data.frame=TRUE)

# Manipulacion
datos$peso <-  as.numeric(datos$peso)
datos$talla <- as.numeric(datos$talla)
datos$sem_gest <-  as.numeric(datos$sem_gest)
datos$edad_mad <- as.numeric(datos$edad_mad)
datos$con_pren <-  as.numeric(datos$con_pren)

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

# Realizar una semilla aleatoria
set.seed(1234)

# Particion de datos ====
nuevadata$peso <- factor(nuevadata$peso)
str(nuevadata
    )
entrenamiento <- createDataPartition(nuevadata$peso,
                                     p = 0.10,
                                     list = F)

# Validacion Cruzada
control_train <- trainControl(method = "cv",
                              number = 10,
                              search = "grid",
                              allowParallel = T)

# Grid va a ser cruzada o validada
grid <- expand.grid(nIter = c(100,400,600),
                    method = "adaboost")


modelo_adaboost <- train(peso ~
                           talla+
                           sem_gest+
                           factor(sexo)+
                           edad_mad+
                           edad2+
                           factor(sabe_leer)+
                           factor(est_civil),
                         data=nuevadata[entrenamiento,],
                         method = "adaboost",
                         tuneGrid = grid,
                         # Modelo de control
                         trControl = control_train
                         )

modelo_adaboost

# Evaluar el modelo ====
## Dentro de la muestra ====
confusionMatrix(predict(modelo_adaboost,
                        nuevadata[entrenamiento,]),
                nuevadata$peso[entrenamiento])


# Predecir fuera de la muestra ====
newdata <- head(nuevadata, n = 10)

predict(modelo_adaboost, newdata, predict.all = T)

