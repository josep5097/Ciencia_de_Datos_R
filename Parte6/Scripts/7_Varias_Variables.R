#Librerias ====
library(openxlsx)
library(psych)
library(e1071)
library(ggplot2)
# Data ====
data <- read.xlsx("Data/variables_macro3.xlsx")

data2 <- data[,-1]

# Calculo de los factores
factores <- factanal(data2, 
                     factor=6,
                     scores = "regression")

# Adjuntar con los scores
datafinal <- data.frame(cbind(data[,1], factores$scores))

names(datafinal)[1] <- "PIB"
# La columna 1, es lo que se desea predecir, en este caso el
# PIB, y los nuevas variables son los factores obtenidos.

modelo.pib <-  tune(svm,
                    PIB ~.,
                    data=datafinal,
                    ranges = list(cost=c(0.001,0.01,0.1,1,5,10,50)),
                    kernel="linear")

summary(modelo.pib)



ggplot(data=modelo.pib$performances,
       aes(x=cost,y=error))+
       geom_line()+
       geom_point()+
       labs(title="Error de validacion vs hiperparametro C")+
       theme_bw()+
       theme(plot.title = element_text(hjust = 0.5))


mejor.modelo.pib <- modelo.pib$best.model
mejor.modelo.pib


# Pronosticando
# Dentro de la muestra
newdatapib <- data.frame(head(factores$scores,5))

# Predicciones
pronosticopib <-predict(mejor.modelo.pib,
                        newdatapib)

# Ploteo
plot(pronosticopib,
     type="l")

plot(c(datafinal[,1],
       pronosticopib),
     type="l")
