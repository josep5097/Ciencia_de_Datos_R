# Ejercicio Integrador ====
# Librerias ====
library(foreign)
library(gmodels)
library(ResourceSelection)
library(ROCR)
library(Epi)
library(QuantPsyc)
library(ggplot2)
library(memisc)
library(reshape2)
library(gridExtra)
library(plotly)

# Data ====
DB_1 <- read.csv(file="Data/germancredit.csv",
               header = T)
attach(DB_1)

## Variables ====
# Default: Dicotomica - 1 mal pagador
# Duration: Plazo de la operacion
# Amount: Monto de la operacion
# Installment: Cuotas pagadas
# Age
# Edad al cuadrado,
# Cards: Numero de tarjetas de credito

names(DB_1)

DB_2 <- DB_1[c("Default",
               "duration",
               "amount",
               "installment",
               "age",
               "cards"
               )]
attach(DB_2)
# Generando la variable de edad al cuadrado
DB_2 <- mutate(DB_2, 
               xage = DB_2$age*DB_2$age
               )
attach(DB_2)

# Modelos Logit y Probit ====
logit <- glm(Default ~ .,
             family = binomial(logit), 
             data = DB_2)

probit <- glm(Default ~ .,
              family = binomial(probit), 
              data = DB_2)

# Contruir la tabla de ambos modelos ====
memisc::mtable(logit,
               probit,
               digits = 6,
               sdigits = 3)

# Bondad de Ajuste ====
## Para Logit ====
hl1 <- hoslem.test(DB_2$Default, 
                   fitted(logit), g=10)

## Para Probit ====
hl2 <- hoslem.test(DB_2$Default, 
                   fitted(probit), g=10)
hl1
hl2
#Ho: Bondad de Ajuste
#H1: no bondad de ajuste
# Debido a que los valores p en ambos casos de la prueba
# de Hosmer and Lemeshow, no son significativos, no se pueden
# negar la hipotesis nula, en este caso, en ambos modelos
# poseen una bondad de ajuste.

# Matriz de clasificacion ====
thresholdl<-mean(fitted(logit))
thresholdp<-mean(fitted(probit))
thresholdl
thresholdp
# Se tienen que los valores de threshold son semejantes.

## Tabla de clasificacion ====
ClassLog(logit,DB_2$Default,cut = thresholdl)
ClassLog(probit,DB_2$Default,cut = thresholdp)
# La tabla de clasificacion nos ofrece informacion de que 
# en ambos casos, el modelo no nos devuelve un valor alto 
# de aciertos.
# Los aciertos generales en el modelo nos brinda:
#   * 61.2% en logit
#   * 60.9 % en probit

# Capacidad Predictiva ====
predl <- prediction(logit$fitted.values, DB_2$Default)
predp <- prediction(probit$fitted.values, DB_2$Default)

perfl <- performance(predl, 
                     measure = "tpr", 
                     x.measure = "fpr")

perfp <- performance(predp, 
                     measure = "tpr", 
                     x.measure = "fpr")

plot(perfl, colorize=T,lty=3)
abline(0,1,col="black")

plot(perfp, colorize=T,lty=3)
abline(0,1,col="black")

# Area bajo la curva
aucl <- performance(predl, measure = "auc")
aucl <- aucl@y.values[[1]]
aucl

aucp <- performance(predp, measure = "auc")
aucp <- aucp@y.values[[1]]
aucp

# ROC con EPI ====
# ROC para el logit
ROC(form=Default ~ duration + amount + installment+
      age + xage + cards,
    plot="ROC")

# Corte optimo
ROC(form=Default ~ duration + amount + installment+
      age + xage + cards,
    plot="sp")

# Probit ROC ====
perf1 <- performance(predp,
                     "sens",
                     "spec")

sen1 <- slot(perf1,"y.values")[[1]]
esp1 <- slot(perf1,"x.values")[[1]]
alp1 <- slot(perf1,"alpha.values")[[1]]

pro_mat <- data.frame(alp1,sen1,esp1)

#logit
perf2 <- performance(predl,
                     "sens",
                     "spec")

sen2 <- slot(perf2,"y.values")[[1]]
esp2 <- slot(perf2,"x.values")[[1]]
alp2 <- slot(perf2,"alpha.values")[[1]]

log_mat <- data.frame(alp2,sen2,esp2)

# Melt para ggplotly
m1 <- melt(pro_mat,id=c("alp1"))
m2<- melt(log_mat,id=c("alp2"))

# Plots
p1 <- ggplot(m1,aes(alp1,value,
                    group=variable,
                    colour=variable))+
  geom_line(size=1.2)+
  labs(title=" Punto de corte para probit")

p1

p2 <- ggplot(m2,aes(alp2,value,
                    group=variable,
                    colour=variable))+
  geom_line(size=1.2)+
  labs(title=" Punto de corte para logit")

p2

# Graficos
ggplotly(p1)
ggplotly(p2)

g1 <- grid.arrange

# Proyectando la probabilidad ====

names(DB_2)

newdata <- data.frame(female=1,
                      xage=19,
                      linc=8.95)

predict(logit,newdata,type = "response")
predict(probit,newdata,type = "response")

