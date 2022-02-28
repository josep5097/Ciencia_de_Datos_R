# Comparando Modelo Logit y Probit

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

# Data =====
db <- read.dta(file="Data/randdata.dta")
attach(db)

DB_1 <- db[year==2,]
attach(DB_1)

DB_1 <- DB_1[complete.cases(educdec), ]
summary(DB_1$educdec)


# Modelo probit, logit y MLP ====

DB_2 <- DB_1[c("binexp","female","xage","linc")]

logit <- glm(binexp ~ .,
              family = binomial(logit), 
              data = DB_2)

probit <- glm(binexp ~ .,
             family = binomial(probit), 
             data = DB_2)

# Contruir la tabla de ambos modelos ====
memisc::mtable(logit,
               probit,
               digits = 6,# Numero de decimales para coeficientes
               sdigits = 3) # Numero de decimales en los criterios

# Bondad de Ajuste ====
## Para Logit ====
hl1 <- hoslem.test(data$binexp, 
                   fitted(logit), g=20)

## Para Probit ====
hl2 <- hoslem.test(data$binexp, 
                   fitted(probit), g=20)
hl1
hl2
#Ho: Bondad de Ajuste
#H1: no bondad de ajuste
# No existe una buena bondad de ajuste con g = 10 y g = 20
# Se rechaza estadisticamente la H0, con lo cual no existe
# bondad de ajuste

# Matriz de clasificacion ====
# Promedio de valores proyectados
thresholdl<-mean(fitted(logit))
thresholdp<-mean(fitted(probit))
thresholdl
thresholdp

## Tabla de clasificacion ====
ClassLog(logit,data$binexp,cut = thresholdl)
ClassLog(probit,data$binexp,cut = thresholdp)

# Capacidad Predictiva ====
predl <- prediction(logit$fitted.values, DB_2$binexp)
predp <- prediction(probit$fitted.values, DB_2$binexp)

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
ROC(form=binexp ~ female + xage + linc,
    plot="ROC")
# Corte optimo
ROC(form=binexp ~ female + xage + 
      linc,
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