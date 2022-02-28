# Modelo Probit ====

# Librerias ====
library(openxlsx)
library(foreign)
library(forecast)
library(gmodels)
library(lmtest)
library(ResourceSelection)
library(ROCR)
library(Epi)
library(QuantPsyc)
library(ggplot2)
library(reshape2)
library(gridExtra)

# Data ====
db <- read.xlsx("Data/laboral.xlsx")
attach(db)

DB_1 <- db[c("inlf","nwifeinc",
             "educ","exper","expersq",
             "age","kidsge6","kidslt6")]

attach(DB_1)

# Modelo Probit ====


probit <- glm(inlf ~.,
              family=binomial(link="probit"), 
          data = DB_1)
summary(probit)

## Bondad de Ajuste ====

hl1 <- hoslem.test(DB_1$inlf, 
                   fitted(probit), g=10)
hl1
#Ho: Bondad de Ajuste
#H1: No tiene bondad de ajuste

# Generar la matriz de reales y esperados
str(hl1)
cbind(hl1$observed,hl1$expected)

## Valor Umbral ====
threshold<-mean(fitted(probit))
threshold

## Matriz de confusion ====
CrossTable(DB_1$inlf, 
           fitted(probit) > threshold,
           expected=FALSE, 
           prop.r=TRUE, 
           prop.c=TRUE,
           prop.t=F, 
           prop.chisq=F, 
           chisq = FALSE, 
           fisher=FALSE)

## Tabla de clasificacion
ClassLog(probit,DB_1$inlf,cut = threshold)

# Criterios: Capacidad predictiva ====

pred <- prediction(probit$fitted.values, 
                   DB_1$inlf)

perf <- performance(pred, 
                    measure = "tpr", 
                    x.measure = "fpr") 

plot(perf, colorize=T,lty=3)

abline(0,1,col="black")

# Curva precision-recall ====
perf <- performance(pred, 
                    measure = "prec", 
                    x.measure = "rec") 
plot(perf, colorize=T,lty=3)

# Punto de Corte Optimo ====
# ROC se puede implementar unicamente con LOGIT
perf1 <- performance(pred,
                     "sens",# Sensitividad
                     "spec") # Especificidad
# Obteniendo los valores se sensitividad, Especificidad y el alpha
sen <- slot(perf1,"y.values"[[1]])
esp <- slot(perf1,"x.values"[[1]])
alpha_val <- slot(perf1,"alpha.values"[[1]])

## Matriz de valores ====
D_Matrix <- data.frame(alpha_val,sen,esp)
names(D_Matrix)[1] <- "alf"
names(D_Matrix)[2] <- "sen"
names(D_Matrix)[3] <- "esp"

## Melt
m <- melt(D_Matrix,id=c("alf"))

plot1 <- ggplot(m,
             aes(alf, 
                 value,
                 group=variable,
                 colour=variable)) + 
                 geom_line(size=1.2)+
                 labs(title="Punto de corte optimo PROBIT")

plot1
  
library(plotly)
ggplotly(plot1)
# Alpha = 0.5829