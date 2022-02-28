# Caso Práctico de Logit ====

# Librerias ====
library(openxlsx)
library(gmodels)
library(ResourceSelection)
library(ROCR)
library(Epi)
library(QuantPsyc)
library(ggplot2)
library(memisc)

# Base de Datos ====
DB_1 <- read.xlsx("DB_2/laboral.xlsx")
attach(DB_1)

names(DB_1)
DB_2 <- DB_1[c("inlf","nwifeinc",
             "educ","exper","expersq",
             "age","kidsge6","kidslt6")]

# Descripción de las variables:

# inlf: Si una mujer participa o no en el Mercado Laboral
#      en algún momento en el tiempo
# nwifeinc: otras fuentes de ingresos
# educ: Años de educacion
# age: Edad
# exper: Años de experiencia
# expersq: Años de experiencia al cuadrado.
# kidslt6: Num hijos menos de 6 años
# kidsg6: Num hijos entre de 6 y 18 años



# Modelo logit ====

logit=glm(inlf ~., 
          family = binomial(link="logit"),
          data=DB_2)

summary(logit)

# Estimadores ====
## Bondad de ajuste ====

hl1 <- hoslem.test(#Datos Observados
                   DB_2$inlf,
                   # Datos Proyectados
                   fitted(logit),
                   # Número de grupos:
                   g=10)
# El número de grupos se basa en función de la cantidad
# de información que se tenga.

#Ho: Bondad de ajuste
#H1: No tiene bondad de ajuste

hl1
# Como p no es significativo, no se puede rechazar 
# la h0.

str(hl1)

cbind(hl1$observed,hl1$expected)


## Promedio del umbral ====
# Punto de corte, se saca mediante la media de la
# proyección, al no conocer el valor.
threshold <- mean(fitted(logit))
threshold

## Matriz de confusión ====

CrossTable(DB_2$inlf,
           fitted(logit)>threshold,
           expected = FALSE,
           prop.r = T,
           prop.c = T,
           prop.t = F,
           prop.chisq = F,
           chisq = F,
           fisher = F)

ClassLog(logit, # Modelo logit calculado
         DB_2$inlf, # Data
         cut=threshold) # Corte 
# Overall: 73% de los datos clasifican correctamente


# Evaluar capacidad predictiva mediante otros criterios ====
## Valores Predichos ====
pred <- prediction(logit$fitted.values,
                   DB_2$inlf)

## Performance ====
perf <- performance(pred,
                    measure = "tpr",
                    x.measure = "fpr")
# Curva ROC ====
plot(perf,
     colorize=T, 
     lty=3)

abline(0,
       1,
       col="black")

# prec =Tp/Tp+Fp
# rec = Tp/Tp+Fn

# Curva ROC con EPI ====
names(DB_2)
ROC(form= inlf~nwifeinc+educ+exper+expersq+
      age+kidsge6+kidslt6, 
    plot="ROC")

# PUNTO DE CORTE OPTIMO ====
# Sensitividad y Especificidad se intersectan

ROC(form= inlf~nwifeinc+educ+exper+expersq+
      age+kidsge6+kidslt6, 
    plot="sp")
