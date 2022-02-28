# Analisis Discriminante para Clasificación
# Identifica a que grupo pertenece un individuo en funcion
# de las variables utilizadas.
# El AD, se extiende en las categorias, a diferencia de los 
# modelos de regresión lineal.

# y1 = a11X1 + ... + a1pXp
# ....
# ym = am1X1 + ... + axpXp
# donde m = min(q-1,p)

# Siendo:
# q: Grupos    y    p: Variables
# Se obtiene una puntuacion de los valores de y,
# que son combinaciones lineales de las x.
# La idea es discriminar lo maximo posible a los 
# grupos para maximizar el porcentaje de clasificacion

# Librerias ====
library(MASS)
library(gmodels)
library(ROCR)
library(Epi)
library(QuantPsyc)
library(ggplot2)
library(reshape2)
library(plotly)

# Data ====
DB <- read.csv("Data/Parte2/germancredit.csv")
head(DB)
str(DB)
attach(DB)

DB1 <- DB[, c("Default", "duration",
              "amount","installment",
              "age")]
attach(DB1)

# AD (Linear Discriminant Analysis)

ad <- lda(Default ~., data = DB1)
ad
# Se obtiene un coeficiente linear discriminante
# q-1. Como de tiene dos grupos: 1 LD

# El modelo indica que la probabilidad de que el 
# cliente pague aumenta de mayor forma con el installment

# Matriz de confusion ====
## crostable ====
CrossTable(predict(ad)$class, 
           Default, 
           prop.t = F, 
           prop.chisq = F)

### Construir el crosstable ====
ct <- table(predict(ad)$class, Default)
ct
### Diagonalizar ====
d1 <- diag(prop.table(ct,1))
# Porcentaje de clasificacion correcta
sum( diag(prop.table(ct)))


### Controlando la posterior ====
categoriaBase <- predict(ad)$posterior[,2]
categoriaBase

### Definiendo el umbral
umbral <- mean(categoriaBase)
umbral
# El umbral para que suceda 1 es 0.299
# Este valor es el que se debe de emplear para iniciar

## Crosstable con el nuevo umbral
CrossTable(DB1$Default,
           categoriaBase>umbral, prop.t = F,
           prop.chisq = F)
### Construir el crosstable ====
ct2 <- table(categoriaBase>umbral, Default)
ct2
### Diagonalizar ====
d2 <- diag(prop.table(ct2,1))
# Porcentaje de clasificacion correcta
sum( diag(prop.table(ct2)))
# El porcentaje de clasificacion correcta es de 61%
# este procentaje completo se redujo pero la clasificacion
# de los 1 aumento.

# Proyectar ====
newdata <- data.frame(duration = 48,
                      amount = 5951,
                      installment = 2,
                      age = 22)

predict(ad, newdata = newdata,
        type = "response")
# El umbral es muy pequeno, sin embargo se lo
# clasifica como buen pagador.

# Sensitividad, Especificidad, Punto de corte Optimo ====
pred <- prediction(categoriaBase,
                   DB1$Default)

perf <- performance(pred,
                    measure = "tpr",
                    x.measure = "fpr")

# Ploteo
plot(perf,
     colorize = T,
     lty = 3)
abline(0,1, col="black")

## Punto de corte ====
perf1 <- performance(pred,
                     "sens",
                     "spec")

sen <- slot(perf1, "y.values")[[1]]

spec <- slot(perf1, "x.values")[[1]]

alf <- slot(perf1, "alpha.values")[[1]]

matr <- data.frame(alf, sen, spec)
matr
matr1 <- melt(matr, id = c("alf"))

# Ploteo ====
p1 <- ggplot(matr1,
             aes(alf,value,group = variable,colour = variable))+
      geom_line(size=1.2)+
      labs(title = "Punto de corte optimo",
           x = "Cortes",
           y = "")
p1
ggplotly(p1)
# Punto de corte optimo = 0.287

# Con esa grafica se puede observar que no es tan 
# favorable el modelo.