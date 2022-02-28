# Regresiones y Analisis Factorial ====
# Seccion A ====

# Librerias ====
library(openxlsx)
library(corrplot)
library(factoextra)
library(GPArotation)
library(psych)
library(dplyr)
library(GGally)

# Data ====
DB <- read.csv("Data/Exterior_Exportaciones_Totales_mensual_puntoycoma.csv",
                 stringsAsFactors=F,
                 header=T,
                 sep = ";",
                 dec = ",")

# Manipulacion de la data ====
attach(DB)

head(DB$variable)
str(DB)

vars <- transmute(DB,
               anio = anio,
               mes.num = mes.num,
               variable = variable,
               val = valor
               )
str(vars)
# Var1
balComNP <- filter(vars,
                   variable == "Balanza Comercial no petrolera")
balComNP <- select(balComNP,anio, mes.num,val)
# Var2
balComP <- filter(vars,
                  variable == "Balanza Comercial Petrolera")
balComP <- select(balComP, anio, mes.num,val)
# Var3
saldoAcumBC <- filter(vars,
                      variable == "Saldo acumulado balanza comercial % PIB ")
saldoAcumBC <- select(saldoAcumBC, anio, mes.num,val)
# Var4
totExpFOB <- filter(vars, 
                    variable == "Total Exportaciones FOB")
totExpFOB <- select(totExpFOB, anio, mes.num,val)
# Var5
totImpFOB <- filter(vars, 
                    variable == "Total Importaciones FOB")
totImpFOB <- select(totImpFOB, anio, mes.num,val)
# Join
data.nueva <- right_join(balComNP,
                         balComP,
                         by = c("anio","mes.num"))

data.nueva <- right_join(data.nueva,
                         saldoAcumBC,
                         by = c("anio","mes.num"))

data.nueva <- right_join(data.nueva,
                         totExpFOB,
                         by = c("anio","mes.num"))

data.nueva <- right_join(data.nueva,
                         totImpFOB,
                         by = c("anio","mes.num"))

colnames(data.nueva) <- c("anio","mes","balCompNP","balComP","saldoAcumBC","totExpFOB","totImpFOB")

data.nueva <- filter(data.nueva, anio >= 2015)
str(data.nueva)

# Corralacion ====
cor(data.nueva[3:7])

ggpairs(data.nueva[3:7])
DB2 <- data.nueva[3:7]

# ACP ====
macp1 <- princomp(DB2,
                  cor = T,
                  scores = T)
summary(macp1)

## Scree Plot ====
p1 <- fviz_eig(macp1,
               choice = "eigenvalue",
               addlabels = T,
               hjust = -0.3)
p1
p2 <- fviz_eig(macp1,
               choice = "variance",
               addlabels = T,
               hjust = -0.3)
p2

# AF ====
# Se realiza un Analisis Factorial con 2 componentes, debido a que al tener 
# 5 variables, realizar con 3 componentes es mucho.

maf1 <- factanal(DB2, 
                 factor=2,
                 rotation="none")
maf1
maf1$loadings

# Plot de F2 vs F1
load <- maf1$loadings[,1:2] 
plot(load)
text(load,
     labels=names(data.nueva),
     cex=.9)
