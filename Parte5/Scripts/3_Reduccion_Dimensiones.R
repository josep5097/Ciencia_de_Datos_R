# Analisis Factorial
# Descubrir variables latentes inobservables

# Librerias ====
library(foreign)
library(openxlsx)
library(psych)

# Data ====
DB <- read.xlsx("Data/dientes.xlsx")

# Descripcion de Variables ====
# 1 es peor - 7 es mejor

# V1: Es importante comprar dentricos que prevengan las caries
# V2: Me gustan los dentrificos que dejan los dientes brillantes
# V3: Un dentrifico tiene que fortalecer las encias
# V4: Prefiero un dentrifico que refresque el aliento
# V5: La prevencion de las caries no es un beneficio importante ofrecido por los dentrificos
# V6: La consideracion mas importante al comprar un dentrifico son los dientes bellos

dientes<-DB[,2:7]
attach(dientes)

# Correlaciones ====
cor(dientes,method = "spearman")
# Sin correlacion entre las variables, probablemente
# no seria buena idea utilizar ACP
# Con una alta correlacion se debe de considerar la rotacion 
# de las variables

# Estadistico KMO
KMO(dientes)
# Si KMO es mayor que 0.6 entonces es buena idea utilizar ACP / AF

macp1 <- princomp(dientes, 
                  scores=TRUE,
                  cor=F)

summary(macp1)

# cargas de los componentes principales

loadings(macp1)
macp1$loadings

# Grafico de sedimentacion o screeplot
plot(macp1)
abline(h=1, v=3)

screeplot(macp1, 
          type="line", 
          main="Scree Plot")
abline(h=1)

# Biplot of score variables
biplot(macp1)

# Scores 
macp1$scores[1:10,]
score <- data.frame(macp1$scores)
score

# Realizarlo por Analisis Factorial ====
maf1 <- factanal(dientes, 
                 factor=2,
                 rotation="none")

maf1
maf1$loadings

# Plot de F2 vs F1
load <- maf1$loadings[,1:2] 
plot(load)
text(load,
     labels=names(dientes),
     cex=.9)

maf2 <- factanal(dientes, 
                 factors=3,
                 rotation="none",
                 scores="regression")

maf2
loadings(maf2)

load2 <- maf2$loadings[,1:3] 
plot(load2) # set up plot 
text(load2,labels=names(dientes),cex=.9) # add variable names


maf3 <- factanal(dientes, 
                 factors=2,
                 rotation="varimax")

maf3
loadings(maf3)