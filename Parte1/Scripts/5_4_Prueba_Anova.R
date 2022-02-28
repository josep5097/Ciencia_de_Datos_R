# Analisis Anova ====

# Comprobar si dif tratamientos muestran
# Diferencias significativas

# Data ====
Informacion <- read.xlsx("Data/ipc20172018.xlsx")

# Realizar un stack de la informacion
Incrustado <- stack(Informacion)

boxplot(values ~ ind,
        data = Incrustado)

Resultado <- aov(values ~ ind,
                 data = Incrustado)

summary(Resultado)
# H0: uciudades iguales
# con p<0.05, H0 se descarta

# Identificar en que combinacion es distinta
TukeyHSD(Resultado)
# Entre uio-gye no hay diferencia significativa de medias




# Analisis Anova 2 Factores ====
# Libreria ====
library(foreign)
# Data ====
BD <- read.spss(file = "Data/02 ENIGHUR11_PERSONAS_INGRESOS.sav",
                use.value.labels = F,
                to.data.frame = T)

# Manipulacion de Data ====
# Tener las variables a disposicipon
attach(BD)
# Cambiar el nombre de Variables
names(BD)[names(BD)=="Provincia"]<-"Prov"
names(BD)[names(BD)=="Regional"]<-"Region"
names(BD)[names(BD)=="P03"]<-"Edad"
names(BD)[names(BD)=="P02"]<-"Genero"
names(BD)[names(BD)=="P06"]<-"Estado"
names(BD)[names(BD)=="P07"]<-"Autoetnica"
names(BD)[names(BD)=="i1702004"]<-"Gassb"
names(BD)[names(BD)=="i1702006"]<-"Gasalq"
names(BD)[names(BD)=="i1401002"]<-"Sueldo"

View(BD)

# Aplicar Filtros
BD2 <- BD %>%
        filter(Edad >=18, Edad<=65,
               Prov == 9, 
               P04 == 1,
               P05A == 1,
               Sueldo >= 368,
               Sueldo <= 800) %>%
        filter(!is.na(Sueldo))%>%
        mutate(P14A = if_else(P14A<=8,1,0),
               P08 = if_else(P08<=3,1,0)
               )

summary(BD2$Sueldo[BD2$P08==1])

boxplot(Sueldo ~ P08, data = BD2)
boxplot(Sueldo ~ P14A, data = BD2)

# Analisis de Frecuencia
library(fdth)
TablaFreq <- fdt(BD2$Sueldo,
                 breaks = "Sturges")
TablaFreq

hist(BD2$Sueldo)

Resultados = aov(lm(Sueldo ~ P08+P14A,
                    data = BD2))
summary(Resultados)

attach(BD2)

interaction.plot(P08,P14A, Sueldo, data=BD2,
                 col = 2:3)

Resultados2 <- aov(lm(Sueldo ~ P08 + P14A + P08:P14A, data = BD2))
summary(Resultados)