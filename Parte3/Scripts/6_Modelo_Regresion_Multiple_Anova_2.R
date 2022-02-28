# Modelos de Regresion Multiple - Anova

# Librerias ====
library(openxlsx)

# Data ====
Anova_DB <- read.xlsx("Data/salarios.xlsx")

#--------- Estimando el modelo #--------

modelo.Anova_DB <-lm(w ~ female, data = Anova_DB)
summary(modelo.Anova_DB)


modelo.Anova_DB <-lm(w ~ female - 1, data = Anova_DB)
summary(modelo.Anova_DB)