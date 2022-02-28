# Prueba T para muestras Relacionadas ====

# Librerias ====
library(openxlsx)

# Data ====
Informacion <- read.xlsx("Data/bc.xlsx")

t.test(x = Informacion$bc_2000_2007,
       y = Informacion$bc_2008_2015,
       alternative = "two.sided",
       paired = T)

# H0: uBc2007 = uBc2015
# H1: uBC2077 =! uBC2015

# Con p<0.05 -> Se rechaza H0.
# Existe una dif de medias entre el 2007 y el 2015!

t.test(x = Informacion$bc_2000_2007,
       y = Informacion$bc_2008_2015,
       alternative = "greater",
       paired = T)
# H0: uBc2007 = uBc2015
# H1: uBC2077 > uBC2015
# Con p < 0.05 -> Se rechaza 
# Entonces u2007> u2015