# Modelos de Regresion multiple ====
# Evaluacion de los modelos
# Modelos Anova_DB

# Modelos de extension multiple variables, se tiene una variable Y,
# que es de caracter cuantitativa o de escala: Ventas, Oferta monetaria, etc,
# frente a una variable dicotomica (1 o 0): genero, ubicacion geogrifica, trimestres.
# Se le coloca 1 cuando uno esta interesado en esa categoria.

# Data ====

Anova_DB <- read.csv("Data/modelo anova.csv")

# Parametros ====

# D2 1 si es noreste o norte centro, 0 ECOP
# D3 1 si es Sur, 0 ECOP
# D4 1 si es oeste, 0 ECOP

# Modelo ====

modelo.Anova_DB <-lm(Salary ~ D2 + D3, 
                     data = Anova_DB)
summary(modelo.Anova_DB)

# Analisis ====

# Como no se toma D4, todo el analisis se lo lleva a esa variable.
# El valor de D4, se lo toma en el valor de la intercepcion.
# Los profesores ubicados en el noreste ganan en promedio al anno 1524 dolares mas que 
# los profesores que se encuentran en el oeste.
# Los profesores que se encuentran en el SUR, ganan 1721 dolares menos que 
# los profesores del OESTE.
# En donde los profesores del OESTE ganan en promedio 48015,

# Caso Particular:
# Estimando el modelo - multicolinealidad perfecta
# Trampa de la variable ficticia o trampa de la variable dicotomica
# Var_Dicotomica = m-1, donde m es categoria

modelo.Anova_DB1 <-lm(Salary ~ D2 + D3+ D4, 
                      data = Anova_DB)
summary(modelo.Anova_DB1)

# Si se incorporan todas las variables, sale en una variable NA.


# Estimando el modelo sin trampa de la variable dicotomica ====
# Calculando los promedios directamente, evadiendo la trampa de la Var Dicotomica

# El -1, suprime el coef de intercepci?n, es decir B0
modelo.Anova_DB2 <-lm(Salary ~ D2 + D3+ D4 - 1, 
                      data = Anova_DB)
summary(modelo.Anova_DB2)

# Analisis:
# Se le resta el valor del coeficiente de intercepcion:
# Aparecen los valores promedios netos para cada caso existente
# Noroeste: gana en prom 49539
# Sur: ganan en prom 46294
# Oeste: Ganan en prom 48015.
# Es el valor que se tienen en el analisis anterior basados en un valor constante.