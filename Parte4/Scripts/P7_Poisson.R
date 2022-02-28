file.choose()

db <- read.csv(file="Data/credito.csv",
               header=TRUE)

attach(db)
modelo <-  glm(MDR ~  Age + Income, 
               family = poisson(link=log), 
               data = db)
# MDR: Numero de veces que una persona queda impago

summary(modelo)
# No es posible interpretarlo directamente, pero se puede 
# reconocer si un coef aporta a que suceda un evento
# En este caso, de que aporte a un evento de impago.

# Calculo de la exponencial del modelo
exp(coef(modelo))

# Predecir
newdata = data.frame(Age=30, Income=3.2)
lambda <-predict(modelo, 
        newdata, 
        type="response") 

# Funcion dpois ====
# Funcion de masa de probabilidad de la distribucion Poisson

# Calculo de la probabilidad de que los eventos ocurra
# a partir del lambda obtenido.
dpois(c(0,1,2,3,4,5,6),
      lambda = lambda,
      log = F)
# La suma de la probabilidades suma siempre 1
sum(dpois(c(0,1,2,3,4,5,6),
          lambda = lambda,
          log = F))

# Grafico de la funcion de probabilidad
plot(dpois(c(0,1,2,3,4,5,6),
           lambda = lambda,
           log = F),
     type = "h",
     lwd = 2,
     main = "Funcion de Probabilidad",
     ylab = "P(X=x)",
     xlab = "Numero de eventos")