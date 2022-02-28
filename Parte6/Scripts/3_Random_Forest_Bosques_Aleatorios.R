# Random Forest ====

# Se escoge (p-m)/p numero de divisiones
# Modelos de regresion p/3
# Modelos de clasificacion raiz(p), # predictores
# Mediante una validacion cruzada.

# 4 Criterios:====
# 1.- mtry= numero de predictores obtenidos de forma aleatoria
# 2.- min.node.size= tamano minimo que debe tener un nodo en cada arbol para ser dividido
# 3.- splitrule=  criterio de division. Generalmente se usa GINI
# 4.- Numero de arboles creados

# 1,2 y 4 se pueden optimizar.
# hiperparametros (mtry, #divisiones, #arboles a generar)
