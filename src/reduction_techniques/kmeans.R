library(caret)
# El paquete clustMixType da la implementacion de kproto, una expansion de
# kmeans para trabajar con variables categoricas, variables quantitativas y la
# combinacion de estas.
# Es posible tambien dividir las variables categorias en variables binarias y
# aplicar kmeans pero no es una solucion factible, la dimension de los datos
# se multiplicaria hasta los 100
library(clustMixType)


# Cargando, eliminando filas con valores NA y estandarizando
employee <- read.csv("../raw_data/adult.data.csv")
for (i in 1:15) employee <- employee[employee[i] != " ?", ]
names <- c(2, 4, 6:10, 14, 15)
employee[, names] <- lapply(employee[, names], factor)
cols <- c(
    "age", "fnlwgt", "education.num", "capital.gain",
    "capital.loss", "hours.per.week"
)
pre_proc_val <- preProcess(employee[, cols], method = c("center", "scale"))
# Aplicando la normalizcion a al set de entrenamiento y prueba
employee[, cols] <- predict(pre_proc_val, employee[, cols])


# Utilizando una semilla fija para obtener resultados deterministas
set.seed(1)
# Metodo para encontrar la cantidad optima de clusters, no obstante
# para nuestro set de datos la ram de la computadora resulta
# insuficiente.
a <- validation_kproto(method = "mcclain", data = employee, k = 2:5)

# Intuimos que tener dos clusters es buena pues muestra grupos
# bien definidos
employee <- subset(employee, select = c(-capital.loss))
fit <- kproto(employee, 4, verbose = T)

plot(employee, col = fit$cluster)
clprofiles(fit, employee)
