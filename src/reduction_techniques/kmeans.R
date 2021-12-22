library(caret)
# El paquete clustMixType da la implementacion de kproto, una expansion de
# kmeans para trabajar con variables categoricas, variables quantitativas y la
# combinacion de estas.
# Es posible tambien dividir las variables categorias en variables binarias y
# aplicar kmeans pero no es una solucion factible, la dimension de los datos
# se multiplicaria hasta los 100
library(clustMixType)


# Cargando y preprocesando los datos
employee <- read.csv("../raw_data/adult.data.csv")
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

# Es necesario leer mas para llegar a esta parte.
confidence <- c()
for (cluster in 2:15) {
    if (cluster %% 10 == 0) {
        print(paste(cluster, "clusters"))
    }
    fit_temp <- kproto(employee, cluster)
    confidence <- append(confidence, fit_temp$betweenss / fit_temp$totss)
}
plot(1:15, confidence)


t <- data.frame(x = c(-5, 1, 2, 4, 5, 6, 7), y = c(4, 5, 7, 8, 10, 11, 14))
a <- validation_kproto(method = "mcclain", data = employee, k = 2:5)

fit <- kproto(employee, 3, verbose = T)
fitk <- kmeans(t, 2)

plot(employee, col = fit$cluster)
clprofiles(fit, employee)
