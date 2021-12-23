library(caret)
# El paquete clustMixType da la implementacion de kproto, una expansion de
# kmeans para trabajar con variables categoricas, variables quantitativas y la
# combinacion de estas.
# Es posible tambien dividir las variables categorias en variables binarias y
# aplicar kmeans pero no es una solucion factible, la dimension de los datos
# se multiplicaria hasta los 100
library(clustMixType)

# Adaptacion de la funcion clprofiles de la libreria clustMixType,
# se afecta principalment el barplot para mostrar la leyenda entera
clprofiles <- function(object, x, col = NULL) {
    vars <- 1:ncol(x)
    clusids <- sort(unique(object$cluster))
    if (length(col) != max(clusids)) {
          warning("Length of col should match number of clusters!")
      }
    par(ask = TRUE)
    for (i in vars) {
        if (is.numeric(x[, i])) {
            boxplot(x[, i] ~ object$cluster, col = col, main = colnames(x)[i])
            legend("topright", legend = clusids, fill = col)
        }
        if (is.factor(x[, i])) {
            tab <- table(x[, i], object$cluster)
            for (j in 1:length(object$size)) {
                tab[, j] <- tab[
                    ,
                    j
                ] / object$size[j]
            }
            barplot(t(tab),
                beside = TRUE, main = colnames(x)[i],
                col = col,
                las = 2,
                cex.names = 0.7
            )
        }
    }
    par(ask = FALSE)
    invisible()
}


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

# Eliminando capital loss y capital gain, son datos muy sesgados. Si se
# observa su boxplot se ve que que son mayoritariamente puntos aberrantes
# Pues la mayoria de los datos se concentran  en 0
boxplot(employee$capital.gain)
boxplot(employee$capital.loss)
employee <- subset(employee, select = c(-capital.loss, -capital.gain))


# Utilizando una semilla fija para obtener resultados deterministas
set.seed(1)
# Metodo para encontrar la cantidad optima de clusters, no obstante
# para nuestro set de datos la ram de la computadora resulta
# insuficiente.
a <- validation_kproto(method = "mcclain", data = employee, k = 2:5)

# Intuimos que tener dos clusters es buena pues muestra grupos
# bien definidos

# Aplicando kproto
k <- 4
fit <- kproto(employee, k, verbose = T)

cluster_colors <- c()
colors <- c("#ffcc66", "#ff99ff", "#99ff99", "#66ccff", "#996666", "#6666ff")
for (i in 1:length(fit$cluster)) {
    cluster_colors <- append(cluster_colors, colors[fit$cluster[i]])
}

plot(employee, col = cluster_colors)
clprofiles(fit, employee, col = colors[1:k])


