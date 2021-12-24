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
employee <- read.csv("src/raw_data/adult.data.csv")
for (i in 1:15) employee <- employee[employee[i] != " ?", ]
names <- c(2, 4, 6:10, 14, 15)
employee[, names] <- lapply(employee[, names], factor)
cols <- c(
    "age", "fnlwgt", "education.num", "capital.gain",
    "capital.loss", "hours.per.week"
)
# Eliminando capital loss y capital gain, son datos muy sesgados. Si se
# observa su boxplot se ve que que son muchos datos conentrados en 0 y
# puntos aberrantes.
boxplot(employee$capital.gain)
boxplot(employee$capital.loss)
employee <- subset(employee, select = c(-capital.loss, -capital.gain))
or_employee <- employee

# Normalizando los datos
pre_proc_val <- preProcess(employee[, cols], method = c("center", "scale"))
employee[, cols] <- predict(pre_proc_val, employee[, cols])

set.seed(1)

# Probamos con el metodo del codo, para buscar el cluster optimo
resultss <- c()
max <- 25
for (i in 1:max) {
    print(paste("Trying with", i, "clusters"))
    fit <- kproto(employee, i, verbose = F, nstart = 5, keep.data = F)
    resultss <- append(resultss, fit$tot.withinss)
}
# El grafico aplicar el metodo del codo no queda muy claro cual es el mejor
# Intuimos que debe estar entre 4 y 9
plot(1:25, resultss[(35 - 25):34], type = "b", xlab = "# Clusters")


# clustMixType permite utilizar metodos para detectar el numero ideal
# de clusters, no obstante la ram de nuestras PCs resulta insuficiente
# para terminar el calclulo
r <- validation_kproto(method = "mcclain", data = employee)

k <- 9

# Aplicando kproto
fit <- kproto(employee, k, verbose = F, nstart = 5)
fit$tot.withinss
summary(fit, data = employee)

# Utilizar el resultado de kproto del reporte
load(file = "src/reduction_techniques/report_data/rfit.rda")

cluster_colors <- c()
colors <- c(
    "blue",
    "#ffcc66",
    "#ff99ff",
    "red",
    "#66ccff",
    "#99ff99",
    "orange",
    "#996666",
    "green"
)
n <- length(fit$cluster)
for (i in 1:n) {
    cluster_colors <- append(cluster_colors, colors[fit$cluster[i]])
}

plot(employee, col = cluster_colors)
clprofiles(fit, or_employee, col = colors[1:k])

# Descomentar para salvar fit
# save(fit, file = "report_data/fit.rda") #nolint
