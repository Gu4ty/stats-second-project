# Utilizamos una implementacion de kprototype
library(clustMixType)

# Cargando y preprocesando los datos
employee <- read.csv("./raw_data/adult.data.csv")
names <- c(2, 4, 6:10, 14, 15)
employee[, names] <- lapply(employee[, names], factor)

set.seed(1)


# validation_kproto calcula el numero optimo de clusters, no obstante,
# nusetros equipos de computo no tienen ram suficiente para realizar
# el analisis.
# a <- validation_kproto(method = "macclain", data = employee, k = 2:5) #nolint

# Intuitivamente elegimos 2 como el numero de clusters a dividir la muestra
# Con esta cantidad se ve una mejor division de los grupos
fit <- kproto(employee, 2, verbose = T)


plot(employee, col = fit$cluster)
clprofiles(fit, employee)
