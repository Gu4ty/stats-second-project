library(dplyr)
library(data.table)

# cargando y procesando los datos
employee <- read.csv("./raw_data/adult.data.csv")
names <- c(2, 4, 6:10, 14, 15)
employee[, names] <- lapply(employee[, names], factor)

glimpse(employee)

# Como contamos con datos categoricos utilizamos la distancia
# de gower. No obstante, las siguientes lineas tienden a fallar
# debido a la falta de ram en nuestras computadoras.
library(cluster)
gower_dist <- daisy(employee, metric = c("gower"))
