# Cargando librerias y procesando datos
library(dplyr)
library(data.table)

employee <- read.csv("./raw_data/adult.data.csv")
for (i in 1:15) employee <- employee[employee[i] != " ?", ]
names <- c(2, 4, 6:10, 14, 15)
employee[, names] <- lapply(employee[, names], factor)

glimpse(employee)

# Se busco calcular la matris de disimilaridad, no obstane el set
# de datos parece ser muy grande y la memoria ram de nuestras pc
# muy poca. Luego no se pudo aplicar esta tecnica.
# No obstante intuimos que no aportaria mucha informacion, pues
# al ser una gran cantidad de individuos identificar que significa
# cada cluster tomaria una enorme e innecesaria cantidad de tiempo

# Los proximas instrucciones provocan que R crashee si no se tiene la suficiente
# cantidad de RAM. Probamos hasta con un maximo de 3 GB de ram libres sin
# obtener un resultado satisfactorio
dist <- dist(employee)

library(cluster)
gower_dist <- daisy(employee, metric = c("gower"))
