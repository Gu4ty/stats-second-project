library(plyr)
# library(readr)
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

employee <- read.csv("../raw_data/adult.data.csv")
glimpse(employee)

names <- c(2, 4, 6:10, 14, 15)
employee[, names] <- lapply(employee[, names], factor)
glimpse(employee)

# Estableciendo una semilla para tener resultados deterministas
set.seed(1)

# Entrenamos con el 0.7 de la muestra y probamos con el 0.3
# Con este arbol de clasificacion queremos predecir el income de una persona
# El arbol resultante nos permite hacer una reduccion de variables a las que
# utiliza dicho arbol pues son las que guardan relacion con la variable.
train_row_number <- createDataPartition(employee$sex, p = 0.7, list = F)

# Normalizando con z-score las variables no cualitativas
cols <- c(
    "age", "fnlwgt", "education.num", "capital.gain",
    "capital.loss", "hours.per.week"
)
pre_proc_val <- preProcess(employee[, cols], method = c("center", "scale"))
# Aplicando la normalizcion a al set de entrenamiento y prueba
employee[, cols] <- predict(pre_proc_val, employee[, cols])

summary(employee)

# Se entrena el algoritmo de clasificacion tratando de predecir
# el income.
tree_model <- rpart(
    income ~ .,
    data = employee[train_row_number, ],
    method = "class",
    cp = 0.05
)

summary(tree_model)

rpart.plot(tree_model)

# Prediciendo en el set de prueba.
predict_cart <- predict(
    tree_model,
    newdata = employee[-train_row_number, ],
    type = "class"
)
tb <- table(predict_cart, employee[-train_row_number, ]$income)
tb
accuracy <- sum(diag(tb)) / sum(tb)
accuracy
precision <- tb[1, 1] / (tb[1, 1] + tb[2, 1])
precision
recall <- tb[1, 1] / (tb[1, 1] + tb[1, 2])
recall
f1 <- 2 * (precision * recall) / (precision + recall)
f1
specificity <- tb[2, 1] / (tb[2, 1] + tb[1, 2])
specificity

# Es posible ver como se tiene un accuracy del 83%. Los resultados de precision,
# recall y f-score son buenos tambien. No obstante specificity tiene un
# resultado muy bajo del ~20%. Esto se debe a que falla la mitad de las veces
# para detectar cuando alguien gana mas de 50k.
# Esto se debe a que esta distribuido 70~30 los que ganan menos de 50k contra
# los que ganan mas. Como tiene muy buenos resultados prediciendo quien gana
# menos y estos representan la mayoria de los datos entonces se tienen buenos
# resultados.
# La razon de por que el entrenamiento falla es debido al sesgo que existe
# en income.
