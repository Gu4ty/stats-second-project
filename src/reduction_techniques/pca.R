library(dplyr)
library(data.table)

employee <- read.csv("src/raw_data/adult.data.csv")
for (i in 1:15) employee <- employee[employee[i] != " ?", ]
names <- c(2, 4, 6:10, 14, 15)
employee[, names] <- lapply(employee[, names], factor)

glimpse(employee)

# Convirtiendo las variables categoricas en variables binarias.
# Codigo extraido y adaptado de:
# https://stackoverflow.com/questions/33990760/converting-factors-to-binary-in-r
setDT(employee)[, c(levels(employee$workclass), "workclass") :=
    c(lapply(levels(workclass), function(x) as.integer(x == workclass)), .(NULL))] # nolint

setDT(employee)[, c(levels(employee$education), "education") :=
    c(lapply(levels(education), function(x) as.integer(x == education)), .(NULL))] # nolint

setDT(employee)[, c(levels(employee$marital.status), "marital.status") :=
    c(lapply(levels(marital.status), function(x) as.integer(x == marital.status)), .(NULL))] # nolint

setDT(employee)[, c(levels(employee$occupation), "occupation") :=
    c(lapply(levels(occupation), function(x) as.integer(x == occupation)), .(NULL))] # nolint

setDT(employee)[, c(levels(employee$relationship), "relationship") :=
    c(lapply(levels(relationship), function(x) as.integer(x == relationship)), .(NULL))] # nolint

setDT(employee)[, c(levels(employee$race), "race") :=
    c(lapply(levels(race), function(x) as.integer(x == race)), .(NULL))] # nolint

setDT(employee)[, c(levels(employee$income), "income") :=
    c(lapply(levels(income), function(x) as.integer(x == income)), .(NULL))] # nolint

setDT(employee)[, c(levels(employee$sex), "sex") :=
    c(lapply(levels(sex), function(x) as.integer(x == sex)), .(NULL))] # nolint

# Se elimina native.country debido a que a tiene muy mala distribucion y que
# no aporta realmente nada importa excepto que 37 variables adicionales

# setDT(employee)[, c(levels(employee$native.country), "native.country") :=
# c(lapply(levels(native.country), function(x) as.integer(x == native.country)), .(NULL))] # nolint
employee <- subset(employee, select = c(-native.country))

acp <- prcomp(employee, scale. = T)
summary(acp)
plot(acp)

# Esta tecnica es realmente mala para este set de datos. Es necesario expandir
# las variables categoricas a variables binarias, lo que implica un incremento
# de 107 varialbe hecho ingenuamente. Si se realiza con tiempo se puede reducir
# este numero a la mitad, aun asi representa un aumento considerable de
# variables con respecto a los datos originales.
# Ademas sobre las variables extraidas como no se tiene correalcion entre los
# datos las componentes son muy poco representativas de la poblacion, luego se
# necesitan una gran cantidad. Luego esta tecnica implica un incremento de
# dimension y un decremento de certeza.

acp$rotation
