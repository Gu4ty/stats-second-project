source("utils.r")

col_classes = c("numeric", "factor", "numeric", "factor", "numeric", "factor", "factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "factor", "factor")
raw_data <- read.csv("raw_data/adult.data.csv", header = TRUE, colClasses = col_classes )
data <- read.csv("data/data.csv")

do_anova <- function(independent, dependent, name_of_dependent){
  anova_data <- data.frame(independent, dependent)
  anova_data <- anova_data[order(anova_data$independent)]
  anova_data
}

plotea <- function(a,b, y){
  d <- data.frame(a,b)
  plot(a ~ b, data =d, ylab = y)
}
#plot(age ~ income , data = raw_data)
plotea(raw_data$age, raw_data$income, 'age')

summary(data)
summary(raw_data)

income <- raw_data$income
native_country <- raw_data$native.country
education <- raw_data$education

df <- data.frame(education, income)
df <- df[order(df$education),]



#income_num <- categorical_to_numerical(data$income, income)
#print(income_num)


#summary(data)
#one.way <- aov(income ~ education, data = data)
#summary(one.way)

w <- raw_data$workclass
a <- raw_data$age

d <- data.frame(w,a)
print(d$w)

#print(do_anova(raw_data$workclass, raw_data$age))