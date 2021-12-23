source("utils.r")
library(lmtest)

col_classes = c("numeric", "factor", "numeric", "factor", "numeric", "factor", "factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "factor", "factor")
raw_data <- read.csv("raw_data/adult.data.csv", header = TRUE, colClasses = col_classes )
data <- delete_bad_rows(raw_data)

income <- seq(1, 2)
names(income) <- c(" <=50K", " >50K")
data$income <- categorical_to_numerical(data$income, income)


do_anova <- function(independent, dependent,name_of_independent, name_of_dependent){
  independent <- sample(independent, 1000)
  dependent <- sample(dependent, 1000)
  
  anova_data <- data.frame(independent, dependent)
  anova_data <- anova_data[order(anova_data$independent),]
  plot(dependent ~ independent, data = anova_data, ylab = name_of_dependent, xlab= name_of_independent)
  
  result <- aov(dependent ~ independent, data = anova_data)
  res <- result$residuals

  is_model_ok = TRUE
  stest <- shapiro.test(res)
  if(stest$p.value < 0.05){
    print("residuals do not have a normal distribution")
    is_model_ok = FALSE
  }

  btest <- bartlett.test(res, anova_data$independent)
  if(btest$p.value < 0.05){
    print("residuals are not homogeneous")
    is_model_ok = FALSE
  }
  dtest <- dwtest(result)
  if(dtest$p.value < 0.05){.
    print('errors are not independent')
    is_model_ok = FALSE
  }
  if(is_model_ok){
    summary(result)
  }
  else{
    print("assumptions not fulfilled")
  }
  
}

anova <- aov(data$income ~ data$workclass, data = data)
summary(anova)

#do_anova(data$race, data$hours.per.week, "race", "hours.per.week")
#do_anova(data$occupation, data$hours.per.week, "occupation", "hours.per.week")
#do_anova(data$occupation, data$capital.gain, "occupation", "capital.gain")
#do_anova(data$occupation, data$age, "occupation", "age")
#do_anova(data$workclass, data$age, "workclass", "age")
#do_anova(data$marital.status, data$income, "marital.status", "income")
#do_anova(data$occupation, data$income, "occupation", "income")
#do_anova(data$education, data$income, "education", "income")
#do_anova(data$workclass, data$income, "workclass", "income")


