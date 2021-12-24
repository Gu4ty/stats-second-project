source("src/utils.R")
library('lmtest')

# process raw data
raw_data <- read.csv('src/raw_data/adult.data.csv')
data <- as.data.frame(
				unclass(delete_bad_rows(raw_data)),
				stringsAsFactors = TRUE
			)

str(data)

chisq.test(data$income,data$native.country)
plot(data$education.num,data$age)

data$income <- as.integer(as.ordered(factor(data$income)))

lmodel <- lm(income~fnlwgt+capital.gain+capital.loss+relationship+hours.per.week+sex+education.num+age+workclass+occupation,data=data)
summary(lmodel)

plot(sample(lmodel$residuals,400))
hist(lmodel$residuals)
print(mean(lmodel$residuals))
print(sum(lmodel$residuals))


# normal distribution 
shapiro.test(sample(lmodel$residuals,50))
ks.test(
		lmodel$residuals,
		'pnorm',
		mean=mean(lmodel$residuals),
		sd=sd(lmodel$residuals)
)

# independence distribution 
dwtest(lmodel)

# constant variance 
bptest(lmodel)
bartlett.test(lmodel$residuals, data$income)

