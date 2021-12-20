#Interval Estimation with R

install.packages("wooldridge")
library(wooldridge)
data("wage2")
View(wage2)
attach(wage2)
wage2

OLS=lm(wage~educ+exper)
summary(OLS)

confint(OLS)
confint(OLS,level=0.90)
confint(OLS,level=0.99)