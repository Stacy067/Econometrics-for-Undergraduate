#Multiple Regression with R

install.packages("wooldridge")
library(wooldridge)
data("wage2")
View(wage2)
attach(wage2)
wage2


#R-Squared and Adjusted R-Squared

extra=rnorm(935,mean=15,sd=100)
OLS1=lm(wage~educ+exper)
OLS2=lm(wage~educ+exper+extra)
summary(OLS1)
summary(OLS2)

OLS=lm(wage~educ+exper)
summary(OLS)

impv=rchisq(935,df=5)
OLS=lm(wage~educ+exper+impv)
summary(OLS)

impv2=rnorm(935,mean=0,sd=100)
OLS=lm(wage~educ+exper+impv2)
summary(OLS)

