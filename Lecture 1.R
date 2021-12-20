#Simulation with R

x=1:50
y=3+5*x+rnorm(50,mean=0,sd=4)
plot(x,y)

x=rnorm(50,mean=5,sd=4)
y=3+5*x+rnorm(50,mean=0,sd=10)
plot(x,y)

x=0:19
y=3+4*x+rnorm(19,mean=0,sd=10)
plot(x,y)

OLS1=lm(y~x)
summary(OLS1)

abline(OLS1,col="red")

x=0:19
y=3+4*x+rnorm(19,mean=0,sd=10)
plot(x,y)

OLS2=lm(y~x)
summary(OLS2)

abline(OLS2,col="red")

y=c(3,2,5,7,8,10,3,5,2,7)
x=c(5,5,5,5,5,5,5,5,5,5)
OLS=lm(y~x)
summary(OLS)

install.packages("wooldridge")
library(wooldridge)
data("wage1")
View(wage1)
attach(wage1)

wage=V1
educ=V2

OLS=lm(wage~educ)
summary(OLS)
plot(wage~educ)
abline(OLS,col="red")

attach(vote1)
OLS=lm(voteA~shareA)
summary(OLS)
plot(voteA~shareA)
abline(OLS,col="red")