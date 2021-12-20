#Simulation with R

S1=rbinom(10,1,0.5)
S1
mean(S1)
hist(S1)

S2=rbinom(10,1,0.5)
S2
mean(S2)
hist(S2)

S3=rbinom(10,1,0.5)
S3
mean(S3)
hist(S3)

S4=rbinom(10,1,0.5)
S4
mean(S4)
hist(S4)


M=NULL

for(i in 1:10000){
x=0:19
y=3+4*x+rnorm(20,mean=0,sd=10)
M[i]=lm(y~x)$coefficient[2]
}

mean(M)
var(M)
hist(M)


M=NULL

for(i in 1:10000){
x=runif(20,min=0,max=20)
y=3+4*x+rnorm(20,mean=0,sd=10)
M[i]=lm(y~x)$coefficient[2]
}

mean(M)
var(M)
hist(M,probability=TRUE)


#OLS Estimation with R

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

coef(OLS)
OLS$coef

residuals(OLS)[1:5]
OLS$residuals[1:5]

fitted(OLS)[1:5]
OLS$fitted[1:5]


summary(OLS)$sigma

cor(wage,educ)
cor(wage,educ)^{2}

plot(wage,OLS$fitted)
cor(wage,OLS$fitted)
cor(wage,OLS$fitted)^{2}

