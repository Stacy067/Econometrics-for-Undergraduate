#Multiple Linear Regression with R

install.packages("wooldridge")
library(wooldridge)
data("wage2")
View(wage2)
attach(wage2)
wage2

wage=V1
educ=V5
exper=V6
pairs(~wage+educ+exper)

OLS=lm(wage~educ+exper)
summary(OLS)

SLR=lm(wage~educ)
MLR=lm(wage~educ+exper)
ASLR=lm(exper~educ)

summary(SLR)
summary(MLR)
summary(ASLR)

SLR2=lm(wage~exper)
ASLR2=lm(educ~exper)

FLR1=lm(wage~ASLR2$residuals)
summary(FLR1)

FLR2=lm(SLR2$residuals~ASLR2$residuals)
summary(FLR2)

cenwage=scale(wage,center=TRUE,scale=FALSE)
ceneduc=scale(educ,center=TRUE,scale=FALSE)
cenexper=scale(exper,center=TRUE,scale=FALSE)
OLS=lm(cenwage~ceneduc+cenexper-1)
summary(OLS)

OLS=lm(cenwage~ceneduc+cenexper)
summary(OLS)