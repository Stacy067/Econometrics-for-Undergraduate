#Hypothesis Testing with R

##Example 1 / H0 : Beta = 0

data("saving")
View(saving)
attach(saving)

save=1
inc=V2
size=V3
educ=V4
age=V5


OLS1=lm(sav~inc+size+educ+age)
OLS6=lm(sav~size+educ+age)
anova(OLS6,OLS1) #Using ANOVA


install.packages("zoo")
library(zoo)
waldtest(OLS1,"inc") #Using WALD test


install.packages("car")
library(car)
lht(OLS1,"inc") #Using Linear Hypothesis


##Example 2 / H0: beta2 = beta3 = beta4 =0

OLS1=lm(sav~inc+size+educ+age)
OLS7=lm(sav~inc)
anova(OLS7,OLS1) #Using ANOVA


install.packages("car")
library(car)
lht(OLS1,c("size","educ","age")) #Using Linear Hypothesis

install.packages("lmtest")
library(lmtest)
waldtest(OLS1,OLS7) #Using WALD test


##Example 3 / H0: beta2 + beta4 = 1

install.packages("car")
library(car)
lht(OLS1,"size=1-age") #Using Lienar Hypothesis

sav1=sav-age
size1=size-age
OLS8=lm(sav1~inc+size1+educ+age)
summary(OLS8)


## Example 4 / H0: beta2 = beta3 = beta4

OLS1=lm(sav~inc+size+educ+age)
lht(OLS1,c("size=educ","size=age"))


OLS1=lm(sav~inc+size+educ+age)
nhr=rbind(c(0,0,1,-1,0),c(0,0,1,0,-1))
lht(OLS1,nhr)

OLSU=lm(sav~inc+size+educ+age)
sea=size+educ+age
OLSR=lm(sav~inc+sea)
anova(OLSR,OLSU)

