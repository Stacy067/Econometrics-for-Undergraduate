install.packages("wooldridge")
library(wooldridge)

data("saving")
View(saving)
attach(saving)


#Beta Coefficient
##Original Model

OLS1=lm(sav~inc+size+educ+age)
summary(OLS1)

##Scaled Model
OLS10=lm(scale(sav)~scale(inc)+scale(size)+scale(educ)+scale(age))
summary(OLS10)


#Log-Level Model

install.packages("Ecdat")
library(Ecdat)
data("Wages1")
View(Wages1)
attach(Wages1)
OLS=lm(log(wage)~school+exper,data=Wages1)
summary(OLS)


#Regression with Quadratic term

data("smoke")
View(smoke)
attach(smoke)
OLS=lm(cigs~cigpric+educ+income+age+I(age^2))
summary(OLS)


#Regression with Interaction term

OLS=lm(cigs~cigpric+educ+income+age+I(cigpric*age))
summary(OLS)


#Dummy Variables Trap

OLS=lm(log(wage)~school+exper+sex,data=Wages1)
summary(OLS)


#Dummy Variable - 1 Dummy

install.packages("Ecdat")
library(Ecdat)
data("Wages1")
View(Wages1)
attach(Wages1)
Wages1$female=as.numeric(Wages1$sex=='female')
OLS=lm(log(wage)~school+exper,data=Wages1)
summary(OLS)


#Dummy Variable - 1 Dummy

data("mlb1")
View(mlb1)
attach(mlb1)
OLS=lm(salary~years+bavg+runs+frstbase+scndbase+shrtstop+thrdbase+outfield)
summary(OLS)


#Dummy Vareiable - Polytomous Factor

data("mlb1")
View(mlb1)
attach(mlb1)
OLS=lm(salary~years+bavg+runs+frstbase+scndbase+shrtstop+thrdbase+outfield+hispan+blackpop)
summary(OLS)


#Dummy Variables with ineractions term

install.packages("Ecdat")
library(Ecdat)
data("Wages1")
View(Wages1)
attach(Wages1)
Wages1$female=as.numeric(Wages1$sex=='female')
OLS=lm(log(wage)~school+exper+female+I(female*school),data=Wages1)
summary(OLS)


#General F-test

wage1<-read.csv("~/Desktop/wage1.csv", header=False)
View(wage1)
attach(wage1)
wage=V1
educ=V2
exper=V3
female=V6

OR
 
install.packages("wooldridge")
library(wooldridge)

data("wage1")
View(wage1)
attach(wage1)

OLSU=lm(log(wage)~educ+exper+female+I(educ*female)+I(exper*female))
OLSR=lm(log(wage)~educ+exper)
anova(OLSR,OLSU)


#Chow Test 1

data("wage1")
View(wage1)
attach(wage1)

male=ifelse((female==0),1,0)
educF=educ*female
educM=educ*male
experF=exper*female
experM=exper*male
OLSR=lm(log(wage)~educ+exper)
OLSU=lm(log(wage)~female+male+educF+educM+experF+experM)
anova(OLSR,OLSU)

OLSR=lm(log(wage)~educ+exper)
OLSU=lm(log(wage)~female+male+educF+educM+experF+experM-1)
anova(OLSR,OLSU)


#Chow Test 2

data("saving")
View(saving)
attach(saving)
regP=lm(sav~inc,data=saving)
reg1=lm(sav~inc,data=saving[saving<1982,])
reg2=lm(sav~inc,data=saving[saving> 1981,])


SSR=NULL
SSR$P=regP$residuals^2
SSR$F=reg1$residuals^2
SSR$S=reg2$residuals^2
k=regP$rank
numerator=(sum(SSR$P)-(sum(SSR$F)+sum(SSR$S)))/k
denominator=(sum(SSR$F)+sum(SSR$S))/(nrow(saving)-2*k)
chow=numerator/denominator
chow
1-pf(chow,k,(nrow(saving)-2*k))


#Multicollinearity 1

data("mlb1")
View(mlb1)
attach(mlb1)
OLS=lm(log(salary)~years+gamesyr+hrunsyr)
summary(OLS)


#Multicollinearity 2

data("mlb1")
View(mlb1)
attach(mlb1)
OLS=lm(log(salary)~years+gamesyr+hrunsyr+rbisyr)
summary(OLS)


#RESET Test 1

data("wage2")
View(wage2)
attach(wage2)

OLSR=lm(wage~educ+exper+tenure,data=wage2)
wagehat=OLSR$fit
OLSU=lm(wage~educ+exper+tenure+I(wagehat^2)+I(wagehat^3),data=wage2)
anova(OLSR,OLSU)


#RESET Test 2

install.packages("lmtest")
library(lmtest)
resettest(wage~educ+exper+tenure,power=2:3,data=wage2)





