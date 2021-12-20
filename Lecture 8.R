#Random Sampling with Chi-sq Dist for CLT

M=NULL
for(i in 1:10000){
x=rchisq(n=1000, df=1)
M[i]=mean(x)
}
hist(M, probability = T, main="Histogram of Sample Mean(n=1000)")
lines(density(M), col="red")


#R description

dname() #denstity of probability function
pname() #culmulative density function
qname() #quantile function
rname() #random deviates


#R name of distribution

beta #Beta distribution
binom #Binominal distribution
cauchy #Cauchy distribution
chisq #Chisquare distribution
f #F distribution
gamma #Gamma distribution
geom #Geometric distribution
hyper #Hypergeometric distribution
logis #Logistic distribution
lnorm #Lognormal distribution
nbinom #Negative Binomial distribution
norm #Normaal distribution
pois #Poisson distribution
t #Student t distribution
tukey #Tukey distribution
weib #Weibull distribution
wilcox #Wilcoxon distribution


#WALD test with Chisq distribution

install.packages("Ecdat")
library(Ecdat)
install.packages("lmtest")
library(lmtest)
data(Housing)
View(Housing)
attach(Housing)
OLS=lm(log(price)~log(lotsize)+bedrooms+bathrms+driveway+airco,data=Housing)
OLSR=lm(log(price)~log(lotsize)+bedrooms+bathrms,data=Housing)
waldtest(OLS,OLSR,test="Chisq")


#WALD test with F distribution

install.packages("Ecdat")
library(Ecdat)
install.packages("lmtest")
library(lmtest)
data(Housing)
View(Housing)
attach(Housing)
OLS=lm(log(price)~log(lotsize)+bedrooms+bathrms+driveway+airco,data=Housing)
OLSR=lm(log(price)~log(lotsize)+bedrooms+bathrms,data=Housing)
waldtest(OLS,OLSR,test="F")


#WALD test with Chisq distribution without lmtest packages

install.packages("Ecdat")
library(Ecdat)
data(Housing)
View(Housing)
attach(Housing)
n=nrow(Housing)
OLS=lm(log(price)~log(lotsize)+bedrooms+bathrms+driveway+airco,data=Housing)
OLSR=lm(log(price)~log(lotsize)+bedrooms+bathrms,data=Housing)
ER=sum(OLSR$residuals^2)
EE=sum(OLS$residuals^2)
WALD=((n-6)*(ER-EE))/EE
WALD
1-pchisq(WALD,2)


#LR test with pacakges

install.packages("Ecdat")
library(Ecdat)
install.packages("lmtest")
library(lmtest)
data(Housing)
View(Housing)
attach(Housing)
OLS=lm(log(price)~log(lotsize)+bedrooms+bathrms+driveway+airco,data=Housing)
OLSR=lm(log(price)~log(lotsize)+bedrooms+bathrms,data=Housing)
lrtest(OLS,OLSR)


#LR test without lmtest pacakges

install.packages("Ecdat")
library(Ecdat)
data(Housing)
View(Housing)
attach(Housing)
n=nrow(Housing)
OLS=lm(log(price)~log(lotsize)+bedrooms+bathrms+driveway+airco,data=Housing)
OLSR=lm(log(price)~log(lotsize)+bedrooms+bathrms,data=Housing)
ER=sum(OLSR$residuals^2)
EE=sum(OLS$residuals^2)
LR=n*(log(ER)-log(EE))
LR
1-pchisq(WALD,2)


#LM test

install.packages("Ecdat")
library(Ecdat)
data(Housing)
View(Housing)
attach(Housing)
n=nrow(Housing)
OLSR=lm(log(price)~log(lotsize)+bedrooms+bathrms,data=Housing)
Housing$er=OLSR$residuals
OLS=lm(er~log(lotsize)+bedrooms+bathrms+driveway+airco,data=Housing)
LMTEST=n*summary(OLS)$r.sq
LMTEST
1-pchisq(LMTEST,2)


