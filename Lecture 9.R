#Weighted Least Squares Estimation

deathrate<-read.csv("C:/Users/DELL/Desktop/deathrate.csv") #Designate the location to "Desktop"
View(deathrate)
attach(deathrate)
WLS=lm(deathrate~drink+smoke+aged+vehipc,data=deathrate,weights=regpop)
summary(WLS)


#Feasible Weighted Least Squares Estimation

deathrate<-read.csv("C:/Users/DELL/Desktop/deathrate.csv") #Designate the location to "Desktop"
View(deathrate)
attach(deathrate)
RegModel=deathrate~drink+aged+smoke+vehipc
OLS=lm(RegModel, data=deathrate)
deathrate$residuals=OLS$residuals
e=deathrate$residuals
AUX=lm(update(RegModel,I(log(e^2))~.,data=deathrate))
deathrate$ghat=AUX$fitted
deathrate$hhat=exp(deathrate$ghat)
FGLS=lm(RegModel,data=deathrate,weights=1/hhat)
summary(FGLS)


#Heteroskedacity - Consistent Variance of Estimator

deathrate<-read.csv("C:/Users/DELL/Desktop/deathrate.csv") #Designate the location to "Desktop"
View(deathrate)
attach(deathrate)
RegModel=deathrate~drink+aged+smoke+vehipc
OLS=lm(RegModel, data=deathrate)
install.packages("sandwich")
library(sandwich)
vcovHC(OLS, type="HC0")
install.packages("lmtest")
library(lmtest)
coeftest(OLS,vcov=vcovHC(OLS,type="HC0"))
coeftest(OLS) #Automatically setting "VHCO"


#Heteroskedacity - Robust WALD test

##Original WALD test

deathrate<-read.csv("C:/Users/DELL/Desktop/deathrate.csv") #Designate the location to "Desktop"
View(deathrate)
attach(deathrate)
RegModel=deathrate~drink+aged+smoke+vehipc
OLS=lm(RegModel, data=deathrate)
install.packages("lmtest")
library(lmtest)
waldtest(OLS,c("drink","smoke"))

##Robust WALD test - Chisq test with VHCO

deathrate<-read.csv("C:/Users/DELL/Desktop/deathrate.csv") #Designate the location to "Desktop"
View(deathrate)
attach(deathrate)
RegModel=deathrate~drink+aged+smoke+vehipc
OLS=lm(RegModel, data=deathrate)
install.packages("sandwich")
library(sandwich)
vcovHC(OLS, type="HC0")
install.packages("lmtest")
library(lmtest)
waldtest(OLS,c("drink","smoke"),vcovHC(OLS, type="HC0"),test="Chisq")

##Robust WALD test - F test with VHCO

deathrate<-read.csv("C:/Users/DELL/Desktop/deathrate.csv") #Designate the location to "Desktop"
View(deathrate)
attach(deathrate)
RegModel=deathrate~drink+aged+smoke+vehipc
OLS=lm(RegModel, data=deathrate)
install.packages("sandwich")
library(sandwich)
vcovHC(OLS, type="HC0")
install.packages("lmtest")
library(lmtest)
waldtest(OLS,c("drink","smoke"),vcovHC(OLS, type="HC0"))

##Robust WALD test - F test with VHCO

deathrate<-read.csv("C:/Users/DELL/Desktop/deathrate.csv") #Designate the location to "Desktop"
View(deathrate)
attach(deathrate)
RegModel=deathrate~drink+aged+smoke+vehipc
OLS=lm(RegModel, data=deathrate)
install.packages("sandwich")
library(sandwich)
vcovHC(OLS, type="HC0")
install.packages("lmtest")
library(lmtest)
waldtest(OLS,c("drink","smoke"),vcovHC(OLS, type="HC0"))

##Robust WALD test - F test with VHC1

deathrate<-read.csv("C:/Users/DELL/Desktop/deathrate.csv") #Designate the location to "Desktop"
View(deathrate)
attach(deathrate)
RegModel=deathrate~drink+aged+smoke+vehipc
OLS=lm(RegModel, data=deathrate)
install.packages("sandwich")
library(sandwich)
vcovHC(OLS, type="HC1")
install.packages("lmtest")
library(lmtest)
waldtest(OLS,c("drink","smoke"),vcovHC(OLS, type="HC1"))

##Robust WALD test - F test with VHC2

deathrate<-read.csv("C:/Users/DELL/Desktop/deathrate.csv") #Designate the location to "Desktop"
View(deathrate)
attach(deathrate)
RegModel=deathrate~drink+aged+smoke+vehipc
OLS=lm(RegModel, data=deathrate)
install.packages("sandwich")
library(sandwich)
vcovHC(OLS, type="HC2")
install.packages("lmtest")
library(lmtest)
waldtest(OLS,c("drink","smoke"),vcovHC(OLS, type="HC2"))

##Robust WALD test - F test with VHC3

deathrate<-read.csv("C:/Users/DELL/Desktop/deathrate.csv") #Designate the location to "Desktop"
View(deathrate)
attach(deathrate)
RegModel=deathrate~drink+aged+smoke+vehipc
OLS=lm(RegModel, data=deathrate)
install.packages("sandwich")
library(sandwich)
vcovHC(OLS, type="HC3")
install.packages("lmtest")
library(lmtest)
waldtest(OLS,c("drink","smoke"),vcovHC(OLS, type="HC3"))

OR

waldtest(OLS,c("drink","smoke"),vcov=vcovHC) #Automatically setting "VHC3"


##Robust WALD test - F test with VHC4

deathrate<-read.csv("C:/Users/DELL/Desktop/deathrate.csv") #Designate the location to "Desktop"
View(deathrate)
attach(deathrate)
RegModel=deathrate~drink+aged+smoke+vehipc
OLS=lm(RegModel, data=deathrate)
install.packages("sandwich")
library(sandwich)
vcovHC(OLS, type="HC4")
install.packages("lmtest")
library(lmtest)
waldtest(OLS,c("drink","smoke"),vcovHC(OLS, type="HC4"))


#Heteroskedacity - Finding Heteroskedacity

##Breush-Pagan test

install.packages("wooldridge")
library(wooldridge)
data("wage1")
View(wage1)
attach(wage1)
install.packages("lmtest")
library(lmtest)
bptest(wage~educ+exper,~educ+exper,data=wage1)

##White test - Using Quadraditc form of y hat

install.packages("wooldridge")
library(wooldridge)
data("wage1")
View(wage1)
attach(wage1)
install.packages("lmtest")
library(lmtest)
OLS=lm(wage~educ+exper,data=wage1)
yhat=OLS$fitted
bptest(OLS,~yhat+yhat^2,data=wage1)


#Heteroskedacity - Autocorrelation

##Prais-Winsten Estimation 1

data("phillips")
View(phillips)
install.packages("prais")
library(prais)
prais_winsten(inf~unem,data=phillips)

##Prais-Winsten Estimation 2

install.packages("prais")
library(prais)
install.packages("orcutt")
library(orcutt)
data("icecream")
View(icecream)
prais_winsten(cons~price+income+temp,data=icecream)


##Cochrane-Orcutt Estimation

install.packages("orcutt")
library(orcutt)
data("icecream")
View(icecream)
OLS=lm(cons~price+income+temp,data=icecream)
cochrane.orcutt(OLS)

##Newy-West Inference - Finding the Max Lag with VHAC3

install.packages("wooldridge")
library(wooldridge)
data("phillips")
View(phillips)
OLS=lm(inf~unem,data=phillips)
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)
coeftest(OLS,vcov=vcovHAC)# VHAC3


##Newy-West Inference - Finding the Max Lag with VHC3

install.packages("wooldridge")
library(wooldridge)
data("phillips")
View(phillips)
OLS=lm(inf~unem,data=phillips)
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)
coeftest(OLS,vcov=vcovHC(OLS, type="HC3"))

OR

coeftest(OLS,vcov=vcovHC)#Automatically setting VHC3


##Newy-West Inference - Finding the Max Lag with Lag=4

install.packages("wooldridge")
library(wooldridge)
data("phillips")
View(phillips)
OLS=lm(inf~unem,data=phillips)
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)
coeftest(OLS,vcov=NeweyWest(OLS,lag=4))

##Newy-West Inference - Finding the Max Lag with Lag=3

install.packages("wooldridge")
library(wooldridge)
data("phillips")
View(phillips)
OLS=lm(inf~unem,data=phillips)
install.packages("lmtest")
library(lmtest)
install.packages("sandwich")
library(sandwich)
coeftest(OLS,vcov=NeweyWest(OLS,lag=3))

OR

coeftest(OLS,vcov=NeweyWest)#Automatically setting Lag=3


#Durbin-Watson test - testing for Autocorrelation

install.packages("wooldridge")
library(wooldridge)
data("phillips")
View(phillips)
install.packages("lmtest")
library(lmtest)
dwtest(inf~unem,data=phillips)

#Breush-Godfrey test - testing for Autocorrelation with order 1

install.packages("wooldridge")
library(wooldridge)
data("phillips")
View(phillips)
install.packages("lmtest")
library(lmtest)
bgtest(inf~unem,data=phillips) #Automatically setting order 1

#Breush-Godfrey test - testing for Autocorrelation with order 3

install.packages("wooldridge")
library(wooldridge)
data("phillips")
View(phillips)
install.packages("lmtest")
library(lmtest)
bgtest(inf~unem,data=phillips, order=3)






