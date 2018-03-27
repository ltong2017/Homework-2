# Homework-2
last homework for Advanced Impact
data(lalonde)
attach(lalonde)
Y <- lalonde$re78 #the outcome of interest
Tr <- lalonde$treat #the treatment of interest
#Now With GenMatch

X <- cbind(age, educ, black, hisp, married, nodegr, re74, re75, u74, u75)
BalanceMat <- cbind(age, I(age^2), educ, I(educ^2), black,
                    + hisp, married, nodegr, re74 , I(re74^2), re75, I(re75^2),
                    + u74, u75, I(re74*re75), I(age*nodegr), I(educ*re74))
set.seed(1234)

library(rgenoud)

gen1 <- GenMatch(Tr=Tr, X=X, BalanceMat=BalanceMat, pop.size=50, 
                 data.type.int=FALSE, print=0, replace=FALSE, 
                 unif.seed=812821, int.seed=53058)

mgen1 <- Match(Y=Y, Tr=Tr, X=X, Weight.matrix=gen1, replace=FALSE)


#Genetic Weights
summary(mgen1)
#Sensitivity Tests
psens(mgen1, Gamma=1.5, GammaInc=.1)
install.packages("rbounds")
library(rbounds)

## boosting Gamma value
mgen2 = mgen1

w <- c(which(mgen1$mdata$Y[1:96]==0))

mgen2$mdata$Y <- replace(mgen1$mdata$Y, w, 20000) 
psens(mgen2, Gamma=3, GammaInc=.1)

## III - JTPA Analysis

setwd("D:/CEU/Second semester 2017-2018/impact evaluation/advance")
library(foreign)
jtpa <- read.dta("jtpa.dta")
estimated_treatment_effect <- mean(jtpa$earnings[jtpa$assignmt == 1]) - 
  mean(jtpa$earnings[jtpa$assignmt == 0])


a <- length(which((jtpa$training ==1) & (jtpa$assignmt==1)))/length(which(jtpa$assignmt == 1))
b <- length(which((jtpa$training ==1) & (jtpa$assignmt==0)))/length(which(jtpa$assignmt == 0))   

compliance_rate <- estimated_treatment_effect/(a-b)

LATE <- estimated_treatment_effect / compliance_rate

## IV test
install.packages("AER")
library(AER)
library(ivreg)
iv1 <- ivreg(jtpa$earnings~jtpa$training|jtpa$assignmt,data=jtpa)

summary(iv1)
summary(iv1, vcov = sandwich, diagnostics = TRUE)
