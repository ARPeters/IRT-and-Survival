################################
## Setting up
################################
rm(list = ls(all.names = TRUE))
# Note: made need to edit this, depending on where this file is saved. 
#setwd("C:/Users/Andrew/Documents/Github/IRT-and-Survival")
setwd("C:/Users/APETERS4/Documents/GitHub")
setwd("C:/Users/APETERS4/Documents/GitHub/IRT-and-Survival")
library(mirt)
library(TAM)
library(eRm)
library(IRTShiny)
library(psych)
library(ltm)
getwd()
################################
## psych
################################

#Rasch model first,
dsRasch<-sim.rasch(nvar=5, n=500, low=-2, high=2, d=NULL, a=1, mu=0, sd=1)
dsRasch

#2pl Logistic model
discrAlphas<-c(.33, .66, 1, 1.33, 1.66, 2)
dsIRT<-sim.irt(nvar = 6, n = 500, low=-3, high=3,a=discrAlphas,c=0,z=1,d=NULL,mu=0,sd=1,mod="logistic")
dsIRT

#Analysis; let's do IRT first

test<-irt.fa(dsIRT$items)
test
str(test)
test$irt$difficulty
dsIRT$difficulty

#irt.item.diff.rasch
estBetasRasch1<-irt.item.diff.rasch(dsRasch$items)
estBetasRasch1
dsRasch$tau


#irt.discrim
estAlphasRasch1<-irt.discrim(estBetasRasch1, theta=dsRasch$theta, items=dsRasch$items)
estAlphasRasch1
dsRasch$theta
?sim.rasch



################################
## ltm
################################
#Can Kinda sorta compare results here:
#http://www.personality-project.org/r/book/Chapter8.pdf

dsBock<-data(bock)
head(LSAT)
dsLaw<-LSAT
dsLaw$StudentID<-1:nrow(dsLaw)

StudentID<-c(1:nrow(dsLaw))

descript(LSAT)
descript(dsLaw[,c(1:5)])

rcor.test(dsLaw[c(1:5)])

cronbach.alpha(dsLaw[c(1:5)])

#Return
unidimTest(LSAT)

LawRasch<-rasch(dsLaw[c(1:5)])
LawLTM<-ltm(LSAT)

?ltm()
unidimTest(LawRasch)

head(WIRS)
dsWIRS<-WIRS
head(dsWIRS)
ltm(dsWIRS ~ z1, constr = rbind(c(1, 1, 1), c(6, 2, -0.5)))
ltm(dsWIRS ~ z1*z2, constr = rbind(c(1, 1, 1), c(6, 2, -0.5)))
ltm(dsWIRS ~ z1*z2)

?anova()
anova(ltm(dsWIRS~z1), ltm(dsWIRS~z1+z2))


LSATRasch<-rasch(dsLaw[c(1:5)])
GoF.rasch(LSATRasch)
ltm0<-ltm(dsLaw[c(1:5)]~z1)
ltmalt<-ltm(dsLaw[c(1:5)]~z1+z2)

GoF.rasch(rasch(LSAT))
anova(ltm0, ltmalt)

item.fit(rasch(LSAT))
