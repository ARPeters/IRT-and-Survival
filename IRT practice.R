################################
## Setting up
################################
library(ltm)
library(mirt)
library(TAM)
library(eRm)
library(IRTShiny)
library(psych)
rm(list = ls(all.names = TRUE))


# Note: made need to edit this, depending on where this file is saved. 
setwd("/Users/Andrew/Documents/Github/IRT-and-Survival")

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


