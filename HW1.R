################################
## Setting up
################################
rm(list = ls(all.names = TRUE))
setwd("/Users/Andrew/Documents/Github/IRT-and-Survival")
library(ltm)
library(mirt)
library(TAM)
library(eRm)
library(IRTShiny)
library(psych)
library(lavaan)
library(semTools)


################################
## Question 12
################################
covm1<-'
1.0 0.7 0.3
    1.0 .48
        1.0
'
covm1b<-getCov(covm1, diagonal=TRUE, lower=FALSE, names=c("B", "C", "D"))

model12<-'
A=~B
C~B+A
D~A
'

fit1<-sem(model12, sample.cov=covm1b, sample.nobs=100)
summary(fit1)


#Next tactic
covm1<-'
1.0 0.7 0.3
1.0 .48
1.0
'
covm1b<-getCov(covm1, diagonal=TRUE, lower=FALSE, names=c("B", "C", "D"))

model12<-'
A=~D+C
B~C
'

fit1<-sem(model12, sample.cov=covm1b, sample.nobs=100)
summary(fit1)

#Harrumph.

#Let's look at the final question really quickly.


