################################
## Setting up
################################

library(ltm)
 library(mirt)
library(TAM)
library(eRm)
library(IRTShiny)

################################
## mirt
################################

################################
##  Messing around

dat <- expand.table(Bock1997)
head(dat)
mod <- mirt(dat, 1, 'nominal')



#reproduce table 3 in Bock (1997)
fs <- round(fscores(mod, verbose = FALSE)[,c('F1','SE_F1')],2)
fttd <- residuals(mod, type = 'exp')
table <- data.frame(fttd[,-ncol(fttd)], fs)
table
#using nominal.highlow matrix to specify lowest and highest categories
(nominal.highlow <- matrix(c(4,4,4,1,1,1), 2, byrow = TRUE))
mod <- mirt(dat, 1, 'nominal', nominal.highlow=nominal.highlow)
coef(mod)


##Looking at mirt() function

## Not run:
#load LSAT section 7 data and compute 1 and 2 factor models
data <- expand.table(LSAT7)
(mod1 <- mirt(data, 1))
coef(mod1)
(mod2 <- mirt(data, 1, SE = TRUE)) #standard errors with crossprod method
coef(mod2)

(mod2 <- mirt(data, 1, SE = TRUE, SE.type = 'SEM')) #standard errors with SEM method
coef(mod2)

(mod3 <- mirt(data, 1, SE = TRUE, SE.type = 'BL')) #standard errors with BL method
residuals(mod1)

plot(mod1) #test score function
plot(mod1, type = 'trace') #trace lines
plot(mod2, type = 'info') #test information

plot(mod2, MI=200) #expected total score with 95% confidence intervals

#estimated 3PL model for item 5 only
(mod1.3PL <- mirt(data, 1, itemtype = c('2PL', '2PL', '2PL', '2PL', '3PL')))
coef(mod1.3PL)

#internally g and u pars are stored as logits, so usually a good idea to include normal prior
# to help stabilize the parameters. For a value around .182 use a mean
# of -1.5 (since 1 / (1 + exp(-(-1.5))) == .182)

model <- 'F = 1-5 PRIOR = (5, g, norm, -1.5, 3)'
mod1.3PL.norm <- mirt(data, model, itemtype = c('2PL', '2PL', '2PL', '2PL', '3PL'))
coef(mod1.3PL.norm)

#limited information fit statistics
M2(mod1.3PL.norm)

#unidimensional ideal point model
idealpt <- mirt(data, 1, itemtype = 'ideal')
plot(idealpt, type = 'trace', facet_items = TRUE)
plot(idealpt, type = 'trace', facet_items = FALSE)
#two factors (exploratory)
mod2 <- mirt(data, 2)
coef(mod2)
summary(mod2, rotate = 'oblimin') #oblimin rotation
62 mirt
residuals(mod2)
plot(mod2)
plot(mod2, rotate = 'oblimin')
anova(mod1, mod2) #compare the two models
scores <- fscores(mod2) #save factor score table
scoresfull <- fscores(mod2, full.scores = TRUE) #factor scores for each response pattern

#confirmatory (as an example, model is not identified since you need 3 items per factor)
# Two ways to define a confirmatory model: with mirt.model, or with a string
# these model definitions are equivalent

cmodel <- mirt.model('
                     F1 = 1,4,5
                     F2 = 2,3')
cmodel2 <- 'F1 = 1,4,5 F2 = 2,3'
cmod <- mirt(data, cmodel)
cmod <- mirt(data, cmodel2) # same as above
coef(cmod)
anova(cmod, mod2)
#check if identified by computing information matrix

(cmod <- mirt(data, cmodel, SE = TRUE))

###########
#data from the 'ltm' package in numeric format
pmod1 <- mirt(Science, 1)
plot(pmod1)
summary(pmod1)

#Constrain all slopes to be equal with the constrain = list() input or mirt.model() syntax
#first obtain parameter index
values <- mirt(Science,1, pars = 'values')
values #note that slopes are numbered 1,5,9,13, or index with values$parnum[values$name == 'a1']
(pmod1_equalslopes <- mirt(Science, 1, constrain = list(c(1,5,9,13))))
coef(pmod1_equalslopes)
# using mirt.model syntax, constrain all item slopes to be equal
model <- 'F = 1-4
CONSTRAIN = (1-4, a1)'
(pmod1_equalslopes <- mirt(Science, model))
coef(pmod1_equalslopes)
coef(pmod1_equalslopes)
anova(pmod1_equalslopes, pmod1) #significantly worse fit with almost all criteria
pmod2 <- mirt(Science, 2)
summary(pmod2)
plot(pmod2, rotate = 'oblimin')
itemplot(pmod2, 1, rotate = 'oblimin')
anova(pmod1, pmod2)
mirt 63
#unidimensional fit with a generalized partial credit and nominal model
(gpcmod <- mirt(Science, 1, 'gpcm'))
coef(gpcmod)
#for the nominal model the lowest and highest categories are assumed to be the
# theoretically lowest and highest categories that related to the latent trait(s), however
# a custom nominal.highlow matrix can be passed to declare which item category should be
# treated as the 'highest' and 'lowest' instead
(nomod <- mirt(Science, 1, 'nominal'))
coef(nomod) #ordering of ak values suggest that the items are indeed ordinal
anova(gpcmod, nomod)
itemplot(nomod, 3)
## example applying survey weights.
# weight the first half of the cases to be more representative of population
survey.weights <- c(rep(2, nrow(Science)/2), rep(1, nrow(Science)/2))
survey.weights <- survey.weights/sum(survey.weights) * nrow(Science)
unweighted <- mirt(Science, 1)
weighted <- mirt(Science, 1, survey.weights=survey.weights)
###########
#empirical dimensionality testing that includes 'guessing'
data(SAT12)
data <- key2binary(SAT12,
                   key = c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5))
mod1 <- mirt(data, 1)
slot(mod1, 'time') #time elapsed for each estimation component
#optionally use Newton-Raphson for (generally) faster convergence in the M-step's
mod1 <- mirt(data, 1, optimizer = 'NR')
slot(mod1, 'time')
mod2 <- mirt(data, 2, optimizer = 'NR')
#difficulty converging with reduced quadpts, reduce TOL
mod3 <- mirt(data, 3, TOL = .001, optimizer = 'NR')
anova(mod1,mod2)
anova(mod2, mod3) #negative AIC, 2 factors probably best
#same as above, but using the QMCEM method for generally better accuracy in mod3
mod3 <- mirt(data, 3, method = 'QMCEM', TOL = .001, optimizer = 'NR')
anova(mod2, mod3)
#with fixed guessing parameters
mod1g <- mirt(data, 1, guess = .1)
coef(mod1g)
###########
#graded rating scale example
#make some data
set.seed(1234)
64 mirt
a <- matrix(rep(1, 10))
d <- matrix(c(1,0.5,-.5,-1), 10, 4, byrow = TRUE)
c <- seq(-1, 1, length.out=10)
data <- simdata(a, d + c, 2000, itemtype = rep('graded',10))
mod1 <- mirt(data, 1)
mod2 <- mirt(data, 1, itemtype = 'grsm')
coef(mod2)
anova(mod2, mod1) #not sig, mod2 should be preferred
itemplot(mod2, 1)
itemplot(mod2, 5)
itemplot(mod2, 10)
###########
# 2PL nominal response model example (Suh and Bolt, 2010)
data(SAT12)
SAT12[SAT12 == 8] <- NA #set 8 as a missing value
head(SAT12)
#correct answer key
key <- c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5)
scoredSAT12 <- key2binary(SAT12, key)
mod0 <- mirt(scoredSAT12, 1)
#for first 5 items use 2PLNRM and nominal
scoredSAT12[,1:5] <- as.matrix(SAT12[,1:5])
mod1 <- mirt(scoredSAT12, 1, c(rep('nominal',5),rep('2PL', 27)))
mod2 <- mirt(scoredSAT12, 1, c(rep('2PLNRM',5),rep('2PL', 27)), key=key)
coef(mod0)$Item.1
coef(mod1)$Item.1
coef(mod2)$Item.1
itemplot(mod0, 1)
itemplot(mod1, 1)
itemplot(mod2, 1)
#compare added information from distractors
Theta <- matrix(seq(-4,4,.01))
par(mfrow = c(2,3))
for(i in 1:5){
  info <- iteminfo(extract.item(mod0,i), Theta)
  info2 <- iteminfo(extract.item(mod2,i), Theta)
  plot(Theta, info2, type = 'l', main = paste('Information for item', i), ylab = 'Information')
  lines(Theta, info, col = 'red')
}
par(mfrow = c(1,1))
#test information
plot(Theta, testinfo(mod2, Theta), type = 'l', main = 'Test information', ylab = 'Information')
lines(Theta, testinfo(mod0, Theta), col = 'red')
###########
# using the MH-RM algorithm
data(LSAT7)
mirt 65
fulldata <- expand.table(LSAT7)
(mod1 <- mirt(fulldata, 1, method = 'MHRM'))
#Confirmatory models
#simulate data
a <- matrix(c(
  1.5,NA,
  0.5,NA,
  1.0,NA,
  1.0,0.5,
  NA,1.5,
  NA,0.5,
  NA,1.0,
  NA,1.0),ncol=2,byrow=TRUE)
d <- matrix(c(
  -1.0,NA,NA,
  -1.5,NA,NA,
  1.5,NA,NA,
  0.0,NA,NA,
  3.0,2.0,-0.5,
  2.5,1.0,-1,
  2.0,0.0,NA,
  1.0,NA,NA),ncol=3,byrow=TRUE)
sigma <- diag(2)
sigma[1,2] <- sigma[2,1] <- .4
items <- c(rep('dich',4), rep('graded',3), 'dich')
dataset <- simdata(a,d,2000,items,sigma)
#analyses
#CIFA for 2 factor crossed structure
model.1 <- '
F1 = 1-4
F2 = 4-8
COV = F1*F2'
#compute model, and use parallel computation of the log-likelihood
mirtCluster()
mod1 <- mirt(dataset, model.1, method = 'MHRM')
coef(mod1)
summary(mod1)
residuals(mod1)
#####
#bifactor
model.3 <- '
G = 1-8
F1 = 1-4
F2 = 5-8'
66 mirt
mod3 <- mirt(dataset,model.3, method = 'MHRM')
coef(mod3)
summary(mod3)
residuals(mod3)
anova(mod1,mod3)
#####
#polynomial/combinations
data(SAT12)
data <- key2binary(SAT12,
                   key = c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5))
model.quad <- '
F1 = 1-32
(F1*F1) = 1-32'
model.combo <- '
F1 = 1-16
F2 = 17-32
(F1*F2) = 1-8'
(mod.quad <- mirt(data, model.quad))
summary(mod.quad)
(mod.combo <- mirt(data, model.combo))
anova(mod.quad, mod.combo)
#non-linear item and test plots
plot(mod.quad)
plot(mod.combo, type = 'SE')
itemplot(mod.quad, 1, type = 'score')
itemplot(mod.combo, 2, type = 'score')
itemplot(mod.combo, 2, type = 'infocontour')
## empirical histogram examples (normal, skew and bimodality)
#make some data
set.seed(1234)
a <- matrix(rlnorm(50, .2, .2))
d <- matrix(rnorm(50))
ThetaNormal <- matrix(rnorm(2000))
ThetaBimodal <- scale(matrix(c(rnorm(1000, -2), rnorm(1000,2)))) #bimodal
ThetaSkew <- scale(matrix(rchisq(2000, 3))) #positive skew
datNormal <- simdata(a, d, 2000, itemtype = 'dich', Theta=ThetaNormal)
datBimodal <- simdata(a, d, 2000, itemtype = 'dich', Theta=ThetaBimodal)
datSkew <- simdata(a, d, 2000, itemtype = 'dich', Theta=ThetaSkew)
normal <- mirt(datNormal, 1, empiricalhist = TRUE)
plot(normal, type = 'empiricalhist')
histogram(ThetaNormal, breaks=30)
bimodal <- mirt(datBimodal, 1, empiricalhist = TRUE)
plot(bimodal, type = 'empiricalhist')
histogram(ThetaBimodal, breaks=30)
mirt 67
skew <- mirt(datSkew, 1, empiricalhist = TRUE)
plot(skew, type = 'empiricalhist')
histogram(ThetaSkew, breaks=30)
#####
# non-linear parameter constraints with Rsolnp package (alabama supported as well):
# Find Rasch model subject to the constraint that the intercepts sum to 0
dat <- expand.table(LSAT6)
#free latent mean and variance terms
model <- 'Theta = 1-5
MEAN = Theta
COV = Theta*Theta'
#view how vector of parameters is organized internally
sv <- mirt(dat, model, itemtype = 'Rasch', pars = 'values')
sv[sv$est, ]
#constraint: create function for solnp to compute constraint, and declare value in eqB
eqfun <- function(p, optim_args) sum(p[1:5]) #could use browser() here, if it helps
solnp_args <- list(eqfun=eqfun, eqB=0)
mod <- mirt(dat, model, itemtype = 'Rasch', optimizer = 'solnp', solnp_args=solnp_args)
print(mod)
coef(mod)
(ds <- sapply(coef(mod)[1:5], function(x) x[,'d']))
sum(ds)
# same likelihood location as: mirt(dat, 1, itemtype = 'Rasch')
#######
# latent regression Rasch model
#simulate data
set.seed(1234)
N <- 1000
# covariates
X1 <- rnorm(N); X2 <- rnorm(N)
covdata <- data.frame(X1, X2)
Theta <- matrix(0.5 * X1 + -1 * X2 + rnorm(N, sd = 0.5))
#items and response data
a <- matrix(1, 20); d <- matrix(rnorm(20))
dat <- simdata(a, d, 1000, itemtype = 'dich', Theta=Theta)
#unconditional Rasch model
mod0 <- mirt(dat, 1, 'Rasch')
#conditional model using X1 and X2 as predictors of Theta
68 mirt
mod1 <- mirt(dat, 1, 'Rasch', covdata=covdata, formula = ~ X1 + X2)
coef(mod1, simplify=TRUE)
anova(mod0, mod1)
#bootstrapped confidence intervals
boot.mirt(mod1, R=5)
#draw plausible values for secondary analyses
pv <- fscores(mod1, plausible.draws = 10)
pvmods <- lapply(pv, function(x, covdata) lm(x ~ covdata$X1 + covdata$X2),
                 covdata=covdata)
#population characteristics recovered well, and can be averaged over
so <- lapply(pvmods, summary)
so
# compute Rubin's multiple imputation average
par <- lapply(so, function(x) x$coefficients[, 'Estimate'])
SEpar <- lapply(so, function(x) x$coefficients[, 'Std. Error'])
averageMI(par, SEpar)
############
# Example using Guass-Hermite quadrature with custom input functions
library(fastGHQuad)
data(SAT12)
data <- key2binary(SAT12,
                   key = c(1,4,5,2,3,1,2,1,3,1,2,4,2,1,5,3,4,4,1,4,3,3,4,1,3,5,1,3,1,5,4,5))
GH <- gaussHermiteData(50)
Theta <- matrix(GH$x)
# This prior works for uni- and multi-dimensional models
prior <- function(Theta, Etable){
  P <- grid <- GH$w / sqrt(pi)
  if(ncol(Theta) > 1)
    for(i in 2:ncol(Theta))
      P <- expand.grid(P, grid)
    if(!is.vector(P)) P <- apply(P, 1, prod)
    P
}
GHmod1 <- mirt(data, 1, optimizer = 'NR',
               technical = list(customTheta = Theta, customPriorFun = prior))
coef(GHmod1, simplify=TRUE)
Theta2 <- as.matrix(expand.grid(Theta, Theta))
GHmod2 <- mirt(data, 2, optimizer = 'NR', TOL = .0002,
               technical = list(customTheta = Theta2, customPriorFun = prior))
summary(GHmod2, suppress=.2)
## End(Not run)