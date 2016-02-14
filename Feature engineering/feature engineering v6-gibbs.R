# cleanup before you start
rm(list=ls())

if (!require('mvtnorm')) install.packages('mvtnorm')
library(mvtnorm)
if (!require('mlogit')) install.packages('mlogit')
library(mlogit)
if (!require('nnet')) install.packages('nnet')
library(nnet)
if (!require('ade4')) install.packages('ade4')
library(ade4)

setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

y <- data.train[,'popularity']
# Log likelihood is sum over n of 1-5 options x true value
# So we need that indicator function matrix of n x classes for real data
y.expanded <- acm.disjonctif(as.matrix(y))
phi <- data.train[,setdiff(colnames(data.train), 'popularity')]

# BAD NEWS
formula.1 <- formula(paste0("popularity ~ ", paste(names(data.train)[1:59], collapse = ' + ')))
base.model <- mlogit(formula.1, data = data.train, shape = 'wide')
# Error in solve.default(H, g[!fixed]) : 
# Lapack routine dgesv: system is exactly singular: U[17,17] = 0

# ADD ARGUMENT FOR CONDITION ON Y
loglik <- function(model) {
  # deviance = -2 log likelihoods
  -model$deviance/2
}

base.model <- multinom(popularity ~ ., data = data.train, maxit = 10)
# Likelihood
# first vector of y.expanded * first vector of base.model$fitted.values
# should get something that is 1 x 5 or 5 x 1
# check that this is ok
(base.loglik <- loglik(base.model))


# for computing r-squared we need intercept log likelihood
intercept.model <- multinom(popularity ~ 1, data = data.train, maxit = 10)
(intercept.loglik <- loglik(intercept.model))

### Gibbs Sampling
# number of iterations
L <- 1000
n <- 100 # SHOULD BE OBSERVATIOSN?? BUT THIS NUMBER IS TOO BIG, MARGINAL LIKELIHOOD IS INF
m <- ncol(data.train - 1)

# start

# update loop
set.seed(14234)
for (i in 1:L) {
  for (j in 1:m) {
    
    #Compute probabilities without var j
    #number of parameters and gamma without j
    d.0 <- sum(gamma[-j]!=0) 
    gamma.test.0 <- gamma
    gamma.test.0[j] <- 0
    #Compute MLE r-squared without j
    phi.0 <- phi[,c(TRUE,(gamma.test.0==1))] 
    model.0 <- multinom(y ~ phi.0)
    
    # pseudo r-squared is 1 - (likelihood(model))/(likelihood(intercept))
    pseudo.rsquared.0 <- 1 - (loglik(model.0))/intercept.loglik
    
    #Compute second term of marginal likelihood (first term same for all models).
    # is this the same as the log likelihood for our model??
    # this doesn't seem right, these are inf
    # Marginal likelihood of data given model.0
    marg.lik.0 <- as.numeric( (1+g)^((n-d.0-2)/2) / (1+g*(1-pseudo.rsquared.0))^((n-1)/2))

    #Compute model probability with var j
    #number of parameters and gamma with j
    d.1 <- d.0 + 1
    gamma.test.1 <- gamma
    gamma.test.1[j] <- 1
    #Compute MLE r-squared with j
    phi.1 <- phi[,c(TRUE,(gamma.test.1==1))] 
    model.1 <- multinom(y ~ as.matrix(phi.1))
    w <- as.matrix(t(summary(model.1)$coefficients))
    phi.1.plus <- cbind(rep(1, nrow(phi.1)), phi.1)
    res <- t(w)%*%t(phi.1.plus)%*%as.matrix(phi.1.plus)%*%w
    pseudo.rsquared.1 <- 1 - (loglik(model.1))/intercept.loglik
    # Marginal likelihood of data given model.1
    marg.lik.1 <- as.numeric( (1+g)^((n-(d.1)-2)/2) / (1+g*(1-pseudo.rsquared.1))^((n-1)/2) )
    
    # compute probability of new variable being in model 
    # this should be okay given we can compute the marginal likelihood of our data given models 0 and 1
    prob <- marg.lik.1 * model.prior[d.1+1] / (marg.lik.1 * model.prior[d.1+1] + marg.lik.0 * model.prior[d.0+1]) 
    
    # update gamma and variables
    if (runif(1) < prob) {
      gamma[j] <- 1  
    }
    else {
      gamma[j] <- 0
    }
    
  }
  #Compute results for the new model
  d.new <- sum(gamma!=0) 
  phi.new <- phi[,c(TRUE,(gamma==1))] 
  model.new <- multinom(y ~ as.matrix(phi.new))
  pseudo.rsquared.1 <- 1 - (loglik(model.new))/intercept.loglik
  e <- model.new$residuals
  # why is this different than above?
  # we don't have standard residuals because these are multinomial models
  marg.lik.new <- as.numeric(factorial((n-1)/2 - 1) / (pi^((n-1)/2) * sqrt(n)) * (sqrt(t(e) %*% e))^(1-n) * (1+g)^((n-d.new-2)/2) / (1+g*(1-R.sq.new))^((n-1)/2) )
  #Save w estimates (w computed based on the w_bayes estimate expression for Zellner's g prior)
  w.history[i,which(gamma!=0)] = ((g/ (g+1)) * solve(t(PHI.new)%*% PHI.new)%*%t(PHI.new)%*%y)[-1]
  #Save probabilities
  prob.new <- marg.lik.new * model.prior[d.new+1]
  prob.history[i] <- marg.lik.new
  #Save gamma
  gamma.history[i,] <- gamma
}
