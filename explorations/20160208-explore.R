## SETUP
filepath <- '~/Projects/kaggle-onlinenewspopularity/data/'
setwd(filepath)
if (!require('e1071')) install.packages('e1071')
library(e1071)
source('../explorations/setup.R')

# This things are pretty slow, so going to run with 1/4 the data
limit <- (nrow(data.train)/4)
x <- data.train[1:limit,-60]
y <- data.train[1:limit,60]

# takes a while...
svm.1 <- svm(x, y = y)
summary(svm.1)
# Parameters:
#   SVM-Type:  eps-regression 
# SVM-Kernel:  radial 
# cost:  1 
# gamma:  0.01694915 
# epsilon:  0.1 
# Number of Support Vectors:  21233
preds <- predict(svm.1, x)
success.rate(round(preds), y)
# 57.8 -> probably way overfitted?

# let's not get overexcited - try predicting the validation data!
x.val <- data.validation[,-60]
y.val <- data.validation[,60]
preds.validation <- predict(svm.1, x.val)
success.rate(round(preds.validation), y.val)
# Ya not as good -> 0.4975

# Try with cross-validation
svm.2 <- svm(x, y = y, cross = 5)
summary(svm.2)
# Mean Squared Errors:
#   0.5793231 0.5607735 0.5742847 0.5558541 0.5925696 ???
x.val <- data.validation[,-60]
y.val <- data.validation[,60]
preds.validation <- predict(svm.2, x.val)
head(preds.validation)
success.rate(round(preds.validation), y.val)


# SVM options
# `scale` - a logical vector (e.g. c(TRUE,TRUE,FALSE,...)) indicating variables to be scaled
#           default is all are scaled.
# `type` - we should be using `C-classification` or `nu-classification` - 
           # `nu-classification` minimizes the error function (why wouldn't we want to do that??)
# `kernel` - defaulted to radial, may also want to try polynomial and sigmoid
# `degree` - used for polynomial kernel (defaults to 3)
# `gamma` - defaults to 1, not sure what it's used for but it's used for everything but linear
# `coef0` -	(copy/pasted from rdocs) parameter needed for kernels of type polynomial and sigmoid (default: 0)
# `cost`-	(copy/pasted from rdocs) cost of constraints violation (default: 1)—it is the ‘C’-constant of the regularization term in the Lagrange formulation.
# (the rest as well, no more typing plz)
# `nu` -	parameter needed for nu-classification and one-classification
# `class.weights` -	a named vector of weights for the different classes, used for asymetric class sizes. Not all factor levels have to be supplied (default weight: 1). All components have to be named.
# `cachesize`	- cache memory in MB (default 40)
# `tolerance` -	tolerance of termination criterion (default: 0.001)
# `epsilon` -	epsilon in the insensitive-loss function (default: 0.1)
# `shrinking` -	option whether to use the shrinking-heuristics (default: TRUE)
# `cross` -	if a integer value k>0 is specified, a k-fold cross validation on the training data is performed to assess the quality of the model: the accuracy rate for classification and the Mean Sqared Error for regression
# `fitted`	logical indicating whether the fitted values should be computed and included in the model or not (default: TRUE)
# `probability` -	logical indicating whether the model should allow for probability predictions.

svm.3 <- svm(x, y = y, type = 'C-classification')
summary(svm.3)
preds <- predict(svm.3, x)
head(preds)
success.rate(preds, y)
# [1] 0.560375
x.val <- data.validation[,-60]
y.val <- data.validation[,60]
preds.validation <- predict(svm.3, x.val)
success.rate(preds.validation, y.val)
# [1] 0.5006667

# new kernel type
svm.4 <- svm(x, y = y, type = 'C-classification', kernel = 'polynomial')
summary(svm.4)
#
preds <- fitted(svm.4)
head(preds)
success.rate(preds, y)
# [1] 0.5899583
x.val <- data.validation[,-60]
y.val <- data.validation[,60]
preds.validation <- predict(svm.4, x.val)
success.rate(preds.validation, y.val)
# 0.494667

# new gamma
svm.5 <- svm(x,
             y = y,
             type = 'C-classification',
             gamma = 0.1)
summary(svm.5)
# Expect high predictive power with higher gamma
preds <- fitted(svm.5)
head(preds)
success.rate(preds, y)
# 94.667
x.val <- data.validation[,-60]
y.val <- data.validation[,60]
preds.validation <- predict(svm.5, x.val)
success.rate(preds.validation, y.val)
# 48.9333

# new kernel type + new gamma
# It appears gamma controls the fit to the data
# E.g. a higher gamma means in-sample prediction is higher
# You can capture 99% with a gamma of 0.1, but out of sample would go down
svm.6 <- svm(x, y = y, 
             type = 'C-classification',
             kernel = 'polynomial',
             gamma = 0.1)
summary(svm.6)
#
preds <- fitted(svm.6)
head(preds)
success.rate(preds, y)
# [1] 0.99
x.val <- data.validation[,-60]
y.val <- data.validation[,60]
preds.validation <- predict(svm.6, x.val)
success.rate(preds.validation, y.val)
# 0.4189


# Try tuning the polynomial

# See which does better in cross-validation radial v polynomial
svm.7 <- svm(x,
             y = y,
             type = 'C-classification',
             cross = 4)
summary(svm.7)
# Total Accuracy: 49.85 
#Single Accuracies:
#  48.93333 49.93333 50.73333 49.8

svm.8 <- svm(x,
             y = y,
             type = 'C-classification',
             kernel = 'polynomial',
             cross = 4)
summary(svm.8)
# Total Accuracy: 48.33333 
# Single Accuracies:
#   46.06667 51.06667 47.66667 48.53333 

# So radial and polynomial do approximately the same, but just to make sure we're not missing anything, we can try different degrees of polynomial
svm.9 <- svm(x,
             y = y,
             type = 'C-classification',
             kernel = 'polynomial',
             degree = 2, # default is 3
             cross = 4)
summary(svm.9)
# Total Accuracy: 48.58333 
# Single Accuracies:
#   49.73333 46.4 49.8 48.4 

svm.10 <- svm(x,
             y = y,
             type = 'C-classification',
             kernel = 'polynomial',
             degree = 4, # default is 3
             cross = 4)
summary(svm.10)
# Total Accuracy: 46.08333 
# Single Accuracies:
#   46.93333 45.53333 45.46667 46.4 

svm.11 <- svm(x,
              y = y,
              type = 'C-classification',
              kernel = 'polynomial',
              degree = 5, # default is 3
              cross = 4)
summary(svm.11)
# Total Accuracy: 46.01667 
# Single Accuracies:
#   46.53333 45.6 46.13333 45.8

# another option for polynomial is the coef0 option which defaults to 0
svm.12 <- svm(x,
              y = y,
              type = 'C-classification',
              kernel = 'polynomial',
              coef0 = 2,
              cross = 4)
summary(svm.12)
# Total Accuracy: 47.26667 
# Single Accuracies:
#   47.53333 48.26667 47.73333 45.53333

svm.13 <- svm(x,
              y = y,
              type = 'C-classification',
              kernel = 'sigmoid',
              cross = 4)
summary(svm.13)
# Total Accuracy: 45.88333 
# Single Accuracies:
#   47 46 45.86667 44.66667

# Seems like radial is our best bet
# having a large gamma performed poorly OOS
# it defaults to 1/datadimension, so maybe we will trye 2 times this
gamma <- 2*1/ncol(x)
svm.14 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              cross = 4)
summary(svm.14)
# Total Accuracy: 49.68333 
# Single Accuracies:
#   49.46667 49.33333 50.33333 49.6 

gamma <- 3*1/ncol(x)
svm.15 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              cross = 4)
summary(svm.15)
# Total Accuracy: 49.5 
# Single Accuracies:
#   48.93333 49.33333 50.13333 49.6

# Default actually seems to be best, perhaps we should make it even smaller
gamma <- 1/(2*ncol(x))
svm.16 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              cross = 4)
summary(svm.16)
# Total Accuracy: 50.08333 
# Single Accuracies:
#   49.93333 51.66667 50.53333 48.2 

gamma <- 1/(3*ncol(x))
svm.17 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              cross = 4)
summary(svm.17)

gamma <- 1/(4*ncol(x))
svm.18 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              cross = 4)
summary(svm.18)
# Total Accuracy: 49.73333 
# Single Accuracies:
#   48.93333 51 50.06667 48.93333
# Small gamma is better

# Try w/o shrinking heuristics, although not sure this is a good idea
svm.19 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              shrinking = FALSE,
              cross = 4)
summary(svm.19)
# Total Accuracy: 50.06667 
# Single Accuracies:
#   49.93333 50.06667 50 50.26667

svm.20 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              shrinking = FALSE,
              tolerance = 0.0001, # default: 0.001
              cross = 4)
summary(svm.20)
# Total Accuracy: 50.1 
# Single Accuracies:
#   49.53333 51.2 49.4 50.26667 

svm.21 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              shrinking = FALSE,
              tolerance = 0.00001, # default: 0.001
              cross = 4)
summary(svm.21)
# Total Accuracy: 50.25 
# Single Accuracies:
#   50.66667 50.13333 50.4 49.8 


svm.22 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              shrinking = FALSE,
              tolerance = 0.000001, # default: 0.001
              cross = 4)
summary(svm.22)
# Total Accuracy: 49.85 
# Single Accuracies:
#   51.06667 50.6 51.13333 46.6

tolerance <- 0.00001

# Try everything so far on a slightly larger dataset
limit <- (nrow(data.train)/2)
x <- data.train[1:limit,-60]
y <- data.train[1:limit,60]

svm.23 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              shrinking = FALSE,
              tolerance = tolerance, # default: 0.001
              cross = 4)
summary(svm.23)
# Total Accuracy: 49.625 
# Single Accuracies:
#   48.13333 49.8 50.13333 50.43333 

limit <- (nrow(data.train)/4)
x <- data.train[1:limit,-60]
y <- data.train[1:limit,60]
svm.24 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              shrinking = FALSE,
              tolerance = tolerance, # default: 0.001
              epsilon = 0.01, # default is 0.1, care less
              cross = 4)
summary(svm.24)
# Total Accuracy: 50 
# Single Accuracies:
#   49.2 48.86667 51.66667 50.26667 

svm.25 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              shrinking = FALSE,
              tolerance = tolerance, # default: 0.001
              epsilon = 0.01, # default is 0.1, care less
              cost = 0.1, # default is 1
              cross = 4)
summary(svm.25)
# Total Accuracy: 46.46667 
# Single Accuracies:
#   47.46667 45.93333 45.46667 47 

svm.26 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              shrinking = FALSE,
              tolerance = tolerance, # default: 0.001
              epsilon = 0.01, # default is 0.1, care less
              cost = 2, # default is 1
              cross = 4)
summary(svm.26)
# Total Accuracy: 50.16667 
# Single Accuracies:
#   49.73333 49.93333 50.4 50.6

svm.27 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              shrinking = FALSE,
              tolerance = tolerance, # default: 0.001
              epsilon = 0.01, # default is 0.1, care less
              cost = 5, # default is 1
              cross = 4)
summary(svm.27)
# Total Accuracy: 50.13333 
# Single Accuracies:
#   48.86667 51.73333 50.26667 49.66667

limit <- (nrow(data.train)/2)
x <- data.train[1:limit,-60]
y <- data.train[1:limit,60]

svm.27 <- svm(x,
              y = y,
              type = 'C-classification',
              gamma = gamma,
              shrinking = FALSE,
              tolerance = tolerance, # default: 0.001
              epsilon = 0.01, # default is 0.1, care less
              cost = 2, # default is 1
              cross = 4)
summary(svm.27)
# Total Accuracy: 50.03333 
# Single Accuracies:
#   50.33333 49.76667 49.26667 50.76667