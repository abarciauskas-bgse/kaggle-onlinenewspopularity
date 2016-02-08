## SETUP
filepath <- '~/Projects/kaggle-onlinenewspopularity/data/'
setwd(filepath)
if (!require('e1071')) install.packages('e1071')
library(e1071)
source('../explorations/setup.R')

x <- data.train[,-60]
y <- data.train[,60]

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
svm.5 <- svm(x, y = y, type = 'C-classification', gamma = 0.1)
summary(svm.5)
#
preds <- fitted(svm.5)
head(preds)
success.rate(preds, y)
# 
x.val <- data.validation[,-60]
y.val <- data.validation[,60]
preds.validation <- predict(svm.5, x.val)
success.rate(preds.validation, y.val)


# new kernel type + new gamma
svm.4 <- svm(x, y = y, type = 'C-classification', kernel = 'polynomial', gamma = 0.1)
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