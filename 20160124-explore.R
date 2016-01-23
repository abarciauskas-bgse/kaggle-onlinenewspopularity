if (!require('class')) install.packages('class')
library(class)
if (!require('MASS')) install.packages('MASS')
library(MASS)

# setwd('~/Projects/kaggle-onlinenewspopularity/data/')

# Read in data
data.train <- read.csv('news_popularity_training.csv')
nobs <- nrow(data.train)

# Remove id and url
data.train <- data.train[,3:ncol(data.train)]

# Use 20% for validation
validation.indices <- sample(nobs, 0.2*nobs)
train.indices <- setdiff(1:nobs, validation.indices)
data.validation <- data.train[validation.indices,]
data.train <- data.train[train.indices,]

# Function which takes `predictions` as vector of predictions
# And `actual` as vector of actual labels
# And returns a rate of success
success.rate <- function(predictions, actual) {
  errors <- 0
  # count number of mistakes
  for (i in 1:length(actual)) {
    if (predictions[i] != actual[i]) {
      errors <- errors + 1
    }
  }
  return (1-errors/length(actual))
}

# Generate 1 to 10 k-nn models and see which one performs best
# WARNING: Takes a while to run
ks <- 1:20
pred.success.rates <- list()
for (k in 1:length(ks)) {
  res <- knn(data.train[,1:(ncol(data.train)-1)],
             data.validation[,1:(ncol(data.train)-1)],
             data.train[,ncol(data.train)], k = ks[k])

  # Percentage correctly identified
  pred.success.rates[paste0('k-',ks[k])] <- success.rate(res, data.validation[,ncol(data.validation)])
}

# seems to plateau ~44% for k=19
pred.success.rates

# Logistic regression for 5 categories
X <- data.train[order(data.train[,'popularity']),]
X <- as.matrix(X[,1:(ncol(data.train)-1)])
countPerCategory <- list()
for (i in 1:5) {
  countPerCategory[toString(i)] <- nrow(subset(data.train, popularity == i))
}

Y <- cbind(cat1.target = c(rep(1, as.numeric(countPerCategory['1'])),
                           rep(0, as.numeric(countPerCategory['2'])),
                           rep(0, as.numeric(countPerCategory['3'])),
                           rep(0, as.numeric(countPerCategory['4'])),
                           rep(0, as.numeric(countPerCategory['5']))),
           cat2.target = c(rep(0, as.numeric(countPerCategory['1'])),
                           rep(1, as.numeric(countPerCategory['2'])),
                           rep(0, as.numeric(countPerCategory['3'])),
                           rep(0, as.numeric(countPerCategory['4'])),
                           rep(0, as.numeric(countPerCategory['5']))),
           cat3.target = c(rep(0, as.numeric(countPerCategory['1'])),
                           rep(0, as.numeric(countPerCategory['2'])),
                           rep(1, as.numeric(countPerCategory['3'])),
                           rep(0, as.numeric(countPerCategory['4'])),
                           rep(0, as.numeric(countPerCategory['5']))),
           cat4.target = c(rep(0, as.numeric(countPerCategory['1'])),
                           rep(0, as.numeric(countPerCategory['2'])),
                           rep(0, as.numeric(countPerCategory['3'])),
                           rep(1, as.numeric(countPerCategory['4'])),
                           rep(0, as.numeric(countPerCategory['5']))),
           cat5.target = c(rep(0, as.numeric(countPerCategory['1'])),
                           rep(0, as.numeric(countPerCategory['2'])),
                           rep(0, as.numeric(countPerCategory['3'])),
                           rep(0, as.numeric(countPerCategory['4'])),
                           rep(1, as.numeric(countPerCategory['5']))))
weightsOptim <- ginv(t(X)%*%X) %*% t(X) %*% Y
# there's probably a better way to do this, rounding is wrong
predictions.percents <- as.matrix(data.validation[,1:(ncol(data.train)-1)]) %*% weightsOptim
predictions = apply(predictions.percents, 1, which.max)
success.rate(predictions, data.validation[,ncol(data.validation)])
# 46.8%, not great but better than k-nn

# TODO:
#
# Feature reduction
# mutual information, and Fisher criterion


