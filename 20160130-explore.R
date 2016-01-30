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

# see what changes with popularity levels 1-3
data.train <- subset(data.train, popularity %in% c(1,2,3))
data.validation <- subset(data.validation, popularity %in% c(1,2,3))
ks <- c(200,300,400)

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
# k-100 gives 46.13%
# plateaus at 47%
