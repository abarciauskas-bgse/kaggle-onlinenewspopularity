# cleanup before you start
rm(list=ls())

if (!require('nnet')) install.packages('nnet')

setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

loglik <- function(model) {
  # deviance = -2 log likelihoods
  -model$deviance/2
}

max.iter <- 10
# TODO: Increase maxit?
base.model <- multinom(popularity ~ ., data = data.train, maxit = max.iter)
# Likelihood
# first vector of y.expanded * first vector of base.model$fitted.values
# should get something that is 1 x 5 or 5 x 1
# check that this is ok
(base.loglik <- loglik(base.model))

# setup x - features, y - popularity
x <- data.train[,setdiff(colnames(data.train), 'popularity')]
y <- data.train[,'popularity']

# iterate through features with high fisher score.
fisher.features <- c('kw_avg_avg','LDA_02','data_channel_is_world','is_weekend','data_channel_is_socmed','weekday_is_saturday',
                     'LDA_04','data_channel_is_entertainment','data_channel_is_tech','kw_max_avg','weekday_is_sunday','LDA_04',
                     'num_hrefs','global_subjectivity','kw_min_avg','global_sentiment_polarity','rate_negative_words','kw_min_min',
                     'title_subjectivity','LDA_01')

# for each feature with a high fisher score, interact it with everything
# estimate a model and calculate the log likelihood
# if higher than the base log likelihood, add the feature and it's interacted model's log lik to our history
good.interactions <- list()

for (j in 1:length(fisher.features)) {
  current.feature <- fisher.features[j]
  # interact it with everything
  all.other.features <- x[,setdiff(colnames(x), current.feature)]
  current.feature.vector <- x[,current.feature]
  interactions.mat <- matrix(0, ncol = ncol(all.other.features), nrow = nrow(x))
  for (f in 1:ncol(all.other.features)) {
    res <- current.feature.vector * all.other.features[,f]
    interactions.mat[,f] <- res
  }
  features.plus <- cbind(x, interactions.mat)
  new.data <- cbind(features.plus, y)
  new.model <- multinom(y ~ ., data = new.data, maxit = max.iter)
  (new.loglik <- loglik(new.model))
  if (new.loglik > base.loglik) {
    print(paste0("Adding: ", current.feature, ", loglik: ", new.loglik))
    good.interactions[[current.feature]] <- new.loglik
  }
}


good.interactions[['LDA_02']]

if (!require('gbm')) install.packages('gbm')
library(gbm)

# Sanity check - so far most likely feature interactions are with LDA_02
current.feature <- 'LDA_02'
all.other.features <- x[,setdiff(colnames(x), current.feature)]
current.feature.vector <- x[,current.feature]
interactions.mat <- matrix(0, ncol = ncol(all.other.features), nrow = nrow(x))
features.plus <- cbind(x, interactions.mat)

# takes a long time to run
n.trees = 2000
gbm1 <- gbm.fit(x = features.plus, y = y,
                distribution = 'multinomial',
                n.trees = n.trees,
                shrinkage = 0.1)
best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)
preds <- apply(predict(gbm1, x, best.iter), 1, which.max)
# 2000 trees vs best.iter trees (48) difference in success rate of 57.3 and 51.1
summary(preds)
success.rate(preds, y)


x.val <- data.validation[,setdiff(colnames(data.train), 'popularity')]
y.val <- data.validation[,'popularity']
all.other.features.val <- x.val[,setdiff(colnames(x), current.feature)]
current.feature.vector.val <- x.val[,current.feature]
interactions.mat.val <- matrix(0, ncol = ncol(all.other.features.val), nrow = nrow(x.val))
features.plus.val <- cbind(x.val, interactions.mat.val)

# Predict using all trees
preds.val.1 <- apply(predict(gbm1, features.plus.val, 2000), 1, which.max)
summary(preds.val.1)
success.rate(preds.val.1, y.val)
# Predict using best.iter trees
preds.val.2 <- apply(predict(gbm1, x.val, best.iter), 1, which.max)
summary(preds.val.2)
success.rate(preds.val.2, y.val)
# Interactions make NO difference

