# cleanup before you start
rm(list=ls())

if (!require('mvtnorm')) install.packages('mvtnorm')
if (!require('nnet')) install.packages('nnet')
if (!require('ade4')) install.packages('ade4')

setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

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
rep.col<-function(x,n){
  matrix(rep(x,each=n), ncol=n, byrow=TRUE)
}
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
  new.model <- multinom(y ~ ., data = new.data, maxit = 10)
  (new.loglik <- loglik(new.model))
}

