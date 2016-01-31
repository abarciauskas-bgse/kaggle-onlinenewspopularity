setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

# Do we get the same results for logit using the nnet package?
#
# As suggested in http://www.r-bloggers.com/how-to-multinomial-regression-models-in-r/
#
obs.weights <- rep(1, nrow(data.train))
model <- multinom(popularity ~ ., data = data.train, weights = obs.weights)
preds <- predict(model, newdata=data.validation)
(sr <- success.rate(preds, data.validation[,'popularity']))
# [1] 0.5280083

# Try boosting with this model
nboost <- 10
# collect all predictions so we can take a majority vote?
# there should also be some weighting of them
preds <- matrix(data = NA, ncol = nboost, nrow = nrow(data.train))
alphas <- list()

for (i in 1:nboost) {
  model <- multinom(popularity ~ ., data = data.train, weights = obs.weights)
  preds[,i] <- as.numeric(predict(model))
  errs <- obs.weights*(preds[,i] != data.train[,'popularity'])
  errs.rate <- sum(errs)/length(errs)
  (sr <- success.rate(preds[,i], data.train[,'popularity']))
  # Make sure calculations are good (e.g.) (can't use with weights?)
  #assert_that(sr + errs.rate == 1)
  # Trying boosting but this didn't seem to help, sr went down
  alphas[i] <- 0.5*log((1-errs.rate)/errs.rate)
  dd <- ifelse(data.train[,'popularity'] == preds[,i], 1, -1)
  obs.weights <- exp(-alpha*dd)
  obs.weights <- obs.weights/sum(obs.weights)
}


# This did poorly (48%) but I haven't weighted the predictions by the goodness of the model making it
# which should be done
concensus <- c()
for (i in 1:nrow(preds)) {
  concensus <- append(concensus, round(median(preds[i,])))
}
