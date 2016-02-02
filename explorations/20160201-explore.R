source('setup.R')
setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
# Do we get the same results for logit using the nnet package?
#
# As suggested in http://www.r-bloggers.com/how-to-multinomial-regression-models-in-r/
#
obs.weights <- rep(1, nrow(data.train))
model <- multinom(popularity ~ ., data = data.train, weights = obs.weights)
preds <- predict(model, newdata=data.validation)
(sr <- success.rate(preds, data.validation[,'popularity']))
# 0.494