if (!require('xgboost')) {
  install.packages("drat", repos="https://cran.rstudio.com")
  drat:::addRepo("dmlc")
  install.packages("xgboost", repos="http://dmlc.ml/drat/", type="source")
}
setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

# setup
data.train <- data.matrix(data.train)
data.validation <- data.matrix(data.validation)
# xgboost wants 0-4
data.train[,'popularity'] <- data.train[,'popularity'] - 1
data.validation[,'popularity'] <- data.validation[,'popularity'] - 1
dtrain <- xgb.DMatrix(data.train[,1:59], label = data.train[,'popularity'])

base.model <- xgb.train(
  params = list(num_class = 5, objective = "multi:softmax"),
  data = dtrain,
  nrounds = 20,
  nthread = 2)

preds <-predict(base.model, data.validation[,1:59]) + 1
table(preds)
success.rate(preds, data.validation[,'popularity']+1)
# 0.5156667


# Find best max depth
depths <- seq(0,6,1)
depth.success.rates <- list()
for (i in 1:length(depths)) {
  depth <- depths[i]
  print(paste0('Running for depth: '), depth)
  model <- xgb.train(
    params = list(
      num_class = 5,
      objective = "multi:softmax",
      max.depth = depth),
    data = dtrain,
    nrounds = 20,
    nthread = 2)
  
  preds <-predict(model, data.validation[,1:59]) + 1
  sr <- success.rate(preds, data.validation[,'popularity']+1) 
  depth.success.rates[as.character(depth)] <- sr
  print(paste0('Success Rate: ', sr, ' using depth ', depth))
}

plot(unlist(depth.success.rates), pch = 19)
best <- which.max(depth.success.rates)
best.depth <- names(unlist(depth.success.rates)[best])


etas <- seq(0,1,0.1)
etas.success.rates <- list()
for (i in 1:length(etas)) {
  eta <- etas[i]
  print(paste0('Running for eta: '), eta)
  model <- xgb.train(
    params = list(
      num_class = 5,
      objective = "multi:softmax",
      max.depth = best.depth,
      eta = eta),
    data = dtrain,
    nrounds = 20,
    nthread = 2)
  
  preds <-predict(model, data.validation[,1:59]) + 1
  sr <- success.rate(preds, data.validation[,'popularity']+1) 
  etas.success.rates[as.character(eta)] <- sr
  print(paste0('Success Rate: ', sr, ' using eta ', eta))
}

plot(unlist(etas.success.rates), pch = 19)
best <- which.max(etas.success.rates)
best.eta <- names(unlist(etas.success.rates)[best])

# Maximum delta step we allow each tree’s weight estimation to be. If the
# value is set to 0, it means there is no constraint. If it is set to a
# positive value, it can help making the update step more conservative.
# Usually this parameter is not needed, but it might help in logistic
# regression when class is extremely imbalanced. Set it to value of 1-10 might
# help control the update
max_delta_step <- seq(0,10,1)
max_delta_step.success.rates <- list()
for (i in 1:length(max_delta_step)) {
  delta <- max_delta_step[i]
  print(paste0('Running for delta: '), delta)
  model <- xgb.train(
    params = list(
      num_class = 5,
      objective = "multi:softmax",
      max.depth = best.depth,
      eta = best.eta,
      max_delta_step = delta),
    data = dtrain,
    nrounds = 20,
    nthread = 2)
  
  preds <-predict(model, data.validation[,1:59]) + 1
  sr <- success.rate(preds, data.validation[,'popularity']+1) 
  max_delta_step.success.rates[as.character(delta)] <- sr
  print(paste0('Success Rate: ', sr, ' using delta ', delta))
}

plot(unlist(max_delta_step.success.rates), pch = 19)
best <- which.max(max_delta_step.success.rates)
best.delta <- names(unlist(max_delta_step.success.rates)[best])


gammas <- seq(0,10,1)
gammas.success.rates <- list()

for (i in 1:length(gammas)) {
  gamma <- gammas[i]
  print(paste0('Running for gamma: '), gamma)
  model <- xgb.train(
    params = list(
      num_class = 5,
      objective = "multi:softmax",
      max.depth = best.depth,
      eta = best.eta,
      max_delta_step = best.delta,
      gamma = gamma),
    data = dtrain,
    nrounds = 20,
    nthread = 2)
  
  preds <-predict(model, data.validation[,1:59]) + 1
  sr <- success.rate(preds, data.validation[,'popularity']+1) 
  gammas.success.rates[as.character(gamma)] <- sr
  print(paste0('Success Rate: ', sr, ' using gamma ', gamma))
}

plot(unlist(gammas.success.rates), pch = 19)
best <- which.max(gammas.success.rates)
best.gamma <- names(unlist(gammas.success.rates)[best])

# CV
params <- list(
  num_class = 5,
  objective = "multi:softmax",
  max.depth = best.depth,
  eta = best.eta,
  max_delta_step = best.delta,
  gamma = best.gamma)
res <- xgb.cv(params, dtrain, nround=2, nfold=10, metrics={'merror'})

