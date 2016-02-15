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
  print(paste0('Running for depth: ', depth))
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
  print(paste0('Running for eta: ', toString(eta)))
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




gammas <- seq(0,10,1)
gammas.success.rates <- list()

for (i in 1:length(gammas)) {
  gamma <- gammas[i]
  print(paste0('Running for gamma: ', gamma))
  model <- xgb.train(
    params = list(
      num_class = 5,
      objective = "multi:softmax",
      max.depth = best.depth,
      eta = best.eta,
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
  gamma = best.gamma)
#res <- xgb.cv(params, dtrain, nround=2, nfold=10, metrics={'merror'})

# N-fold cross validation
# for every row in the training set, remove the row, run xgb.train
# If prediction goes up, remove the prediction from the overall set

best.depth <- 4
best.eta <- 0.2
best.gamma <- 0
(params = list(
  num_class = 5,
  objective = "multi:softmax",
  max.depth = best.depth,
  eta = best.eta,
  gamma = best.gamma))

model <- xgb.train(
  params,
  data = dtrain,
  nrounds = 2,
  nthread = 2)
preds <-predict(model, data.validation[,1:59]) + 1
(sr.0 <- success.rate(preds, data.validation[,'popularity']+1))

rows.to.remove <- c()
for (i in 1:nrow(dtrain)) {
  print(paste('training without row:', toString(i)))
  dtrain.new <- xgb.DMatrix(data.train[-i,1:59], label = data.train[-i,'popularity'])
  model <- xgb.train(
    params,
    data = dtrain.new,
    nrounds = 2,
    nthread = 2)
  preds <-predict(model, data.validation[,1:59]) + 1
  (sr.1 <- success.rate(preds, data.validation[,'popularity']+1))
  print(sr.1)
  if (sr.1 > sr.0) {
    rows.to.remove <- append(rows.to.remove, i)
  }
}

write.csv(rows.to.remove, paste0('potential-outliers', as.numeric(Sys.time()), '.csv'), row.names = FALSE)
# stopped at 14166

# Remove rows we think are outliers
# First load the files
setwd('~/Projects/kaggle-onlinenewspopularity/data/potential-outliers/')
file.prefix <- 'potential-outliers'
(filenames <- Sys.glob(paste0(file.prefix,"*")))
potential.outliers <- c()

for (i in 1:length(filenames)) {
  ids <- read.csv(filenames[i], stringsAsFactors = FALSE)
  potential.outliers <- append(potential.outliers, ids)
}

potential.outliers <- unlist(potential.outliers)

data.train.new <- data.train[-potential.outliers,]

# Try to train
data.train.new <- data.matrix(data.train.new)
dtrain.new <- xgb.DMatrix(data.train.new[,1:59], label = data.train.new[,'popularity'])

params[['data']] <- data.train.new
params[['nrounds']] <- 60
rates <- cross.val(model.function = xgb.train,
  model.args = params,
  data = data.train.new)

base.model.new <- xgb.train(
  params = list(num_class = 5, objective = "multi:softmax"),
  data = dtrain.new,
  nrounds = 20,
  nthread = 2)

preds <-predict(base.model.new, data.validation[,1:59]) + 1
table(preds)
success.rate(preds, data.validation[,'popularity']+1)
# 0.519

model <- xgb.train(
  params,
  data = dtrain.new,
  nrounds = 60,
  nthread = 2)
preds <-predict(model, data.validation[,1:59]) + 1
success.rate(preds, data.validation[,'popularity']+1)
# 0.5225

library(randomForest)
(seed <- runif(1))
set.seed(seed)

data.train.new <- data.train.outliers.removed

data.train.new <- data.frame(data.train.new)
data.train.new[,'popularity'] <- factor(data.train.new[,'popularity'])
data.validation <- data.frame(data.validation)
data.validation[,'popularity'] <- factor(data.validation[,'popularity'])

base.model.rf <- randomForest(popularity ~ ., data=data.train.new)
table(base.model.rf$predicted)
success.rate(base.model.rf$predicted, data.train.new$popularity)
preds.val <- predict(base.model.rf, data.validation[,1:59])
table(preds.val)
success.rate(preds.val, data.validation[,'popularity'])
# [1] 0.7993333 ?!?!
data.test <- read.csv('news_popularity_test.csv')
# remove id and url
x.test <- data.test[,3:ncol(data.test)]
preds <- predict(base.model.rf, x.test)
summary(preds)
predictions <- cbind(id=data.test[,'id'], popularity=preds)
# not yet submitted
write.csv(predictions, '02142016-1-predictions.csv', row.names = FALSE)

# Let's do with cross validation
rates <- cross.val(model.function=randomForest, 
                   model.args= list(
                     formula=popularity ~ ., 
                     data=data.train.new),
                   data=data.train.new)

# compare with original
data.train <- data.frame(data.train)
data.train$popularity <- factor(data.train$popularity)
base.model.rf <- randomForest(popularity ~ ., data=data.train)
preds <- predict(base.model.rf, data.validation[,1:59])
table(preds)
success.rate(preds, data.validation[,'popularity'])
# [1] 0.5173333

# Try different seeds
# Store seeds and results
iters <- 100
seeds <- runif(iters)*100
success.rates.by.seeds <- matrix(data=c(seeds,rep(0,length(seeds)), nrow=length(seeds), ncol=2))

for (iter in 1:iters) {
  # set seed
  current.seed <- seeds[iter]
  print(paste0('Working on iteration: ', iter, ', seed: ', current.seed))
  set.seed(current.seed)
  # run random forest on training data
  base.model.rf <- randomForest(popularity ~ ., data=data.train.new)
  # predict on validation data
  preds.val <- predict(base.model.rf, data.validation[,1:59])
  
  sr.val <- success.rate(preds.val, data.validation[,'popularity'])
  print(paste0('finished training and predicting, success rate: ', sr.val))
  # store results 
  
  success.rates.by.seeds[iter,2] <- sr.val
}

