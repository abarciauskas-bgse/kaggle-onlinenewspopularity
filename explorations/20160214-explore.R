if (!require('randomForest')) install.packages('randomForest')
setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')
source('../utilities.R')

# setup
data.train <- data.matrix(data.train)
data.validation <- data.matrix(data.validation)

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
data.train$popularity <- factor(data.train$popularity)
iters <- 100
seeds <- round(runif(iters)*1000)
success.rates.by.seeds <- matrix(data=list(seeds=seeds,sr=rep(0,length(seeds))), nrow=length(seeds), ncol=2)

for (iter in 1:iters) {
  # set seed
  current.seed <- seeds[iter]
  print(paste0('Working on iteration: ', iter, ', seed: ', current.seed))
  set.seed(current.seed)
  # run random forest on training data
  base.model.rf <- randomForest(popularity ~ ., data=data.train)
  # predict on validation data
  preds.val <- predict(base.model.rf, data.validation[,1:59])
  
  sr.val <- success.rate(preds.val, data.validation[,'popularity'])
  print(paste0('finished training and predicting, success rate: ', sr.val))
  # store results 
  
  success.rates.by.seeds[iter,2] <- sr.val
}

