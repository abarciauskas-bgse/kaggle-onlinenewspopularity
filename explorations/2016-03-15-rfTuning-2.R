if (!require('foreach')) install.packages('foreach')
if (!require('randomForest')) install.packages('randomForest')
if (!require('caret')) install.packages('caret')

setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

# rf is pulling weight, so going to see how much we can tune and improve it

partition <- createDataPartition(data$popularity, times = 2, p = 0.20)
data.train <- data[partition$Resample1,]
data.test <- data[partition$Resample2,]

assertthat(union(partition$Resample1, partition$Resample2) == integer(0))
data.train.x <- data.train[, setdiff(colnames(data.train), 'popularity')]
data.train.y <- data.train[,'popularity']
data.test.x <- data.test[, setdiff(colnames(data.train), 'popularity')]
data.test.y <- data.test[,'popularity']

# tune.rf <- tuneRF(data.train.x, data.train.y, 1, ntreeTry=50, stepFactor=2, improve=0.05, trace=TRUE, plot=TRUE)

# not too exciting so we will try our own grid search

# options:
# ntree, mtry, replace, classwt, nodesize, maxnodes, importance, proximity
base.forest <- randomForest(data.train.x, data.train.y, mtry = 2)
base.preds <- predict(base.forest, data.test)
success.rate(base.preds, data.test.y)

classwts <- sapply(1:5, function(cl) {
  sum(data.train.y == cl)/length(data.train.y)
})

weighted.forest <- randomForest(data.train.x, data.train.y, mtry = 2, classwt = classwts)
weighted.preds <- predict(weighted.forest, data.test)
success.rate(weighted.preds, data.test.y)
# slight improvement


imp.forest <- randomForest(data.train.x, data.train.y, mtry = 2, importance = TRUE)
imp.preds <- predict(imp.forest, data.test)
success.rate(imp.preds, data.test.y)
# no improvement

impwt.forest <- randomForest(data.train.x, data.train.y, mtry = 2, classwt = classwts, importance = TRUE)
impwt.forest <- predict(impwt.forest, data.test)
success.rate(impwt.forest, data.test.y)
# nope

# so just try to improve over weighted forest
mtries <- seq(2,10,1)
mtry.accuracies <- matrix(NA, nrow = length(mtries), ncol = 2)
colnames(mtry.accuracies) <- c('mtry', 'acc')
for (i in 1:length(mtries)) {
  mtry <- mtries[i]
  print(paste0('trying:', mtry))
  m.forest <- randomForest(data.train.x, data.train.y, mtry = mtry, classwt = classwts)
  acc <- success.rate(predict(m.forest, data.test), data.test.y)
  mtry.accuracies[i,'mtry'] <- mtry
  mtry.accuracies[i,'acc'] <- acc
}

write.csv(mtry.accuracies, 'datadir/mtryaccuracies.csv', row.names = FALSE)
ntrees <- c(10,20,40,80,160,320,640)
ntrees.accuracies <- matrix(NA, nrow = length(ntrees), ncol = 2)
colnames(ntrees.accuracies) <- c('ntrees', 'acc')

for (i in 1:length(ntrees)) {
  ntree <- ntrees[i]
  print(paste0('trying:', ntree))
  t.forest <- randomForest(data.train.x, data.train.y, mtry = 2, ntrees = ntree, classwt = classwts)
  acc <- success.rate(predict(m.forest, data.test), data.test.y)
  ntrees.accuracies[i,'ntrees'] <- ntree
  ntrees.accuracies[i,'acc'] <- acc
}
write.csv(ntrees.accuracies, 'datadir/ntreesaccuracies.csv', row.names = FALSE)


nodesize.forest <- randomForest(data.train.x, data.train.y, mtry = 2, nodesize = 2, classwt = classwts)
nodesize.preds <- predict(nodesize.forest, data.test)
success.rate(nodesize.preds, data.test.y)
