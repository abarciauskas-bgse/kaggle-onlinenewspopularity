if (!require('foreach')) install.packages('foreach')
if (!require('randomForest')) install.packages('randomForest')
if (!require('caret')) install.packages('caret')

setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

# List of potential models
potential.models <- c('rf',
                      'gbm',
                      'svmRadial',
                      'kknn',
                      'adabag',
                      'multinom',
                      'glmnet',
                      'pda',
                      'rbf',
                      'qrf',
                      'rFerns',
                      'rfRules',
                      'rknn',
                      'xgbTree')


# Small amount of data to make testing faster
partition <- createDataPartition(data$popularity, times = 2, p = 0.10)
data.train <- data[partition$Resample1,]
data.test <- data[partition$Resample2,]
# save train and test idcs

my_control <- trainControl(
  method="cv",
  number=3,
  index=createResample(data.train$popularity, times = 3)
)

# split data into features and class, easier for caret
data.train.x <- data.train[, setdiff(colnames(data.train), 'popularity')]
data.train.y <- data.train[,'popularity']
(ts <- as.numeric(Sys.time()))
# save indices
write.csv(do.call(cbind, partition), paste0('datadir/', ts, 'dataidcs.csv'), row.names = FALSE)

PP <- c('center', 'scale')
rf.model <- train(data.train.x,
  data.train.y, method='rf',
  trControl=my_control,
  preProcess = PP,
  search = 'random',
  tuneLength = 10)
# make and save predictions
rf.predictions <- cbind(partition$Resample2, predict(rf.model, data.test))
colnames(rf.predictions) <- c('training.data.idx', 'rf.pred')
write.csv(rf.predictions, paste0('datadir/', ts, 'rfpreds.csv'), row.names = FALSE)
write(as.character(rf.model$bestTune), paste0('datadir/', ts, 'rftune.txt'))


gbm.model <- train(data.train.x, data.train.y,
  method='gbm',
  trControl=my_control,
  distribution = 'multinomial',
  tuneLength = 10)
gbm.predictions <- cbind(
  partition$Resample2,
  predict(gbm.model, data.test[,setdiff(colnames(data.test), 'popularity')], n.trees = as.numeric(gbm.model$bestTune['n.trees'])))
colnames(gbm.predictions) <- c('training.data.idx', 'gbm.pred')
write.csv(gbm.predictions, paste0('datadir/', ts, 'gbmpreds.csv'), row.names = FALSE)
write(as.character(gbm.model$bestTune), paste0('datadir/', ts, 'gbmtune.txt'))


knn.model <- train(data.train.x, data.train.y,
  method='knn',
  trControl=my_control,
  preProcess=PP,
  tuneLength = 10)
knn.predictions <- cbind(partition$Resample2, predict(knn.model, data.test.x))
colnames(knn.predictions) <- c('training.data.idx', 'knn.pred')
write.csv(knn.predictions, paste0('datadir/', ts, 'knnpreds.csv'), row.names = FALSE)
write(as.character(knn.model$bestTune), paste0('datadir/', ts, 'knntune.txt'))


adabag.model <- train(data.train.x, data.train.y,
  method='AdaBag',
  trControl=my_control,
  preProcess=PP,
  search = 'random',
  tuneLength = 10)
adabag.predictions <- cbind(partition$Resample2, predict(adabag.model, data.test))
colnames(adabag.predictions) <- c('training.data.idx', 'adabag.pred')
write.csv(adabag.predictions, paste0('datadir/', ts, 'adabagpreds.csv'), row.names = FALSE)
write(as.character(adabag.model$bestTune), paste0('datadir/', ts, 'adabagtune.txt'))


