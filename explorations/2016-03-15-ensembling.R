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
rf.model <- train(data.train.x, data.train.y, method='rf', trControl=my_control, preProcess = PP)
# make and save predictions
rf.predictions <- cbind(partition$Resample2, predict(rf.model, data.test))
colnames(rf.predictions) <- c('training.data.idx', 'rf.pred')
write.csv(rf.predictions, 'datadir/rfpreds.csv', row.names = FALSE)
write(as.character(rf.model$bestTune), 'datadir/rftune.txt')

gbm.model <- train(data.train.x, data.train.y, method='gbm', trControl=my_control, distribution = 'multinomial')
gbm.predictions <- cbind(
  partition$Resample2,
  predict(gbm.model, data.test[,setdiff(colnames(data.test), 'popularity')], n.trees = as.numeric(gbm.model$bestTune['n.trees'])))
colnames(gbm.predictions) <- c('training.data.idx', 'gbm.pred')
write.csv(gbm.predictions, 'datadir/gbmpreds.csv', row.names = FALSE)
write(as.character(gbm.model$bestTune), 'datadir/gbmtune.txt')

knn.model <- train(data.train.x, data.train.y, method='knn', trControl=my_control, preProcess=PP)
knn.predictions <- cbind(partition$Resample2, predict(knn.model, data.test[,setdiff(colnames(data.test),'popularity')]))
colnames(knn.predictions) <- c('training.data.idx', 'knn.pred')
write.csv(knn.predictions, 'datadir/knnpreds.csv', row.names = FALSE)
write(as.character(knn.model$bestTune), 'datadir/knntune.txt')


# :( FAILED options type = C-classification, kernel = 'linear', 'polynomial', 
# svmRadial.model <- train(data.train.x, data.train.y, method='svmRadial', trControl=my_control, preProcess=PP)
# adabag is SLOW
adabag.model <- train(data.train.x, data.train.y, method='AdaBag', trControl=my_control, preProcess=PP)
adabag.predictions <- cbind(partition$Resample2, predict(adabag.model, data.test))
colnames(adabag.predictions) <- c('training.data.idx', 'adabag.pred')
write.csv(adabag.predictions, 'datadir/adabagpreds.csv', row.names = FALSE)
write(as.character(adabag.model$bestTune), 'datadir/adabagtune.txt')

multinom.model <- train(data.train.x, data.train.y, method='multinom', trControl=my_control, preProcess=PP)
multinom.predictions <- cbind(partition$Resample2, predict(multinom.model, data.test))
colnames(multinom.predictions) <- c('training.data.idx', 'multinom.pred')
write.csv(multinom.predictions, 'datadir/multinompreds.csv', row.names = FALSE)
write(as.character(multinom.model$bestTune), 'datadir/multinomtune.txt')

glmnet.model <- train(data.train.x, data.train.y, method='glmnet', trControl=my_control, preProcess=PP)
glmnet.predictions <- cbind(partition$Resample2, predict(glmnet.model, data.test[,setdiff(colnames(data.test), 'popularity')]))
colnames(glmnet.predictions) <- c('training.data.idx', 'glmnet.pred')
write.csv(glmnet.predictions, 'datadir/glmnetpreds.csv', row.names = FALSE)
write(as.character(glmnet.model$bestTune), 'datadir/glmnettune.txt')

#doesn't work
#glmboost.model <- train(data.train.x, data.train.y, method='glmboost', trControl=my_control, preProcess=PP,
# family = Multinomial())
pda.model <- train(data.train.x, data.train.y, method='pda', trControl=my_control, preProcess=PP,
  tuneGrid = expand.grid(lambda = seq(0.1,0.9,0.1)))
pda.predictions <- cbind(partition$Resample2, predict(pda.model, data.test))
colnames(pda.predictions) <- c('training.data.idx', 'pda.pred')
write.csv(pda.predictions, 'datadir/pdapreds.csv', row.names = FALSE)
write(as.character(pda.model$bestTune), 'datadir/pdatune.txt')

# can't install RSNNS
# rbf.model <- train(data.train.x, data.train.y, method='rbf', trControl=my_control, preProcess=PP)
rFerns.model <- train(data.train.x, data.train.y, method='rFerns', trControl=my_control, preProcess=PP)
rfFerns.predictions <- cbind(partition$Resample2, predict(rFerns.model, data.test))
colnames(rfFerns.predictions) <- c('training.data.idx', 'rfFerns.pred')
write.csv(rfFerns.predictions, 'datadir/rfFernspreds.csv', row.names = FALSE)
write(as.character(rFerns.model$bestTune), 'datadir/rfFernstune.txt')

# super slow
#   mtry maxdepth
#     59        4
rfRules.model <- train(
  data.train.x,
  data.train.y,
  method='rfRules',
  trControl=my_control,
  tuneGrid = expand.grid(mtry = 59, maxdepth = 4),
  preProcess=PP)
rfRules.predictions <- cbind(partition$Resample2, predict(rfRules.model, data.test))
colnames(rfRules.predictions) <- c('training.data.idx', 'rfRules.pred')
write.csv(rfRules.predictions, 'datadir/rfRulespreds.csv', row.names = FALSE)
write(as.character(rfRules.model$bestTune), 'datadir/rfRulestune.txt')

# failed to install rknn and gmp
# rknn.model <- train(data.train.x, data.train.y, method='rknn', trControl=my_control, preProcess=PP)

# failed ton install xgboost
# xgbTree.model <- train(data.train.x, data.train.y, method='xgbTree', trControl=my_control, preProcess=PP)


# moremore

# For every model, put a prior probability based on the accuracy
models <- list(
  rf.model,
  gbm.model,
  knn.model,
  adabag.model,
  multinom.model,
  glmnet.model,
  pda.model,
  rFerns.model,
  rfRules.model)

# find least correlated models and start with their prediction as an ensemble to beat
all.predictions <- do.call(cbind, list(
  rf.predictions[,2],
  gbm.predictions[,2],
  knn.predictions[,2],
  adabag.predictions[,2],
  multinom.predictions[,2],
  glmnet.predictions[,2],
  pda.predictions[,2],
  rFerns.predictions[,2],
  rfRules.predictions[,2]))
colnames(all.predictions) <- c('rf','gbm','knn','adabag','multinom','glmnet','pda','rFerns','rfRules')

# intial predictive accuracy
test.accuracies <- apply(all.predictions, 2, function(preds) {
  success.rate(preds, data.test[,'popularity'])
})

cor(all.predictions)
# by inspection of accuracies and correlations, we find that the least correlated predictors are knn, rfRules and rf
# I exclude rFerns from this because it's accuracy is so low, must be doing something wrong.

init.ensemble <- all.predictions[,c('rf','rf','knn','rFerns','rfRules')]
init.ensemble.preds <- apply(init.ensemble, 1, my.mode)
success.rate(init.ensemble.preds, data.test[,'popularity'])
# still lower than rf alone


