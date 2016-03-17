if (!require('foreach')) install.packages('foreach')
if (!require('randomForest')) install.packages('randomForest')
if (!require('caret')) install.packages('caret')

setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

# rf is pulling weight, so going to see how much we can tune and improve it

partition <- createDataPartition(data$popularity, times = 2, p = 0.10)
data.train <- data[partition$Resample1,]
data.test <- data[partition$Resample2,]
# save train and test idcs

my_control <- trainControl(
  method="cv",
  number=3,
  index=createResample(data.train$popularity, times = 3)
)

data.train.x <- data.train[, setdiff(colnames(data.train), 'popularity')]
data.train.y <- data.train[,'popularity']
data.test.x <- data.test[, setdiff(colnames(data.train), 'popularity')]
data.test.y <- data.test[,'popularity']


(ts <- as.numeric(Sys.time()))
# save indices
write.csv(do.call(cbind, partition), paste0('datadir/', ts, 'dataidcs.csv'), row.names = FALSE)

PP <- c('center','scale')
rrf.model <- train(data.train.x, data.train.y,
  method='RRF',
  trControl=my_control,
  preProcess = PP,
  search = 'random',
  tuneLength = 10)

# evaluate predictive power against the test set
success.rate(predict(rrf.model, data.test), data.test.y)

# random forest with feature selection
# TOO SLOW / DIDN'T RUN
boruta.model <- train(data.train.x, data.train.y,
  method='Boruta',
  trControl=my_control,
  preProcess = PP,
  tuneLength = 10)
success.rate(predict(boruta.model, data.test), data.test.y)

cforest.model <- train(data.train.x, data.train.y,
  method='cforest',
  trControl=my_control,
  preProcess = PP,
  tuneLength = 10)
success.rate(predict(cforest.model, data.test), data.test.y)


ranger.model <- train(data.train.x, data.train.y,
  method='ranger.model',
  trControl=my_control,
  preProcess = PP,
  tuneLength = 10)
success.rate(predict(ranger.model, data.test), data.test.y)


