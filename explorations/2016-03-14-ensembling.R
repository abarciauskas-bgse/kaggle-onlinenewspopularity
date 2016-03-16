setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
# IMPORTANT: loads data and factorizes 'popularity'
source('setup.R')

# copied from past file
fiftypct.rand.idcs <- sample(1:nrow(data), nrow(data)/2)
train.data <- data[fiftypct.rand.idcs,]
test.data <- data[-fiftypct.rand.idcs,]
train.x <- train.data[,setdiff(colnames(train.data), 'popularity')]
train.y <- train.data[,'popularity']
test.x <- test.data[,setdiff(colnames(test.data), 'popularity')]
test.y <- test.data[,'popularity']

# train multinom, rf and gbm
multinom.model <- multinom(popularity ~ ., train.data)
rf.model <- randomForest(train.x, train.y, mtry = 2)
gbm.model <- gbm(formula = popularity ~ .,
                 data = train.data,
                 distribution = 'multinomial',
                 n.trees = 50,
                 interaction.depth = 1,
                 shrinkage = 0.1,
                 n.minobsinnode = 10)

ensemble.preds <- lapply(list(multinom.model, rf.model), function(mod) {
  return(predict(mod, test.x))
})
ensemble.preds <- do.call(cbind, ensemble.preds)
gbm.preds <- apply(predict(gbm.model, test.x, n.trees = 50), 1, which.max)
ensemble.preds <- cbind(ensemble.preds, gbm.preds)
colnames(ensemble.preds) <- c('multinom','rf','gbm')
ensemble.preds.gathered <- apply(ensemble.preds, 1, my.mode)
success.rate(ensemble.preds.gathered, test.y)
# [1] "Number of errors: 7377"
# [1] "Rate: 0.5148
# [1] 0.5148


# ALL THE DATA
train.data <- data
test.data <- read.csv('../data/news_popularity_test.csv')
train.x <- train.data[,setdiff(colnames(train.data), 'popularity')]
train.y <- train.data[,'popularity']
test.x <- test.data[,setdiff(colnames(test.data), c('id', 'url','popularity'))]

if (!require('nnet')) install.packages('nnet')
if (!require('randomForest')) install.packages('randomForest')
if (!require('gbm')) install.packages('gbm')

multinom.model <- multinom(popularity ~ ., train.data)
rf.model <- randomForest(train.x, train.y, mtry = 2)
gbm.model <- gbm(formula = popularity ~ .,
                 data = train.data,
                 distribution = 'multinomial',
                 n.trees = 50,
                 interaction.depth = 1,
                 shrinkage = 0.1,
                 n.minobsinnode = 10)

ensemble.preds <- lapply(list(multinom.model, rf.model), function(mod) {
  return(predict(mod, test.x))
})
ensemble.preds <- do.call(cbind, ensemble.preds)
gbm.preds <- apply(predict(gbm.model, test.x, n.trees = 50), 1, which.max)
ensemble.preds <- cbind(ensemble.preds, gbm.preds)
colnames(ensemble.preds) <- c('multinom','rf','gbm')
(ensemble.preds.gathered <- apply(ensemble.preds, 1, my.mode))

predictions <- cbind(id=test.data[,'id'], popularity=ensemble.preds.gathered)
write.csv(predictions, '../submissions/14032016-2-predictions.csv', row.names = FALSE)
