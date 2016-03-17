if (!require('randomForest')) install.packages('randomForest')

setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

data.test <- read.csv('../data/news_popularity_test.csv')
# remove id and url
x.test <- data.test[,3:ncol(data.test)]

classwts <- sapply(1:5, function(cl) {
  sum(data[,'popularity'] == cl)/nrow(data)
})
data.x <- data[,setdiff(colnames(data), 'popularity')]
data.y <- data[,'popularity']
set.seed(0214)
final.forest <- randomForest(data.x, data.y, classwt = classwts)

final.preds <- predict(final.forest, x.test)

# compare with ->
submission <- read.csv('16032016-2-final-predictions.csv')
