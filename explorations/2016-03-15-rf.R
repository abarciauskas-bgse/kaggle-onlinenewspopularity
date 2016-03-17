# rf with classwts
if (!require('randomForest')) install.packages('randomForest')

setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

# get class weights
classwts <- sapply(1:5, function(cl) {
  sum(data[,'popularity'] == cl)/nrow(data)
})
mtry = 2

seeds <- c(187, 841, 20160315, 4569121, 45121)
rf.model <- randomForest(popularity ~ ., data = data, classwt = classwts, mtry = mtry)


data.test <- read.csv('../data/news_popularity_test.csv')
# remove id and url
x.test <- data.test[,3:ncol(data.test)]
preds <- predict(rf.model, x.test)
table(preds)
predictions <- cbind(id=data.test[,'id'], popularity=preds)
write.csv(predictions, '03152016-5-predictions.csv', row.names = FALSE)

(files <- Sys.glob("03152016*predictions.csv"))
preds.mat <- matrix(NA, nrow = nrow(x.test), ncol = length(files))
all.preds <- sapply(1:length(files), function(f.idx) {
  read.csv(files[f.idx])[,'popularity']
})

voted.preds <- apply(all.preds, 1, my.mode)
voted.predictions <- cbind(id=data.test[,'id'], popularity=voted.preds)
write.csv(voted.predictions, '03152016-final-predictions.csv', row.names = FALSE)



