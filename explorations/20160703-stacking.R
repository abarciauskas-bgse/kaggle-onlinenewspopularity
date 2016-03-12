setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

# Train on all the data
data.train <- data
# SHOULD THE DATA BE SCALED LIKE IN CARET??

kknn.fit <- kknn(popularity ~ .,
                 train = data.train[1:nrow(data.train),],
                 test = data.train[1,],
                 k = 9,
                 distance = 2,
                 kernel = 'optimal')

avnnet.fit <- avNNet(popularity ~ .,
                     data = data.train,
                     size = 1, decay = 0.1, bag = FALSE)

# should do a grid search of other parameters? there are a lot
system.time(
  rf.fit <- randomForest(popularity ~ ., data = data.train, mtry = 2)
)


test.data <- read.csv('../data/news_popularity_test.csv')
# remove id and url
x.test <- test.data[,3:ncol(test.data)]

# First attempt at stacking

# take predictions from each 3 fitted models
knn.preds <- predict(knn.fit, newdata = x.test)
rf.preds <- predict(rf.fit, newdata = x.test)
# from the caret file
avnnet.preds <- predict(avNNet.fit, newdata = x.test)

# three-party ensemble
# take a weighted majority, which basically amounts to taking majority vote as
# they all got around 50%, so take the most popular of the three models
ensemble.preds <- rep(NA, nrow(x.test))
for (i in 1:length(ensemble.preds)) {
  ensemble.preds[i] <- my.mode(c(avnnet.preds[i], rf.preds[i], knn.preds[i]))
}

table(ensemble.preds)
length(ensemble.preds)
predictions <- cbind(id=data.test[,'id'], popularity=ensemble.preds)
ts <- as.numeric(Sys.time())
write.csv(predictions, paste0(ts,'-predictions.csv'), row.names = FALSE)
