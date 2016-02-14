# Code for first submission
setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

# Use all the data
#data.train <- rbind(data.train, data.validation)

# I really just wanted to boost performance from baseline logistic (using adaptive boosting)
# But this appears to do a lot more:
# 'Boosting is the process of iteratively adding basis functions in a greedy fashion
#  so that each additional basis function further reduces the selected loss function'
#
if (!require('gbm')) install.packages('gbm')
library(gbm)
x <- data.train[,setdiff(colnames(data.train), 'popularity')]
y <- data.train[,'popularity']
# takes a long time to run
n.trees = 2000
gbm1 <- gbm.fit(x = x, y = y,
                distribution = 'multinomial',
                n.trees = n.trees,
                shrinkage = 0.1)
best.iter <- gbm.perf(gbm1,method="OOB")
print(best.iter)
preds <- apply(predict(gbm1, x, 2000), 1, which.max)
# 2000 trees vs best.iter trees (48) difference in success rate of 57.3 and 51.1
summary(preds)
success.rate(preds, y)

x.val <- data.validation[,setdiff(colnames(data.train), 'popularity')]
y.val <- data.validation[,'popularity']
# Predict using all trees
preds.val.1 <- apply(predict(gbm1, x.val, 2000), 1, which.max)
summary(preds.val.1)
success.rate(preds.val.1, y.val)
# Predict using best.iter trees
preds.val.2 <- apply(predict(gbm1, x.val, best.iter), 1, which.max)
summary(preds.val.2)
success.rate(preds.val.2, y.val)
# difference btw success rate here was 0.5089 for 2000 trees
# and 0.4992 for best.iter trees

# so far best with 1000 trees and shrinkage 0.1
#
# should use cv.folds for cross valiation to report the error
# this validation apparently defaults to using class stratification
# should optimize shrinkage, n.trees, possibly other options to this method (there are lots)
# should try to optimize shrinkage, done so for trees

# Now we use it to predict the test data...
#
data.test <- read.csv('news_popularity_test.csv')
# remove id and url
x.test <- data.test[,3:ncol(data.test)]
preds <- apply(predict(gbm1, x.test, n.trees), 1, which.max)
summary(preds)
predictions <- cbind(id=data.test[,'id'], popularity=preds)
head(predictions)
write.csv(predictions, '02022016-2-predictions.csv', row.names = FALSE)

pred.1 <- read.csv('02022016-1-predictions.csv')
# was actually submitted on the 3rd of Feb, whoops. Sorry guys.
# Also was not an improvement from the first iteration, may be overfitting.
#
pred.2 <- read.csv('02022016-2-predictions.csv')
sum(pred.1!=pred.2)
