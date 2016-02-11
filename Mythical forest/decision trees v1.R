# DECISION TREES V1
# This file attempts to use decision trees to classify. It uses both the vanilla list of
# features that we were initially given and the engineered features that we made a few
# days back.
# It is an abject failure.

library(rpart)
library(party)
library(dplyr)
filepath <- '/home/beeb/Documents/Data_Science/News Competition/OnlineNewsPopularity'
setwd(filepath)
newspop <- read.csv('news_popularity_training.csv')

#Create binary from y variable
for(t in sort(unique(newspop[,'popularity']))) {
  newspop[paste("pop",t,sep="")] <- ifelse( newspop[,'popularity'] == t , 1 , 0 )
}

newspop <- newspop[,4:ncol(newspop)]
training.sample <- sample(nrow(newspop), 0.8*nrow(newspop))
newspop.train <- newspop[training.sample,]
newspop.test <- newspop[setdiff(1:nrow(newspop), training.sample),]

newspop.train$popularity <- as.factor(newspop.train$popularity)
newtree <- ctree(popularity ~ ., data = newspop.train[,1:59])
plot(newtree)
# I... I see.....

# HOW IS THIS SUPPOSED TO HELP?!?

# Meh
predictions <- Predict(newtree, newdata=newspop.test[,1:59])
#........................................
#............
#...................no but cmon srsly
# YYYYYYYYYYYYYYYYYYYYY
length(which(predictions==newspop.test$popularity))/nrow(newspop.test)

#predictions <- predict(newtree, newdata=newspop.test)
#length(which(predictions == newspop.test$popularity))/nrow(newspop.test)

# That hasn't been awfully successful - what if we try out with the engineered features
# we built earlier?
source('../../kaggle-onlinenewspopularity/Feature engineering/feature engineering v2-3.R')
newdata$popularity <- as.factor(newdata$popularity)
newdata.train <- newdata[training.sample,]
newdata.test <- newdata[setdiff(1:nrow(newdata), training.sample),]
tree.engineered <- ctree(popularity ~ ., data = newdata.train[,1:34])
predictions.engineered <- Predict(tree.engineered, newdata=newdata.test[,1:34])
#........................................
#............
#...................no but cmon srsly
# YYYYYYYYYYYYYYYYYYYYY
length(which(predictions.engineered==newdata.test$popularity))/nrow(newdata.test)