# RANDOM FOREST
# I HAVE WATCHED A 5 MINUTE VIDEO ABOUT THIS ON YOUTUBE I THEREFORE DECLARE MYSELF
# AN EXPERT. NOW BEHOLD, AS I RANDOMLY FOREST.
library(randomForest)
filepath <- '/home/beeb/Documents/Data_Science/News Competition/OnlineNewsPopularity'
setwd(filepath)
newspop <- read.csv('news_popularity_training.csv')

#Create binary from y variable
for(t in sort(unique(newspop[,'popularity']))) {
  newspop[paste("pop",t,sep="")] <- ifelse( newspop[,'popularity'] == t , 1 , 0 )
}

# Now do the same thing but with the engineered data
source('../../kaggle-onlinenewspopularity/Feature engineering/feature engineering v2-3.R')
newdata$popularity <- as.factor(newdata$popularity)
newspop <- newspop[,4:ncol(newspop)]
success.rate <- rep(NA, 10)
success.rate2 <- rep(NA, 10)
for(i in 1:10) {
training.sample <- sample(nrow(newspop), 0.8*nrow(newspop))
newspop.train <- newspop[training.sample,]
newspop.test <- newspop[setdiff(1:nrow(newspop), training.sample),]

newspop.train$popularity <- as.factor(newspop.train$popularity)

newtree <- randomForest(popularity ~ ., data = newspop.train[,1:59])

# it is so random and forestyyyyyyyyyyyyyy

# omg takes nearly as long as the knn algo

# foreeeeeeeeeeeeeest
#now what do I do?!
random.predictions <- predict(newtree, newdata = newspop.test)
success.rate[i] <- length(which(random.predictions==newspop.test$popularity))/nrow(newspop.test)

newdata.train <- newdata[training.sample,]
newdata.test <- newdata[setdiff(1:nrow(newdata), training.sample),]
tree.engineered <- randomForest(popularity ~ ., data = newdata.train[,1:34])
predictions.engineered <- predict(tree.engineered, newdata=newdata.test[,1:34])

success.rate2[i] <- length(which(predictions.engineered==newdata.test$popularity))/nrow(newdata.test)
}
mean(success.rate)
sd(success.rate)
mean(success.rate2)
sd(success.rate2)
# This shows that the tree built using the initial data is a better classifier

# Time to make a submission
test <- read.csv('news_popularity_test.csv')
sample <- read.csv('news_popularity_sample.csv')
final.predict <- predict(newtree, newdata = test)
sample$popularity <- final.predict
write.csv(sample, 'finalrforest.csv', row.names = FALSE)
