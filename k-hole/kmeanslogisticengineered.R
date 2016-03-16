# K MEANS CLUSTERING
# IN THIS FILE
# We perform the same analysis as in kmeanslogistic.R but we use the engineered
# features rather than the vanilla features.
library(party)
library(dplyr)
source('/home/beeb/Documents/Data_Science/kaggle-onlinenewspopularity/Feature engineering/feature engineering v2-3.R')

# Create training and testing sample
training.sample <- sample(nrow(newdata), 0.8*nrow(newdata))
newdata.train <- newdata[training.sample, ]
newdata.test <- newdata[setdiff(1:nrow(newdata), training.sample),]

# Create clusters and assign training and testing data to the clusters
newdata.k <- kmeans(newdata.train, centers = 3)
newdata.train$cluster <- as.factor(newdata.k$cluster)
closest.cluster <- function(x) {
  cluster.dist <- apply(newdata.k$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}
newdata.test$clusters <- apply(newdata.test, 1, closest.cluster)

# Train the models and test the performance
errors <- rep(0, 5)
errors.bycluster <- rep(NA, 3)
for(k in 1:3) {
  errors <- matrix(NA, ncol = 5, nrow = sum(newdata.test$cluster == k))
  for(i in 1:5) {
    current.cluster <- newdata.train[newdata.train$cluster == k, 1:33]    
    current.cluster$target <- newdata.train[newdata.train$cluster == k,(34 + i)]
    model <- glm(target ~ ., data = current.cluster, family = binomial)
    errors[,i] <- predict(model, newdata = newdata.test[newdata.test$cluster == k,])
  }
  predvals <- apply(errors, 1, which.max)
  cat('')
  errors.bycluster[k] <- sum(newdata.test$popularity[newdata.test$cluster == k] != predvals)
}

success.rate <-  1 - sum(errors.bycluster)/nrow(newdata.test)
success.rate