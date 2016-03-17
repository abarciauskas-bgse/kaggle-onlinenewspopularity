# K MEANS CLUSTERING
# IN THIS FILE
# We perform random forest and boosting on the newspop dataset using 3 clusters

library(rpart)
library(party)
library(dplyr)
filepath <- 'C:/Users/Sarah/Documents/Data science'
setwd(filepath)
newspop <- read.csv('news_popularity_training.csv')
iterations <- 5
clusters <- 4
#Create binary from y variable
for(t in sort(unique(newspop[,'popularity']))) {
  newspop[paste("pop",t,sep="")] <- ifelse( newspop[,'popularity'] == t , 1 , 0 )
}
success.rate <- rep(NA, iterations)
newspop <- newspop[,4:ncol(newspop)]

# We remove that goddamn observation that's screwing everything up
newspop <- newspop[setdiff(1:nrow(newspop), 22686),]

newspop[,1:11] <- apply(newspop[,1:11], 2, function(x) { return(x/sd(x))})
newspop[,18:29] <- apply(newspop[,18:29], 2, function(x) { return(x/sd(x))})
newspop[,38:58] <- apply(newspop[,38:58], 2, function(x) { return(x/sd(x))})

#newspop$popularity <- as.factor(newspop$popularity)


# Create training and testing sample
training.sample <- sample(nrow(newspop), 0.8*nrow(newspop))
newspop.train <- newspop[training.sample,]
newspop.test <- newspop[setdiff(1:nrow(newspop), training.sample),]

for(iter in 1:iterations) {
  cat(iter)
  # Create clusters and assign training and testing data to the clusters
  newspop.k <- kmeans(newspop.train[,1:58], centers = clusters)
  newspop.train$cluster <- as.factor(newspop.k$cluster)
  closest.cluster <- function(x) {
    cluster.dist <- apply(newspop.k$centers, 1, function(y) sqrt(sum((x-y)^2)))
    return(which.min(cluster.dist)[1])
  }
  newspop.test$clusters <- apply(newspop.test, 1, closest.cluster)
  
  # Train the models and test the performance
  errors <- rep(0, 5)
  errors.bycluster <- rep(NA, clusters)
  pred <- rep(NA, nrow(newspop.test))
  for(k in 1:clusters) {
    errors <- matrix(NA, ncol = 5, nrow = sum(newspop.test$cluster == k))
    for(i in 1:5) {
      current.cluster <- newspop.train[newspop.train$cluster == k, 1:58]    
      current.cluster$target <- newspop.train[newspop.train$cluster == k,(59 + i)]
      #model <- rpart(target ~ ., data = current.cluster)
      model <- gbm(target ~ ., data = current.cluster)
      assign(paste0('model', k, i), model)
      # This is the line that is causing the bug
      errors[,i] <- predict(model, n.trees = 100, newdata = newspop.test[newspop.test$cluster == k,])
    }
    predvals <- apply(errors, 1, which.max)
    pred[newspop.test$clusters == k] <- predvals
    errors.bycluster[k] <- sum(newspop.test$popularity[newspop.test$cluster == k] != predvals)
  }
  
  success.rate[iter] <-  1 - sum(errors.bycluster)/nrow(newspop.test)
}

success.rate

# Submitty submittalness
# Time to make a submission
test <- read.csv('news_popularity_test.csv')
test <- test[,4:ncol(test)]

test[,1:11] <- apply(test[,1:11], 2, function(x) { return(x/sd(x))})
test[,18:29] <- apply(test[,18:29], 2, function(x) { return(x/sd(x))})
test[,38:58] <- apply(test[,38:58], 2, function(x) { return(x/sd(x))})

newspop.k <- kmeans(newspop[,1:58], centers = clusters)
newspop$clusters <- newspop.k$cluster

test$clusters <- apply(test, 1, closest.cluster)
pred <- rep(NA, nrow(test))

for(k in 1:clusters) {
  errors <- matrix(NA, ncol = 5, nrow = sum(test$clusters == k))
  for(i in 1:5) {
    current.cluster <- newspop[newspop$cluster == k, 1:58]    
    current.cluster$target <- newspop[newspop$cluster == k,(59 + i)]
    model <- rpart(target ~ ., data = current.cluster)
    # This is the line that is causing the bug
    errors[,i] <- predict(model, newdata = test[test$clusters == k,])
  }
  predvals <- apply(errors, 1, which.max)
  pred[test$clusters == k] <- predvals
}
table(pred)

sample <- read.csv('news_popularity_sample.csv')


#final.predict <- predict(newtree, newdata = test)
sample$popularity <- pred
write.csv(sample, 'finalrpartkmeans.csv', row.names = FALSE)