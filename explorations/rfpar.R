if (!require('foreach')) install.packages('foreach')
if (!require('doSNOW')) install.packages('doSNOW')
if (!require('randomForest')) install.packages('randomForest')
if (!require('caret')) install.packages('caret')

setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

x <- data[,setdiff(colnames(data), 'popularity')]
y <- data[,'popularity']

# Setting number of cores in your machine. In this case, it is 2
registerDoSNOW(makeCluster(2, type="SOCK"))

mtry <- 2

# Main Random Forest Code. Run 250 trees on 2 cores parallely and then combine them
system.time(
  rf <- foreach(
    ntree = rep(250, 2),
    .combine = combine,
    .packages = "randomForest") %dopar%
    randomForest(popularity ~ ., 
                 data = data,
                 ntree = ntree,
                 mtry = mtry,
                 importance=TRUE))

system.time(rf.caret <- train(popularity ~ .,
                              data = data,
                              method="rf",
                              trControl = trainControl(method="cv",number=3),
                              prox = TRUE,
                              allowParallel = TRUE))
print(rf.caret)
