library(randomForest)
setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')

data.train$popularity <- factor(data.train$popularity)
data.validation$popularity <- factor(data.validation$popularity)

set.seed(415)

base.model <- randomForest(popularity ~ ., data=data.train)
success.rate(base.model$predicted, data.train$popularity)
# 0.5199583
# > base.model$ntree
# [1] 500
preds <- predict(base.model, data.validation[,1:59])
success.rate(preds, data.validation[,'popularity'])

# Add more trees, should not overfit?
model.1 <- randomForest(popularity ~ ., data=data.train, ntree = 1000)
preds <- predict(model.1, data.validation[,setdiff(colnames(data.validation), 'popularity')])
success.rate(preds, data.validation[,'popularity'])

if (!require(party)) install.packages('party')
str(data.train)
model.party <- cforest()