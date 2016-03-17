setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
# IMPORTANT: loads data and factorizes 'popularity'
source('setup.R')
install.packages('devtools')
devtools::install_github('zachmayer/caretEnsemble')

library("caretEnsemble")
library('rpart')

partition <- createDataPartition(data$popularity, times = 2, p = 0.10)
data.small <- data[partition$Resample1,]
data.test.small <- data[partition$Resample2,]

my_control <- trainControl(
  method="repeatedcv",
  number=3,
  # a series of test/training partitions creating one ore more bootstrap samples
  index=createResample(data.small$popularity, times = 3)
)

model_list <- caretList(
  popularity ~ ., data = data.small,
  trControl = my_control,
  methodList = c("avNNet", "rpart", 'rf')
)

# ideally we want the predictions to be as uncorrelated as possible
p <- as.data.frame(predict(model_list, newdata=data.test.small))
table(p[,'rpart'])
table(p[,'avNNet'])
table(p[,'rf'])

# I don't really get this but it appears the models are too highly correlated
xyplot(resamples(model_list))
# i don't really get the point of this, seem uncorrelated but??
modelCor(resamples(model_list))

# make predictions using test and return accuracy
data.train.x <- data.small[,setdiff(colnames(data.small), 'popularity')]
data.train.y <- data.small[,'popularity']

# http://www.r-bloggers.com/caretensemble-classification-example/
model1 <- train(data.train.x, data.train.y, method='gbm', trControl=my_control, distribution = 'multinomial')
# blackboost crashed my rstudio. Tried with additional parameter (family = 'multinomial') and got errors about ngradient
#model2 <- train(data.train.x, data.train.y, method='blackboost', trControl=my_control, family = 'multinomial')

# parallel random forest
model2 <- train(data.train.x, data.train.y, method='parRF', trControl=my_control)

# not sure why this is only started now
PP <- c('center', 'scale')
# is slow
model3 <- train(data.train.x, data.train.y, method='mlpWeightDecay', trControl=my_control, trace=FALSE, preProcess=PP)

# is fast
model4 <- train(data.train.x, data.train.y, method='knn', trControl=my_control, preProcess=PP)
# but really poor, could try more neighbors

model5 <- train(data.train.x, data.train.y, method='earth', trControl=my_control, preProcess=PP)

model6 <- train(data.train.x, data.train.y, method='svmRadial', trControl=my_control, preProcess=PP)

model7 <- train(data.train.x, data.train.y, method='glmnet', trControl=my_control, preProcess=PP, family = 'multinomial')

model8 <- train(data.train.x, data.train.y, method='multinom', trControl=my_control, preProcess=PP)


all.models <- list(model1, model2, model3, model4, model5, model6, model7, model8)
names(all.models) <- sapply(all.models, function(x) x$method)
sort(sapply(all.models, function(x) max(x$results$Accuracy)))

# determine which predictions are least correlated
data.test.x <-data.test.small[,setdiff(colnames(data.test.small), 'popularity')]
data.test.y <-data.test.small[,'popularity']
preds <- lapply(all.models, function(mod) {
  return(predict(mod, data.test.x))
})

# matrix of predictions
all.preds <- do.call(cbind, preds)
head(cor(all.preds))

# is this the way to compare or with cor?
sum(preds$gbm == preds$multinom)/length(preds$gbm)

accuracies <- lapply(preds, function(pred) {
  return(sum(pred == data.test.y)/length(pred))
})

# $gbm
# [1] 0.4993338
# 
# $parRF
# [1] 0.5656229
# 
# $mlpWeightDecay
# [1] 0.4896736
# 
# $knn
# [1] 0.4713524
# 
# $earth
# [1] 0.5016656
# 
# $svmRadial
# [1] 0.5083278
# 
# $glmnet
# [1] 0.4956696
# 
# $multinom
# [1] 0.488008

# they all return near 50% so may just take the mode
ensemble.preds <- apply(all.preds, 1, my.mode)

success.rate(ensemble.preds, data.test.y)

# parRF has the highest out of sample accuracy,
# it would appear so going to start with that and find the model with which it is the least correlated
parRf.corrs <- list()
for (i in 1:length(preds)) {
  other.model.name <- names(preds[i])
  other.model.preds <- preds[[i]]
  parRf.corrs[other.model.name] = sum(preds$parRF == other.model.preds)/length(preds$parRF)
}

# knn is least correlated with 0.65 similarity in predictions

# so find the one least correlated with knn?
knn.corrs <- list()
for (i in 1:length(preds)) {
  other.model.name <- names(preds[i])
  other.model.preds <- preds[[i]]
  knn.corrs[other.model.name] = sum(preds$knn == other.model.preds)/length(preds$knn)
}
knn.corrs

# Try an ensemble with gbm, knn and parRF
mini.ensemble.preds <- apply(all.preds[,c('gbm','knn','parRF')], 1, my.mode)

success.rate(mini.ensemble.preds, data.test.y)
# slightly whole ensemble -> 0.5135

# try to train with caret ensemble
model_list <- caretList(
  popularity ~ ., data=data.small,
  trControl = my_control,
  methodList = c('knn','parRF','rf')
)
model_preds <- lapply(model_list, predict, newdata=data.test.small, type="raw")
model_preds.mat <- matrix(as.numeric(unlist(model_preds)), ncol = 3, byrow = TRUE)
ensemble.preds.new <- apply(model_preds.mat, 1, my.mode)
head(ensemble.preds.new)
success.rate(ensemble.preds.new, data.test.small[,'popularity'])

# Make a list of potential models
init.models <- c('knn','parRF','gbm')
potential.models <- setdiff(c(
  'gbm',
  'multinom',
  'knn',
  'parRF',
  'rpart',
  'avNNet',
  'rf',
  'mlpWeightDecay',
  'svmRadial'), init.models)

# make an initial ensemble
model_list <- caretList(
  popularity ~ ., data=data.small,
  trControl = my_control,
  methodList = init.models
)
# and an initial test accuracy
model_preds <- lapply(model_list, predict, newdata=data.test.small, type="raw")
model_preds.mat <- matrix(as.numeric(unlist(model_preds)), ncol = 3, byrow = TRUE)
ensemble.preds.new <- apply(model_preds.mat, 1, my.mode)
success.rate(ensemble.preds.new, data.test.small[,'popularity'])

# for the other models, cycle and add if they improve accuracy
for (model.idx in 1:length(potential.models)) {
  model_list <- caretList(
    popularity ~ ., data=data.small,
    trControl = my_control,
    methodList = append(init.models, potential.models[model.idx])
  )
  
  # add new model predictions to ensemble.preds.new and take mode
  
}

# this is going nowhere so just trying an ensemble with multinom, parRF and knn on more data

fiftypct.rand.idcs <- sample(1:nrow(data), nrow(data)/2)
train.data <- data[fiftypct.rand.idcs,]
test.data <- data[-fiftypct.rand.idcs,]
train.x <- train.data[,setdiff(colnames(train.data), 'popularity')]
train.y <- train.data[,'popularity']
test.x <- test.data[,setdiff(colnames(test.data), 'popularity')]
test.y <- test.data[,'popularity']

# train multinom, parRF and gbm
multinom.model <- multinom(popularity ~ ., train.data)
rf.model <- randomForest(train.x, train.y, mtry = 2)
#   n.trees interaction.depth shrinkage n.minobsinnode
# 1      50                 1       0.1             10
gbm.model <- gbm(formula = popularity ~ .,
                 data = train.data,
                 distribution = 'multinomial',
                 n.trees = 50,
                 interaction.depth = 1,
                 shrinkage = 0.1,
                 n.minobsinnode = 10)
# relics of attempts past
# library(class)
# knn.model <- knn(train.x, test.x, train.y, k = 9)

ensemble.preds <- lapply(list(multinom.model, rf.model), function(mod) {
  return(predict(mod, test.x))
})
ensemble.preds <- do.call(cbind, ensemble.preds)
gbm.preds <- apply(predict(gbm.model, test.x, n.trees = 50), 1, which.max)
ensemble.preds <- cbind(ensemble.preds, gbm.preds)
colnames(ensemble.preds) <- c('multinom','rf','gbm')
ensemble.preds.gathered <- apply(ensemble.preds, 1, my.mode)
success.rate(ensemble.preds.gathered, test.y)
# [1] "Number of errors: 7377"
# [1] "Rate: 0.5148
# [1] 0.5148

# swap multinom for avNNet
# TODO: tune parameters for avNNet
avNNet.model <- avNNet(popularity ~ ., data = train.data)
ensemble.preds <- lapply(list(avNNet.model, rf.model), function(mod) {
  return(predict(mod, test.x))
})
ensemble.preds <- do.call(cbind, ensemble.preds)
gbm.preds <- apply(predict(gbm.model, test.x, n.trees = 50), 1, which.max)
ensemble.preds <- cbind(ensemble.preds, gbm.preds)
colnames(ensemble.preds) <- c('avNNet','rf','gbm')
ensemble.preds.gathered <- apply(ensemble.preds, 1, my.mode)
success.rate(ensemble.preds.gathered, test.y)


# FOR EVERYTHING
train.data <- data
test.data <- read.csv('../data/news_popularity_test.csv')
train.x <- train.data[,setdiff(colnames(train.data), 'popularity')]
train.y <- train.data[,'popularity']
test.x <- test.data[,setdiff(colnames(test.data), c('id', 'url','popularity'))]

multinom.model <- multinom(popularity ~ ., train.data)
rf.model <- randomForest(train.x, train.y, mtry = 2)
knn.model <- knn(train.x, test.x, train.y, k = 9)

ensemble.preds <- lapply(list(multinom.model, rf.model), function(mod) {
  return(predict(mod, test.x))
})
ensemble.preds <- do.call(cbind, ensemble.preds)
ensemble.preds <- cbind(ensemble.preds, knn.model)
colnames(ensemble.preds) <- c('multinom','rf','knn9')
ensemble.preds.gathered <- apply(ensemble.preds, 1, my.mode)

predictions <- cbind(id=test.data[,'id'], popularity=ensemble.preds.gathered)
write.csv(predictions, '../submissions/14032016-predictions.csv', row.names = FALSE)
