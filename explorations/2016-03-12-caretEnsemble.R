setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
# IMPORTANT: loads data and factorizes 'popularity'
source('setup.R')
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

# just train parRF since it is doing awesome
library(foreach)

rfParam <- expand.grid(ntree=100, importance=TRUE, mtry = 2)
x <- data[,setdiff(colnames(data), 'popularity')]
y <- data[,'popularity']

# took forever
m <- train(x, y, method="parRF")

library("foreach")
if (!require('doSNOW')) install.packages('doSNOW')
library(randomForest)

# Setting number of cores in your machine. In this case, it is 2
registerDoSNOW(makeCluster(2, type="SOCK"))

# Optimal mtry
# as found by caret's train
mtry <- 30
print(mtry)

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
                 importance=TRUE)
)

# train many models on a small number of training data
# determine which return the most uncorrelated fits to the features
# cross-validate differen uncorrelated combinations

# TODO: more tuning parameters, random tuning parameters

