if (!require('randomForest')) install.packages('randomForest')
setwd('~/Projects/kaggle-onlinenewspopularity/explorations/')
source('setup.R')
source('../utilities.R')

# setup
data.train <- data.frame(data.train)
data.validation <- data.frame(data.validation)
data.train[,'popularity'] <- factor(data.train[,'popularity'])
data.validation[,'popularity'] <- factor(data.validation[,'popularity'])

seed <- 738#round(runif(1)*1000)
set.seed(seed)
print(paste0('Running basic random forest classification with seed: ', seed))
base.model <- randomForest(popularity ~ ., data=data.train)
preds <- predict(base.model, data.validation[,1:59])
success.rate(preds, data.validation[,'popularity'])
# [1] "Number of errors: 2933"
# [1] "Rate: 0.511166666666667"
# [1] 0.5111667

set.seed(seed)
print(paste0('Running random forest with imporatnce, using seed: ', seed))
model.with.importance <- randomForest(popularity ~ ., data=data.train, importance = TRUE)
preds <- predict(model.with.importance, data.validation[,1:59])
success.rate(preds, data.validation[,'popularity'])
# [1] "Number of errors: 2909"
# [1] "Rate: 0.515166666666667"
# [1] 0.5151667

#### SUBSET CLASSIFIERS
#
# add a column for 1,2,3 / 4,5
# e.g. upper vs lower binary classification
data.train <- cbind(data.train,
                    upper=factor(ifelse(data.train[,'popularity'] %in% c('4','5'), 1, 0)))
data.validation <- cbind(data.validation,
                         upper=factor(ifelse(data.validation[,'popularity'] %in% c('4','5'), 1, 0)))

# train a classifier on the binary classification
set.seed(seed)
print(paste0('Running random forest on binary classes using seed: ', seed))
(bin.formula <- formula(paste0('upper ~ ', paste(colnames(data.train)[1:59], collapse = '+'))))
model.binary <- randomForest(bin.formula, data=data.train, importance = TRUE)

# predict binary classification of training set
preds.train <- model.binary$predicted
success.rate(preds.train, data.train[,'upper'])
data.train <- cbind(data.train, upper.predicted=ifelse(preds.train == 2, 1, 0))
# [1] "Number of errors: 820"
# [1] "Rate: 0.965833333333333"
# [1] 0.9658333

# predict binary classification of validation set
preds.val <- predict(model.binary, data.validation[,1:59])
success.rate(preds.val, data.validation[,'upper'])
data.validation <- cbind(data.validation, upper.predicted=ifelse(preds.val == 2, 1, 0))
# [1] "Number of errors: 226"
# [1] "Rate: 0.962333333333333"
# [1] 0.9623333

# doesn't actually seem to be predicting any in upper classes...

# train a classifier on those in lower class
train.lower <- subset(data.train, popularity %in% c('1','2','3'))
train.lower$popularity <- factor(train.lower$popularity)
#val.lower <- subset(data.validation, upper.predicted == 0)
val.lower <- subset(data.validation, popularity %in% c('1','2','3'))
val.lower$popularity <- factor(val.lower$popularity)

(lower.formula <- formula(paste0('popularity ~ ', paste(colnames(data.train)[1:59], collapse = '+'))))
model.lower <- randomForest(lower.formula, data=train.lower, importance = TRUE)
lower.preds.val <- predict(model.lower, val.lower[,1:59])
success.rate(lower.preds.val, val.lower$popularity)

# join data, see if bin predicts anything in upper class
data.full <- rbind(data.validation, data.train)
model.binary <- randomForest(bin.formula, data=data.full, importance = TRUE)
# > table(model.binary$predicted)

#     0     1 
# 29999     1
# LOLs

data.full.lower <- rbind(train.lower, val.lower)
set.seed(seed)
full.model.lower <- randomForest(lower.formula, data=data.full.lower, importance = TRUE)
success.rate(full.model.lower$predicted, data.full.lower$popularity)

full.model.lower <- randomForest(lower.formula, data=data.full.lower, importance = TRUE, ntree=100)
success.rate(full.model.lower$predicted, data.full.lower$popularity)

library(cphtbo)
res <- cross.val(model.function = randomForest, model.args = list(formula = lower.formula, data = data.full.lower, importance = TRUE, ntree = 1000), data.train = data.full.lower, no.subsets = 10)

